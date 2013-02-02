let word_size = float_of_int Sys.word_size /. 8.
let get_time = Unix.gettimeofday
let get_words () = float_of_int Gc.(minor (); (quick_stat ()).heap_words)
let get_top_words () = float_of_int Gc.(minor (); (quick_stat ()).top_heap_words)

let measure f =
    let start_words = get_words () in
    let start_time = get_time () in
    let x = f () in
    let end_time = get_time () -. start_time in
    let end_words = get_words () -. start_words in
    ( x, end_time, end_words )

let list_filter_task (type a) (module L : Adapton.Signatures.SAListType.S with type t = a and type data = float) =
    fst (L.memo_filter (fun x -> x < 0.5))

let list_map_task (type a) (module L : Adapton.Signatures.SAListType.S with type t = a and type data = float) =
    fst (L.memo_map (module L) (fun x -> x *. 3. +. x *. 7. +. x *. 9.))

let list_tfold_task (type a) (module L : Adapton.Signatures.SAListType.S with type t = a and type data = float) =
    let tfold = fst (L.memo_tfold (+.)) in
    (fun xs -> L.const (`Cons ( L.SAData.force (tfold xs), L.const `Nil )))

let list_quicksort_task (type a) (module L : Adapton.Signatures.SAListType.S with type t = a and type data = float) =
    fst (L.memo_quicksort Pervasives.compare)

let list_mergesort_task (type a) (module L : Adapton.Signatures.SAListType.S with type t = a and type data = float) =
    fst (L.memo_mergesort Pervasives.compare)

let tasks = [
    ( "filter", list_filter_task );
    ( "map", list_map_task );
    ( "tfold", list_tfold_task );
    ( "quicksort", list_quicksort_task );
    ( "mergesort", list_mergesort_task );
]

let opt_salist = ref (fst (List.hd Adapton.All.salist_list))
let opt_task = ref "filter"
let opt_edit_count = ref 1
let opt_input_size = ref 1
let opt_take_count = ref 1
let opt_random_seed = ref 1

let show_config () =
    let list_printer ff list =
        ignore (List.fold_left (fun b x -> Printf.fprintf ff "%(%)%S" b x; ", ") "" list)
    in
    Printf.printf "{ \"modules\": [ %a ], \"tasks\": [ %a ] }\n%!"
        list_printer (fst (List.split Adapton.All.salist_list))
        list_printer (fst (List.split tasks));
    exit 0

let _ =
    Arg.parse (Arg.align [
        ( "-c", Arg.Unit show_config, " output available configuration" );
        ( "-m", Arg.Symbol ( (fst (List.split Adapton.All.salist_list)), (fun s -> opt_salist := s) ), "list module" );
        ( "-t", Arg.Symbol ( (fst (List.split tasks)), (fun s -> opt_task := s) ), "list task" );
        ( "-I", Arg.Set_int opt_input_size, "size input size" );
        ( "-T", Arg.Set_int opt_take_count, "count take count" );
        ( "-E", Arg.Set_int opt_edit_count, "count edit count" );
        ( "-S", Arg.Set_int opt_random_seed, "seed random seed" );
    ]) (fun s -> raise (Arg.Bad ("extraneous argument " ^ s))) (Sys.argv.(0) ^ " [options]");

    let rng = Random.State.make [| !opt_random_seed |] in
    Random.init (Random.State.bits rng);
    let module SAList = (val (List.assoc !opt_salist Adapton.All.salist_list)) in
    let module SAFloatList = SAList.Make (Adapton.Types.Float) in
    let task = (List.assoc !opt_task tasks) (module SAFloatList) in

    let start_time = get_time () in

    let xs = ref (SAFloatList.const `Nil) in
    let xss = Array.init !opt_input_size begin fun _ ->
        xs := SAFloatList.const (`Cons (Random.State.float rng 1.0, !xs));
        !xs
    end in
    let xs = !xs in

    Printf.eprintf "%32s %24s %4d %10d %20d\n%!" !opt_salist !opt_task !opt_take_count !opt_input_size !opt_random_seed;
    try
        let ys, setup_time, setup_words = measure begin fun () ->
            let ys = task xs in
            ignore (SAFloatList.take ys !opt_take_count);
            ys
        end in
        let setup_top_words = get_top_words () in

        try
            let rec do_edits past n update_time update_words take_time take_words =
                if n == 0 then
                    ( update_time, update_words, take_time, take_words )
                else begin
                    let past =
                        let now = get_time () in
                        if now -. past < 20. then
                            past
                        else begin
                            Printf.eprintf "%32s %24s %4d %10d %20d edit %10d %9.2fMB\n%!"
                                !opt_salist !opt_task !opt_take_count !opt_input_size !opt_random_seed n (word_size *. get_top_words () /. 1024. /. 1024.);
                            now
                        end
                    in
                    let edit = Random.State.int rng !opt_input_size in
                    let zs = xss.(edit) in

                    let ( z', zs' ), delete_update_time, delete_update_words = measure begin fun () ->
                        match SAFloatList.force zs with
                            | `Cons ( z', zs' ) ->
                                SAFloatList.update_const zs (SAFloatList.force zs');
                                ( z', zs' )
                            | `Nil ->
                                failwith "delete"
                    end in

                    let _, delete_take_time, delete_take_words = measure begin fun () ->
                        SAFloatList.refresh ();
                        ignore (SAFloatList.take ys !opt_take_count)
                    end in

                    let (), insert_update_time, insert_update_words = measure begin fun () ->
                        SAFloatList.update_const zs (`Cons ( z', zs' ))
                    end in
                    let _, insert_take_time, insert_take_words = measure begin fun () ->
                        SAFloatList.refresh ();
                        ignore (SAFloatList.take ys !opt_take_count)
                    end in

                    do_edits past (pred n)
                        (update_time +. delete_update_time +. insert_update_time) (update_words +. delete_update_words +. insert_update_words)
                        (take_time +. delete_take_time +. insert_take_time) (take_words +. delete_take_words +. insert_take_words)
                end
            in
            let update_time, update_words, take_time, take_words = do_edits 0. !opt_edit_count 0. 0. 0. 0. in
            let edit_top_words = get_top_words () in
            let update_time = update_time /. float_of_int !opt_edit_count /. 2. in
            let update_words = update_words /. float_of_int !opt_edit_count /. 2. in
            let take_time = take_time /. float_of_int !opt_edit_count /. 2. in
            let take_words = take_words /. float_of_int !opt_edit_count /. 2. in
            Printf.printf
                ("{ \"setup\": { \"time\": %.17g, \"heap\": %.17g, \"max-heap\": %.17g }, "
                    ^^ "\"edits\": { \"update-time\": %.17g, \"update-heap\": %.17g, \"take-time\": %.17g, \"take-heap\": %.17g, \"max-heap\": %.17g },"
                    ^^ "\"units\": { \"time\": \"seconds\", \"heap\": \"bytes\", \"max-heap\": \"bytes\" } }\n%!")
                setup_time (word_size *. setup_words) (word_size *. setup_top_words)
                update_time (word_size *. update_words)
                take_time (word_size *. take_words)
                (word_size *. edit_top_words);
            Printf.eprintf "%32s %24s %4d %10d %20d ... done (%9.2fs) %9.3gs edit %9.3gs\n%!"
                !opt_salist !opt_task !opt_take_count !opt_input_size !opt_random_seed (get_time () -. start_time) setup_time (update_time +. take_time)
        with Adapton.Exceptions.NonSelfAdjustingValue ->
            Printf.printf
                ("{ \"setup\": { \"time\": %.17g, \"heap\": %.17g, \"max-heap\": %.17g }, "
                    ^^ "\"units\": { \"time\": \"seconds\", \"heap\": \"bytes\", \"max-heap\": \"bytes\" } }\n%!")
                setup_time (word_size *. setup_words) (word_size *. setup_top_words);
            Printf.eprintf "%32s %24s %4d %10d %20d ... done (%9.2fs) %9.3gs\n%!"
                !opt_salist !opt_task !opt_take_count !opt_input_size !opt_random_seed (get_time () -. start_time) setup_time

    with e ->
        let err = Printexc.to_string e in
        Printf.printf ("{ \"error\": %S }\n%!") err;
        Printf.eprintf "%s\n%!" err;
        Printf.eprintf "%32s %24s %4d %10d %20d ... done (%9.2fs)\n%!"
            !opt_salist !opt_task !opt_take_count !opt_input_size !opt_random_seed (get_time () -. start_time)
