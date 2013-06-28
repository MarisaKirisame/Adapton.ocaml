let word_size = float_of_int Sys.word_size /. 8.
let get_time = Unix.gettimeofday
let stack = ref 0.
let reset_stack () = stack := 0.
let get_stack () = !stack
let top_stack = ref 0.
let get_top_stack () = !top_stack
let _ = Gc.create_alarm (fun () -> let s = float_of_int Gc.((quick_stat ()).stack_size) in if !stack < s then (stack := s; top_stack := max !top_stack s))
let get_heap () = float_of_int Gc.(minor (); (quick_stat ()).heap_words)
let get_top_heap () = float_of_int Gc.(minor (); (quick_stat ()).top_heap_words)

let measure f =
    reset_stack ();
    let start_heap = get_heap () in
    let start_time = get_time () in
    let x = f () in
    let end_time = get_time () -. start_time in
    let end_heap = get_heap () -. start_heap in
    let end_stack = get_stack () in
    ( x, end_time, end_heap, end_stack )

let list_filter_task (type a) (module L : Adapton.Signatures.SAListType.S with type t = a and type data = float) =
    L.memo_filter (fun x -> log (1. +. x) < log 1.5)

let list_map_task (type a) (module L : Adapton.Signatures.SAListType.S with type t = a and type data = float) =
    L.memo_map (module L) (fun x -> log (1. +. x) +. log 1.5)

let list_sum_task (type a) (type b) (module L : Adapton.Signatures.SAListType.S with type t = a and type SAData.t = b and type data = float) =
    L.memo_tfold (+.)

let list_quicksort_task (type a) (module L : Adapton.Signatures.SAListType.S with type t = a and type data = float) =
    L.memo_quicksort Pervasives.compare

let list_mergesort_task (type a) (module L : Adapton.Signatures.SAListType.S with type t = a and type data = float) =
    L.memo_mergesort Pervasives.compare

let tasks = [
    ( "filter", `List list_filter_task );
    ( "map", `List list_map_task );
    ( "sum", `One list_sum_task );
    ( "quicksort", `List list_quicksort_task );
    ( "mergesort", `List list_mergesort_task );
]

let opt_sa = ref (fst (List.hd Adapton.All.sa_list))
let opt_task = ref "filter"
let opt_edit_count = ref 1
let opt_input_size = ref 1
let opt_take_count = ref 1
let opt_random_seed = ref 1
let opt_monotonic = ref false

let header ff = Printf.fprintf ff "%32s %24s %8d %8d %20d" !opt_sa !opt_task !opt_take_count !opt_input_size !opt_random_seed

let show_config () =
    let list_printer printer ff list =
        ignore (List.fold_left (fun b x -> Printf.fprintf ff "%(%)%a" b printer x; ", ") "" list)
    in
    let task_printer ff task =
        Printf.fprintf ff "{ \"name\": %S, \"take\": %S }" (fst task) (match snd task with `One _ -> "one" | `List _ -> "list")
    in
    Printf.printf "{ \"modules\": [ %a ], \"tasks\": [ %a ] }\n%!"
        (list_printer (fun ff -> Printf.fprintf ff "%S")) (fst (List.split Adapton.All.sa_list))
        (list_printer task_printer) tasks;
    exit 0

let _ =
    Arg.parse (Arg.align [
        ( "-c", Arg.Unit show_config, " output available configuration" );
        ( "-m", Arg.Symbol ( (fst (List.split Adapton.All.sa_list)), (fun s -> opt_sa := s) ), "list module" );
        ( "-t", Arg.Symbol ( (fst (List.split tasks)), (fun s -> opt_task := s) ), "list task" );
        ( "-I", Arg.Set_int opt_input_size, "size input size" );
        ( "-T", Arg.Set_int opt_take_count, "count take count" );
        ( "-E", Arg.Set_int opt_edit_count, "count edit count" );
        ( "-S", Arg.Set_int opt_random_seed, "seed random seed" );
        ( "-M", Arg.Set opt_monotonic, "monotonic edits" );
    ]) (fun s -> raise (Arg.Bad ("extraneous argument " ^ s))) (Sys.argv.(0) ^ " [options]");

    let rng = Random.State.make [| !opt_random_seed |] in
    Random.init (Random.State.bits rng);
    let module SA = (val (List.assoc !opt_sa Adapton.All.sa_list)) in
    let module SAList = Adapton.SAList.Make (SA) in
    let module SAFloatList = SAList.Make (Adapton.Types.Float) in
    SA.tweak_gc ();
    Gc.compact ();
    let task = match List.assoc !opt_task tasks with
        | `One task ->
            if !opt_take_count != 1 then begin
                Printf.eprintf "Task %s only supports -T 1\n%!" !opt_task;
                exit 1
            end;
            `One (task (module SAFloatList))
        | `List task ->
            `List (task (module SAFloatList))
    in

    let start_time = get_time () in

    let xs = ref (SAFloatList.const `Nil) in
    let xss = Array.init !opt_input_size begin fun _ ->
        xs := SAFloatList.const (`Cons (Random.State.float rng 1.0, !xs));
        !xs
    end in
    let xs = !xs in
    let last = ref 0 in

    Printf.eprintf "%t\n%!" header;
    try
        let take, setup_time, setup_heap, setup_stack = measure begin fun () ->
            let take = match task with
                | `List task ->
                    let ys = task xs in
                    (fun () -> ignore (SAFloatList.take ys !opt_take_count))
                | `One task ->
                    let y = task xs in
                    (fun () -> ignore (SAFloatList.SAData.force y))
            in
            take ();
            take
        end in
        let setup_top_heap = get_top_heap () in
        let setup_top_stack = get_top_stack () in

        if SAList.is_self_adjusting then begin
            let rec do_edits past n update_time update_heap update_stack take_time take_heap take_stack =
                if n == 0 then
                    ( update_time, update_heap, update_stack, take_time, take_heap, take_stack )
                else begin
                    let past =
                        let now = get_time () in
                        if now -. past < 20. then
                            past
                        else begin
                            Printf.eprintf "%t edit %10d %9.2fMB %9.2fMB\n%!"
                                header n
                                (word_size *. get_top_heap () /. 1024. /. 1024.)
                                (word_size *. get_top_stack () /. 1024. /. 1024.);
                            now
                        end
                    in

                    let update_time', update_heap', update_stack', take_time', take_heap', take_stack' = if !opt_monotonic then
                        (* delete then re-insert *)
                        let edit = Random.State.int rng !opt_input_size in
                        let zs = xss.(edit) in

                        let ( z', zs' ), delete_update_time, delete_update_heap, delete_update_stack = measure begin fun () ->
                            match SAFloatList.force zs with
                                | `Cons ( z', zs' ) ->
                                    SAFloatList.update_const zs (SAFloatList.force zs');
                                    ( z', zs' )
                                | `Nil ->
                                    failwith "delete"
                        end in

                        let (), delete_take_time, delete_take_heap, delete_take_stack = measure begin fun () ->
                            SAFloatList.refresh ();
                            take ()
                        end in

                        let (), insert_update_time, insert_update_heap, insert_update_stack = measure begin fun () ->
                            SAFloatList.update_const zs (`Cons ( z', zs' ))
                        end in

                        let (), insert_take_time, insert_take_heap, insert_take_stack = measure begin fun () ->
                            SAFloatList.refresh ();
                            take ()
                        end in

                        ( (delete_update_time +. insert_update_time) /. 2.,
                            (delete_update_heap +. insert_update_heap) /. 2.,
                            (delete_update_stack +. insert_update_stack) /. 2.,
                            (delete_take_time +. insert_take_time) /. 2.,
                            (delete_take_heap +. insert_take_heap) /. 2.,
                            (delete_take_stack +. insert_take_stack) /. 2. )
                    else
                        (* split into two and swap *)
                        let edit = 1 + Random.State.int rng (!opt_input_size - 2) in
                        let edit = if edit = !last then edit + 1 else edit in
                        let zs = xss.(edit) in

                        let (), update_time, update_heap, update_stack = measure begin fun () ->
                            match SAFloatList.force xs with
                                | `Cons _ as xs' ->
                                    begin match SAFloatList.force xss.(!last) with
                                        | `Cons _ as last' ->
                                            begin match SAFloatList.force zs with
                                                | `Cons _ as zs' ->
                                                    SAFloatList.update_const xs zs';
                                                    SAFloatList.update_const xss.(!last) xs';
                                                    SAFloatList.update_const zs last';
                                                    last := edit;
                                                | `Nil ->
                                                    failwith "swap"
                                            end
                                        | `Nil ->
                                            failwith "swap"
                                    end
                                | `Nil ->
                                    failwith "swap"
                        end in

                        let (), take_time, take_heap, take_stack = measure begin fun () ->
                            SAFloatList.refresh ();
                            take ()
                        end in

                        ( update_time, update_heap, update_stack, take_time, take_heap, take_stack )
                    in

                    do_edits past (pred n)
                        (update_time +. update_time') (update_heap +. update_heap') (update_stack +. update_stack')
                        (take_time +. take_time') (take_heap +. take_heap') (take_stack +. take_stack')
                end
            in
            let update_time, update_heap, update_stack, take_time, take_heap, take_stack = do_edits 0. !opt_edit_count 0. 0. 0. 0. 0. 0. in
            let edit_top_heap = get_top_heap () in
            let edit_top_stack = get_top_stack () in
            let update_time = update_time /. float_of_int !opt_edit_count in
            let update_heap = update_heap /. float_of_int !opt_edit_count in
            let update_stack = update_stack /. float_of_int !opt_edit_count in
            let take_time = take_time /. float_of_int !opt_edit_count in
            let take_heap = take_heap /. float_of_int !opt_edit_count in
            let take_stack = take_stack /. float_of_int !opt_edit_count in
            Printf.printf
                ("{ \"setup\": { \"time\": %.17g, \"heap\": %.17g, \"stack\": %.17g, \"max-heap\": %.17g, \"max-stack\": %.17g }, "
                    ^^ "\"edits\": { \"update-time\": %.17g, \"update-heap\": %.17g, \"update-stack\": %.17g, "
                        ^^ "\"take-time\": %.17g, \"take-heap\": %.17g, \"take-stack\": %.17g, \"max-heap\": %.17g, \"max-stack\": %.17g },"
                    ^^ "\"units\": { \"time\": \"seconds\", \"heap\": \"bytes\", \"stack\": \"bytes\", \"max-heap\": \"bytes\", \"max-stack\": \"bytes\" } }\n%!")
                setup_time (word_size *. setup_heap) (word_size *. setup_stack) (word_size *. setup_top_heap) (word_size *. setup_top_stack)
                update_time (word_size *. update_heap) (word_size *. update_stack)
                take_time (word_size *. take_heap) (word_size *. take_stack)
                (word_size *. edit_top_heap) (word_size *. edit_top_stack);
            Printf.eprintf "%t ... done (%9.2fs) %9.3gs edit %9.3gs\n%!" header (get_time () -. start_time) setup_time (update_time +. take_time)
        end else begin
            Printf.printf
                ("{ \"setup\": { \"time\": %.17g, \"heap\": %.17g, \"stack\": %.17g, \"max-heap\": %.17g, \"max-stack\": %.17g }, "
                    ^^ "\"units\": { \"time\": \"seconds\", \"heap\": \"bytes\", \"stack\": \"bytes\", \"max-heap\": \"bytes\", \"max-stack\": \"bytes\" } }\n%!")
                setup_time (word_size *. setup_heap) (word_size *. setup_stack) (word_size *. setup_top_heap) (word_size *. setup_top_stack);
            Printf.eprintf "%t ... done (%9.2fs) %9.3gs\n%!" header (get_time () -. start_time) setup_time
        end

    with e ->
        let err = Printexc.to_string e in
        Printf.printf ("{ \"error\": %S }\n%!") err;
        Printf.eprintf "%s\n%!" err;
        Printf.eprintf "%t ... done (%9.2fs)\n%!" header (get_time () -. start_time)
