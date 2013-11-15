open Adapton.Statistics

let list_filter_task (type a) (module L : Adapton.Signatures.SAListType.S with type t = a and type data = float) =
    L.memo_filter (fun x -> log (1. +. x) < log 1.5)

let list_map_task (type a) (module L : Adapton.Signatures.SAListType.S with type t = a and type data = float) =
    L.memo_map (module L) (fun x -> log (1. +. x) +. log 1.5)

let list_tfold_min_task (type a) (type b) (module L : Adapton.Signatures.SAListType.S with type t = a and type SAData.t = b and type data = float) =
    L.memo_tfold min

let list_tfold_sum_task (type a) (type b) (module L : Adapton.Signatures.SAListType.S with type t = a and type SAData.t = b and type data = float) =
    L.memo_tfold (+.)

let list_quicksort_task (type a) (module L : Adapton.Signatures.SAListType.S with type t = a and type data = float) =
    L.memo_quicksort Pervasives.compare

let list_mergesort_task (type a) (module L : Adapton.Signatures.SAListType.S with type t = a and type data = float) =
    L.memo_mergesort Pervasives.compare

let list_updown1_task
        (type a) (module L : Adapton.Signatures.SAListType.S with type t = a and type data = float)
        (type b) (module B : Adapton.Signatures.SAType.S with type t = b and type data = bool)
        xs b =
    let up = L.memo_quicksort Pervasives.compare in
    let down = L.memo_quicksort (fun x y -> -(Pervasives.compare x y)) in
    L.thunk (fun () -> L.force (if B.force b then up xs else down xs))

let list_updown2_task
        (type a) (module L : Adapton.Signatures.SAListType.S with type t = a and type data = float)
        (type b) (module B : Adapton.Signatures.SAType.S with type t = b and type data = bool)
        xs b =
    let up = L.memo_quicksort Pervasives.compare xs in
    let down = L.memo_quicksort (fun x y -> -(Pervasives.compare x y)) xs in
    L.thunk (fun () -> L.force (if B.force b then up else down))

let tasks = [
    ( "filter", `List list_filter_task );
    ( "map", `List list_map_task );
    ( "tfold(min)", `One list_tfold_min_task );
    ( "tfold(sum)", `One list_tfold_sum_task );
    ( "quicksort", `List list_quicksort_task );
    ( "mergesort", `List list_mergesort_task );
    ( "updown1", `Flip list_updown1_task );
    ( "updown2", `Flip list_updown2_task );
    ( "exptree", `ExpTree );
]

let opt_sa = ref (fst (List.hd Adapton.All.sa_list))
let opt_task = ref "filter"
let opt_input_size = ref 1
let opt_repeat_count = ref 1
let opt_edit_count = ref 1
let opt_take_count = ref 1
let opt_random_seed = ref 1
let opt_monotonic = ref false

let header ff = Printf.fprintf ff "%24s %24s %8d %8d %20d" !opt_sa !opt_task !opt_take_count !opt_input_size !opt_random_seed
let stats ff s =
    Printf.fprintf ff "\"time\": %.17g, \"heap\": %.17g, \"stack\": %.17g, \"update\": %.17g, \"evaluate\": %.17g, \"dirty\": %.17g, \"clean\": %.17g"
        s.time s.heap s.stack s.update s.evaluate s.dirty s.clean
let max_heap_stack ff ( heap, stack ) = Printf.fprintf ff "\"max-heap\": %.17g, \"max-stack\": %.17g" (word_bytes heap) (word_bytes stack)
let units =
    "\"units\": { \"time\": \"seconds\", \"heap\": \"bytes\", \"stack\": \"bytes\", "
    ^ "\"update\": null, \"evaluate\": null, \"dirty\": null, \"clean\": null, "
    ^ "\"max-heap\": \"bytes\", \"max-stack\": \"bytes\" }"

let show_config () =
    let list_printer printer ff list =
        ignore (List.fold_left (fun b x -> Printf.fprintf ff "%(%)%a" b printer x; ", ") "" list)
    in
    let task_printer ff task =
        Printf.fprintf ff "{ \"name\": %S, \"take\": %S }"
            (fst task) (match snd task with `One _ -> "one" | `List _ -> "list" | `Flip _ -> "flip" | `ExpTree -> "exptree")
    in
    Printf.printf "{ \"modules\": [ %a ], \"tasks\": [ %a ] }\n%!"
        (list_printer (fun ff -> Printf.fprintf ff "%S")) (fst (List.split Adapton.All.sa_list))
        (list_printer task_printer) tasks;
    exit 0

let exptree (module SA : Adapton.Signatures.SAType) rng =
    if !opt_input_size < 4 then begin
        Printf.eprintf "Task %s only supports -I n where n >= 4\n%!" !opt_task;
        exit 1
    end;
    if !opt_take_count != 1 then begin
        Printf.eprintf "Task %s only supports -T 1\n%!" !opt_task;
        exit 1
    end;
    let module F = SA.Make (Adapton.Types.Float) in
    let module E = struct
        type e = e' SA.thunk
        and e' = Num of float | Op of op * e * e
        and op = Plus | Mul | Minus | Div
        module E = SA.Make (struct
            type t = e'
            let hash seed = function
                | Num f -> Hashtbl.seeded_hash seed f
                | Op  ( op, x, y ) -> SA.hash (SA.hash (Hashtbl.seeded_hash seed op) x) y
            let equal x y = x == y || match x, y with
                | Num x, Num y -> x == y
                | Op ( op1, x1, y1 ), Op ( op2, x2, y2 ) -> op1 == op2 && SA.equal x1 x2 && SA.equal y1 y2
                | _ -> false
        end)
        include E
        let rand_num () = Num (Random.State.float rng 1.0)
        let rand_op x y =
            let op = if Random.State.bool rng then
                if Random.State.bool rng then Plus else Mul
            else
                if Random.State.bool rng then Minus else Div
            in
            if Random.State.bool rng then
                Op ( op, x, y )
            else
                Op ( op, y, x )
        let eval = F.memo (module E) begin fun eval e -> match E.force e with
            | Num f -> f
            | Op ( Plus, x, y ) -> F.force (eval x) +. F.force (eval y)
            | Op ( Mul, x, y ) -> F.force (eval x) *. F.force (eval y)
            | Op ( Minus, x, y ) -> F.force (eval x) -. F.force (eval y)
            | Op ( Div, x, y ) -> let y = F.force (eval y) in F.force (eval x) /. (if y == 0. then 1. else y)
        end
    end in
    SA.tweak_gc ();
    Gc.compact ();
    let start_time = get_time () in

    let x =
        let rec make_xs n acc = if n > 0 then
            make_xs (n - 1) (E.const (E.rand_num ())::acc)
        else
            let rec make_xs acc n = function
                | x::y::rest ->
                    let x = if Random.State.bool rng then
                        if Random.State.bool rng then E.Op ( E.Plus, x, y ) else E.Op ( E.Mul, x, y )
                    else
                        if Random.State.bool rng then E.Op ( E.Minus, x, y ) else E.Op ( E.Div, x, y )
                    in
                    make_xs (E.const x::acc) (n + 1) rest
                | y::[] when n > 0 -> make_xs [] 0 (y::acc)
                | y::[] -> y
                | [] -> make_xs [] 0 acc
            in
            make_xs [] 0 acc
        in
        make_xs !opt_input_size []
    in

    Printf.eprintf "%t\n%!" header;
    begin try
        let take, setup_stats = measure begin fun () ->
            let ys = Array.init !opt_repeat_count (fun _ -> E.eval x) in
            let take () = Array.iter (fun y -> ignore (F.force y)) ys in
            take ();
            take
        end in
        let setup_stats = finish setup_stats 1 in
        let setup_top_heap_stack = get_top_heap_stack () in

        if SA.is_self_adjusting then begin
            let half = int_of_float (floor (log (float_of_int !opt_input_size) /. log 2. /. 2.)) in
            let rec do_edits past n update_stats take_stats edit_count =
                if n == 0 then
                    ( update_stats, take_stats, edit_count )
                else begin
                    let past =
                        let now = get_time () in
                        if now -. past < 20. then
                            past
                        else begin
                            let heap, stack = get_top_heap_stack () in
                            Printf.eprintf "%t edit %10d %9.2fMB %9.2fMB\n%!"
                                header n (word_megabytes heap) (word_megabytes stack);
                            now
                        end
                    in

                    let update_stats', take_stats', edit_count' = if !opt_monotonic then
                        (* add/remove one leaf *)
                        let rec change add x = match E.force x with
                            | E.Op ( op, a, b ) ->
                                let dir = Random.State.bool rng in
                                let y = if dir then a else b in
                                begin match E.force y with
                                    | E.Num _ as z ->
                                        if add then
                                            E.update_const y (E.rand_op (E.const z) (E.const (E.rand_num ())))
                                        else
                                            E.update_const x (E.force (if dir then b else a))
                                    | _ ->
                                        change add y
                                end
                            | E.Num _ ->
                                failwith "change"
                        in

                        let (), update_stats = measure (fun () -> change false x) in
                        let (), take_stats = measure (fun () -> SA.refresh (); take ()) in
                        let (), update_stats' = measure (fun () -> change true x) in
                        let (), take_stats' = measure (fun () -> SA.refresh (); take ()) in

                        ( add update_stats update_stats', add take_stats take_stats', 2 )
                    else
                        (* swap two nodes *)
                        let rec pick n x =
                            if n == 0 then x else match E.force x with
                                | E.Op ( op, a, b ) ->
                                    let dir = Random.State.bool rng in
                                    pick (n - 1) (if dir then a else b)
                                | E.Num _ ->
                                    failwith "pick"
                        in
                        let a = pick half x in
                        let b =
                            let rec pick_b () = let b = pick half x in if b == a then pick_b () else b in
                            pick_b ()
                        in
                        let (), update_stats = measure begin fun () -> match E.force a, E.force b with
                            | E.Op ( aop, a1, a2 ), E.Op ( bop, b1, b2 ) ->
                                if Random.State.bool rng then
                                    if Random.State.bool rng then begin
                                        E.update_const a (E.Op ( aop, a1, b1 ));
                                        E.update_const b (E.Op ( bop, a2, b2 ));
                                    end else begin
                                        E.update_const a (E.Op ( aop, b2, a2 ));
                                        E.update_const b (E.Op ( bop, b1, a1 ));
                                    end
                                else
                                    if Random.State.bool rng then begin
                                        E.update_const a (E.Op ( aop, b1, a2 ));
                                        E.update_const b (E.Op ( bop, a1, b2 ));
                                    end else begin
                                        E.update_const a (E.Op ( aop, a1, b2 ));
                                        E.update_const b (E.Op ( bop, b1, a2 ));
                                    end
                            | _ ->
                                failwith "swap"
                        end in

                        let (), take_stats = measure (fun () -> SA.refresh (); take ()) in

                        ( update_stats, take_stats, 1 )
                    in

                    do_edits past (pred n)
                        (add update_stats update_stats')
                        (add take_stats take_stats')
                        (edit_count + edit_count')
                end
            in
            let update_stats, take_stats, edit_count = do_edits 0. !opt_edit_count zero zero 0 in
            let edit_top_heap_stack = get_top_heap_stack () in
            let update_stats = finish update_stats !opt_edit_count in
            let take_stats = finish take_stats !opt_edit_count in
            Printf.printf "{ \"setup\": { %a, %a }, \"edits\": { \"update\": { %a }, \"take\": { %a }, %a }, %s }\n%!"
                stats setup_stats max_heap_stack setup_top_heap_stack stats update_stats stats take_stats max_heap_stack edit_top_heap_stack units;
            Printf.eprintf "%t ... done (%9.2fs) %9.3gs edit %9.3gs\n%!"
                header (get_time () -. start_time) setup_stats.time (update_stats.time +. take_stats.time)
        end else begin
            Printf.printf "{ \"setup\": { %a, %a }, %s }\n%!"
                stats setup_stats max_heap_stack setup_top_heap_stack units;
            Printf.eprintf "%t ... done (%9.2fs) %9.3gs\n%!" header (get_time () -. start_time) setup_stats.time
        end

    with e ->
        let err = Printexc.to_string e in
        Printf.printf ("{ \"error\": %S }\n%!") err;
        Printf.eprintf "%s\n%!" err;
        Printf.eprintf "%t ... done (%9.2fs)\n%!" header (get_time () -. start_time)
    end;
    exit 0

let _ =
    Arg.parse (Arg.align [
        ( "-c", Arg.Unit show_config, " output available configuration" );
        ( "-m", Arg.Symbol ( (fst (List.split Adapton.All.sa_list)), (fun s -> opt_sa := s) ), "list module" );
        ( "-t", Arg.Symbol ( (fst (List.split tasks)), (fun s -> opt_task := s) ), "list task" );
        ( "-I", Arg.Set_int opt_input_size, "size input size" );
        ( "-R", Arg.Set_int opt_repeat_count, "count repeat count" );
        ( "-T", Arg.Set_int opt_take_count, "count take count" );
        ( "-E", Arg.Set_int opt_edit_count, "count edit count" );
        ( "-S", Arg.Set_int opt_random_seed, "seed random seed" );
        ( "-M", Arg.Set opt_monotonic, "monotonic edits" );
    ]) (fun s -> raise (Arg.Bad ("extraneous argument " ^ s))) (Sys.argv.(0) ^ " [options]");

    let rng = Random.State.make [| !opt_random_seed |] in
    Random.init (Random.State.bits rng);
    let module SA = (val (List.assoc !opt_sa Adapton.All.sa_list)) in
    begin match List.assoc !opt_task tasks with
        | `ExpTree -> exptree (module SA) rng
        | _ -> ()
    end;
    let module SABool = SA.Make (Adapton.Types.Bool) in
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
        | `Flip task ->
            if !opt_monotonic then begin
                Printf.eprintf "Task %s does not support -M\n%!" !opt_task;
                exit 1
            end;
            `Flip (task (module SAFloatList) (module SABool))
        | `ExpTree ->
            failwith "exptree"
    in

    let start_time = get_time () in

    let xs = ref (SAFloatList.const `Nil) in
    let xss = Array.init !opt_input_size begin fun _ ->
        xs := SAFloatList.const (`Cons (Random.State.float rng 1.0, !xs));
        !xs
    end in
    let xs = !xs in
    let last = ref 0 in

    let b = SABool.const false in

    Printf.eprintf "%t\n%!" header;
    try
        let take, setup_stats = measure begin fun () ->
            let take = match task with
                | `List task ->
                    let yss = Array.init !opt_repeat_count (fun _ -> task xs) in
                    (fun () -> Array.iter (fun ys -> ignore (SAFloatList.take ys !opt_take_count)) yss)
                | `One task ->
                    let ys = Array.init !opt_repeat_count (fun _ -> task xs) in
                    (fun () -> Array.iter (fun y -> ignore (SAFloatList.SAData.force y)) ys)
                | `Flip task ->
                    let yss = Array.init !opt_repeat_count (fun _ -> task xs b) in
                    (fun () -> Array.iter (fun ys -> ignore (SAFloatList.take ys !opt_take_count)) yss)
                | `ExpTree ->
                    failwith "exptree"
            in
            take ();
            take
        end in
        let setup_stats = finish setup_stats 1 in
        let setup_top_heap_stack = get_top_heap_stack () in

        if SA.is_self_adjusting then begin
            let rec do_edits past n update_stats take_stats edit_count =
                if n == 0 then
                    ( update_stats, take_stats, edit_count )
                else begin
                    let past =
                        let now = get_time () in
                        if now -. past < 20. then
                            past
                        else begin
                            let heap, stack = get_top_heap_stack () in
                            Printf.eprintf "%t edit %10d %9.2fMB %9.2fMB %6.0fs left\n%!"
                                header n (word_megabytes heap) (word_megabytes stack)
                                (float_of_int n *. (get_time () -. start_time) /. float_of_int (!opt_edit_count - n));
                            now
                        end
                    in

                    let update_stats', take_stats', edit_count' = match task with
                        | `List _ | `One _ when !opt_monotonic ->
                            (* delete then re-insert *)
                            let edit = Random.State.int rng !opt_input_size in
                            let zs = xss.(edit) in

                            let ( z', zs' ), delete_update_stats = measure begin fun () ->
                                match SAFloatList.force zs with
                                    | `Cons ( z', zs' ) ->
                                        SAFloatList.update_const zs (SAFloatList.force zs');
                                        ( z', zs' )
                                    | `Nil ->
                                        failwith "delete"
                            end in

                            let (), delete_take_stats = measure begin fun () ->
                                SA.refresh ();
                                take ()
                            end in

                            let (), insert_update_stats = measure begin fun () ->
                                SAFloatList.update_const zs (`Cons ( z', zs' ))
                            end in

                            let (), insert_take_stats = measure begin fun () ->
                                SA.refresh ();
                                take ()
                            end in

                            ( add delete_update_stats insert_update_stats, add delete_take_stats insert_take_stats, 2 )

                        | `List _ | `One _ ->
                            (* split into two and swap *)
                            let edit = 1 + Random.State.int rng (!opt_input_size - 2) in
                            let edit = if edit = !last then edit + 1 else edit in
                            let zs = xss.(edit) in

                            let (), update_stats = measure begin fun () ->
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

                            let (), take_stats = measure begin fun () ->
                                SA.refresh ();
                                take ()
                            end in

                            ( update_stats, take_stats, 1 )

                        | `Flip _ ->
                            (* change one value *)
                            let edit = Random.State.int rng !opt_input_size in
                            let value = Random.State.float rng 1.0 in
                            let zs = xss.(edit) in

                            let (), update_stats = measure begin fun () ->
                                SABool.update_const b (not (SABool.force b));
                                match SAFloatList.force zs with
                                    | `Cons ( _, zs' ) ->
                                        SAFloatList.update_const zs (`Cons ( value, zs' ))
                                    | `Nil ->
                                        failwith "flip"
                            end in

                            let (), take_stats = measure (fun () -> SA.refresh (); take ()) in
                            let (), update_stats' = measure (fun () -> SABool.update_const b (not (SABool.force b))) in
                            let (), take_stats' = measure (fun () -> SA.refresh (); take ()) in

                            ( add update_stats update_stats', add take_stats take_stats', 2 )

                        | `ExpTree ->
                            failwith "exptree"
                    in

                    do_edits past (pred n)
                        (add update_stats update_stats')
                        (add take_stats take_stats')
                        (edit_count + edit_count')
                end
            in
            let update_stats, take_stats, edit_count = do_edits 0. !opt_edit_count zero zero 0 in
            let edit_top_heap_stack = get_top_heap_stack () in
            let update_stats = finish update_stats edit_count in
            let take_stats = finish take_stats edit_count in
            Printf.printf "{ \"setup\": { %a, %a }, \"edits\": { \"update\": { %a }, \"take\": { %a }, %a }, %s }\n%!"
                stats setup_stats max_heap_stack setup_top_heap_stack stats update_stats stats take_stats max_heap_stack edit_top_heap_stack units;
            Printf.eprintf "%t ... done (%9.2fs) %9.3gs edit %9.3gs\n%!"
                header (get_time () -. start_time) setup_stats.time (update_stats.time +. take_stats.time)
        end else begin
            Printf.printf "{ \"setup\": { %a, %a }, %s }\n%!"
                stats setup_stats max_heap_stack setup_top_heap_stack units;
            Printf.eprintf "%t ... done (%9.2fs) %9.3gs\n%!" header (get_time () -. start_time) setup_stats.time
        end

    with e ->
        let err = Printexc.to_string e in
        Printf.printf ("{ \"error\": %S }\n%!") err;
        Printf.eprintf "%s\n%!" err;
        Printf.eprintf "%t ... done (%9.2fs)\n%!" header (get_time () -. start_time)
