
open AdaptonUtil.Statistics

let list_filter_task (type a) (module L : AdaptonUtil.Signatures.AListType.S with type t = a and type data = float) =
    L.memo_filter (fun x -> log (1. +. x) < log 1.5)

let list_map_task (type a) (module L : AdaptonUtil.Signatures.AListType.S with type t = a and type data = float) =
    L.memo_map (module L) (fun x -> log (1. +. x) +. log 1.5)

let list_tfold_min_task (type a) (type b) (module L : AdaptonUtil.Signatures.AListType.S with type t = a and type AData.t = b and type data = float) =
    L.memo_tfold min

let list_tfold_sum_task (type a) (type b) (module L : AdaptonUtil.Signatures.AListType.S with type t = a and type AData.t = b and type data = float) =
    L.memo_tfold (+.)

let list_quicksort_task (type a) (module L : AdaptonUtil.Signatures.AListType.S with type t = a and type data = float) =
    L.memo_quicksort Pervasives.compare

let list_mergesort_task (type a) (module L : AdaptonUtil.Signatures.AListType.S with type t = a and type data = float) =
    L.memo_mergesort Pervasives.compare

let list_updown1_task
        (type a) (module L : AdaptonUtil.Signatures.AListType.S with type t = a and type data = float)
        (type b) (module B : AdaptonUtil.Signatures.AType.S with type t = b and type data = bool)
        xs b =
    let up = L.memo_quicksort Pervasives.compare in
    let down = L.memo_quicksort (fun x y -> -(Pervasives.compare x y)) in
    L.thunk (fun () -> L.force (if B.force b then up xs else down xs))

let list_updown2_task
        (type a) (module L : AdaptonUtil.Signatures.AListType.S with type t = a and type data = float)
        (type b) (module B : AdaptonUtil.Signatures.AType.S with type t = b and type data = bool)
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

let opt_a = ref (fst (List.hd AdaptonZoo.All.a_list))
let opt_task = ref "filter"
let opt_input_size = ref 1
let opt_repeat_count = ref 1
let opt_edit_count = ref 1
let opt_take_count = ref 1
let opt_random_seed = ref 1
let opt_monotonic = ref false

let header ff = Printf.fprintf ff "%24s %24s %8d %8d %20d" !opt_a !opt_task !opt_take_count !opt_input_size !opt_random_seed
let stats ff s =
    Printf.fprintf ff "\"time\": %.17g, \"heap\": %.17g, \"stack\": %.17g, \"update\": %.17g, \"evaluate\": %.17g, \"dirty\": %.17g, \"clean\": %.17g"
        s.time s.heap s.stack s.update s.evaluate s.dirty s.clean
let top_heap_stack ff ( heap, stack ) = Printf.fprintf ff "\"max-heap\": %.17g, \"max-stack\": %.17g" (word_bytes heap) (word_bytes stack)
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
        (list_printer (fun ff -> Printf.fprintf ff "%S")) (fst (List.split AdaptonZoo.All.a_list))
        (list_printer task_printer) tasks;
    exit 0

let do_benchmark (module A : AdaptonUtil.Signatures.AType) ~make_input ~setup ~do_edit =
    A.tweak_gc ();
    Gc.compact ();
    let start_time = get_time () in

    let input = make_input () in

    Printf.eprintf "%t\n%!" header;
    try
        let take, setup_stats = measure begin fun () ->
            let take = setup input in
            take ();
            take
        end in
        let setup_stats = finish setup_stats 1 in
        let setup_top_heap_stack = get_top_heap_stack () in

        if A.is_incremental then begin
            let rec do_edits past n update_stats take_stats edit_count =
                if n == 0 then
                    ( update_stats, take_stats, edit_count )
                else
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
                    let update_stats', take_stats', edit_count' = do_edit input (fun () -> A.refresh (); take ()) in
                    do_edits past (pred n)
                        (add update_stats update_stats')
                        (add take_stats take_stats')
                        (edit_count + edit_count')
            in
            let update_stats, take_stats, edit_count = do_edits 0. !opt_edit_count zero zero 0 in
            let edit_top_heap_stack = get_top_heap_stack () in
            let update_stats = finish update_stats edit_count in
            let take_stats = finish take_stats edit_count in
            Printf.printf "{ \"setup\": { %a, %a }, \"edits\": { \"update\": { %a }, \"take\": { %a }, %a }, %s }\n%!"
                stats setup_stats top_heap_stack setup_top_heap_stack stats update_stats stats take_stats top_heap_stack edit_top_heap_stack units;
            Printf.eprintf "%t ... done (%9.2fs) %9.3gs edit %9.3gs\n%!"
                header (get_time () -. start_time) setup_stats.time (update_stats.time +. take_stats.time)
        end else begin
            Printf.printf "{ \"setup\": { %a, %a }, %s }\n%!"
                stats setup_stats top_heap_stack setup_top_heap_stack units;
            Printf.eprintf "%t ... done (%9.2fs) %9.3gs\n%!" header (get_time () -. start_time) setup_stats.time
        end
    with e ->
        let err = Printexc.to_string e in
        Printf.printf ("{ \"error\": %S }\n%!") err;
        Printf.eprintf "%s\n%!" err;
        Printf.eprintf "%t ... done (%9.2fs)\n%!" header (get_time () -. start_time)

let exptree (module A : AdaptonUtil.Signatures.AType) rng =
    if !opt_input_size < 4 then begin
        Printf.eprintf "Task %s only supports -I n where n >= 4\n%!" !opt_task;
        exit 1
    end;
    if !opt_take_count != 1 then begin
        Printf.eprintf "Task %s only supports -T 1\n%!" !opt_task;
        exit 1
    end;
    let module F = A.Make (AdaptonUtil.Types.Float) in
    let module E = struct
        type e = e' A.thunk
        and e' = Num of float | Op of op * e * e
        and op = Plus | Mul | Minus | Div
        module E = A.Make (struct
            type t = e'
            let hash seed = function
                | Num f -> Hashtbl.seeded_hash seed f
                | Op  ( op, x, y ) -> A.hash (A.hash (Hashtbl.seeded_hash seed op) x) y
            let equal x y = x == y || match x, y with
                | Num x, Num y -> x == y
                | Op ( op1, x1, y1 ), Op ( op2, x2, y2 ) -> op1 == op2 && A.equal x1 x2 && A.equal y1 y2
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
    let half = int_of_float (floor (log (float_of_int !opt_input_size) /. log 2. /. 2.)) in

    do_benchmark (module A)
        ~make_input:begin fun () ->
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
        end
        ~setup:begin fun x ->
            let ys = Array.init !opt_repeat_count (fun _ -> E.eval x) in
            fun () -> Array.iter (fun y -> ignore (F.force y)) ys
        end
        ~do_edit:begin fun x take ->
            if !opt_monotonic then
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
                let (), take_stats = measure take in
                let (), update_stats' = measure (fun () -> change true x) in
                let (), take_stats' = measure take in

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

                let (), take_stats = measure take in

                ( update_stats, take_stats, 1 )
            end

let listtasks (module A : AdaptonUtil.Signatures.AType) rng =
    let module ABool = A.Make (AdaptonUtil.Types.Bool) in
    let module AList = AdaptonUtil.AList.Make (A) in
    let module AFloatList = AList.Make (AdaptonUtil.Types.Float) in
    let task = match List.assoc !opt_task tasks with
        | `One task ->
            if !opt_take_count != 1 then begin
                Printf.eprintf "Task %s only supports -T 1\n%!" !opt_task;
                exit 1
            end;
            `One (task (module AFloatList))
        | `List task ->
            `List (task (module AFloatList))
        | `Flip task ->
            if !opt_monotonic then begin
                Printf.eprintf "Task %s does not support -M\n%!" !opt_task;
                exit 1
            end;
            `Flip (task (module AFloatList) (module ABool))
        | `ExpTree ->
            failwith "exptree"
    in

    do_benchmark (module A)
        ~make_input:begin fun () ->
                let xs = ref (AFloatList.const `Nil) in
                let xss = Array.init !opt_input_size begin fun _ ->
                    xs := AFloatList.const (`Cons (Random.State.float rng 1.0, !xs));
                    !xs
                end in
                let xs = !xs in
                let last = ref 0 in
                let b = ABool.const false in
                ( xs, xss, last, b )
        end
        ~setup:begin fun ( xs, _, _, b ) ->
            match task with
                | `List task ->
                    let yss = Array.init !opt_repeat_count (fun _ -> task xs) in
                    (fun () -> Array.iter (fun ys -> ignore (AFloatList.take ys !opt_take_count)) yss)
                | `One task ->
                    let ys = Array.init !opt_repeat_count (fun _ -> task xs) in
                    (fun () -> Array.iter (fun y -> ignore (AFloatList.AData.force y)) ys)
                | `Flip task ->
                    let yss = Array.init !opt_repeat_count (fun _ -> task xs b) in
                    (fun () -> Array.iter (fun ys -> ignore (AFloatList.take ys !opt_take_count)) yss)
                | `ExpTree ->
                    failwith "exptree"
        end
        ~do_edit:begin fun ( xs, xss, last, b ) take ->
            match task with
                | `List _ | `One _ when !opt_monotonic ->
                    (* delete then re-insert *)
                    let edit = Random.State.int rng !opt_input_size in
                    let zs = xss.(edit) in

                    let ( z', zs' ), delete_update_stats = measure begin fun () ->
                        match AFloatList.force zs with
                            | `Cons ( z', zs' ) ->
                                AFloatList.update_const zs (AFloatList.force zs');
                                ( z', zs' )
                            | `Nil ->
                                failwith "delete"
                    end in

                    let (), delete_take_stats = measure take in
                    let (), insert_update_stats = measure (fun () -> AFloatList.update_const zs (`Cons ( z', zs' ))) in
                    let (), insert_take_stats = measure take in

                    ( add delete_update_stats insert_update_stats, add delete_take_stats insert_take_stats, 2 )

                | `List _ | `One _ ->
                    (* split into two and swap *)
                    let edit = 1 + Random.State.int rng (!opt_input_size - 2) in
                    let edit = if edit = !last then edit + 1 else edit in
                    let zs = xss.(edit) in

                    let (), update_stats = measure begin fun () ->
                        match AFloatList.force xs with
                            | `Cons _ as xs' ->
                                begin match AFloatList.force xss.(!last) with
                                    | `Cons _ as last' ->
                                        begin match AFloatList.force zs with
                                            | `Cons _ as zs' ->
                                                AFloatList.update_const xs zs';
                                                AFloatList.update_const xss.(!last) xs';
                                                AFloatList.update_const zs last';
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

                    let (), take_stats = measure take in

                    ( update_stats, take_stats, 1 )

                | `Flip _ ->
                    (* change one value *)
                    let edit = Random.State.int rng !opt_input_size in
                    let value = Random.State.float rng 1.0 in
                    let zs = xss.(edit) in

                    let (), update_stats = measure begin fun () ->
                        ABool.update_const b (not (ABool.force b));
                        match AFloatList.force zs with
                            | `Cons ( _, zs' ) ->
                                AFloatList.update_const zs (`Cons ( value, zs' ))
                            | `Nil ->
                                failwith "flip"
                    end in

                    let (), take_stats = measure take in
                    let (), update_stats' = measure (fun () -> ABool.update_const b (not (ABool.force b))) in
                    let (), take_stats' = measure take in

                    ( add update_stats update_stats', add take_stats take_stats', 2 )

                | `ExpTree ->
                    failwith "exptree"
        end

let _ =
    Arg.parse (Arg.align [
        ( "-c", Arg.Unit show_config, " output available configuration" );
        ( "-m", Arg.Symbol ( (fst (List.split AdaptonZoo.All.a_list)), (fun s -> opt_a := s) ), "list module" );
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
    let module A = (val (List.assoc !opt_a AdaptonZoo.All.a_list)) in
    match List.assoc !opt_task tasks with
        | `ExpTree -> exptree (module A) rng
        | _ -> listtasks (module A) rng
