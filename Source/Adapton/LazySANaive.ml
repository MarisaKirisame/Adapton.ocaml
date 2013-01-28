(** Naive variant of lazy self-adjusting values. *)

(** Types and operations common to lazy self-adjusting values containing any type. *)
module T = struct
    (** Lazy self-adjusting values containing ['a]. *)
    type 'a thunk = { (* 2 + 7 = 9 words (not including closures of receipt, repair, evaluate, and unmemo) *)
        id : int;
        mutable thunk : 'a thunk';
    }
    (**/**) (* auxiliary types *)
    and 'a thunk' =
        | MemoValue of repair * 'a * receipt * receipt list * 'a evaluate * unmemo (* 7 words *)
        | Value of repair * 'a * receipt * receipt list * 'a evaluate (* 6 words *)
        | MemoThunk of 'a evaluate * unmemo (* 3  words *)
        | Thunk of 'a evaluate (* 2 words *)
        | Const of 'a * receipt (* 3 words *)
    and unmemo = unit -> unit
    and 'a evaluate = unit -> 'a * receipt
    and receipt = visited -> (visited -> bool -> unit) -> unit
    and repair = visited -> (visited -> unit) -> unit
    and visited = (int, unit) Hashtbl.t
    (**/**)


    (**/**) (* change-propagation state *)
    let lazy_id_counter = ref 0
    let lazy_stack = ref []
    (**/**)


    (** Compute the hash value of a self-adjusting value. *)
    let hash seed m = Hashtbl.seeded_hash seed m.id

    (** Compute whether two self-adjusting values are equal. *)
    let equal = (==)

    (** Recompute self-adjusting values if necessary (unused by this module; a no-op). *)
    let refresh () = ()

    (** Return the value contained by a self-adjusting value, (re-)computing it if necessary. *)
    let force m =
        let value, receipt = match m.thunk with
            | MemoValue ( repair, v, _, _, _, _ ) | Value ( repair, v, _, _, _ ) ->
                (* compute the value if necessary *)
                repair (Hashtbl.create 0) (fun _ -> ());
                begin match m.thunk with
                    | MemoValue ( _, value, receipt, _, _, _ ) | Value ( _, value, receipt, _, _ ) ->
                        ( value, receipt )
                    | MemoThunk _ | Thunk _ | Const _ ->
                        failwith "repair did not compute result"
                end
            | MemoThunk ( evaluate, _ ) | Thunk evaluate ->
                evaluate ()
            | Const ( value, receipt ) ->
                ( value, receipt )
        in
        (* add receipt to caller *)
        begin match !lazy_stack with
            | h::_ ->
                h := receipt::!h
            | _ ->
                ()
        end;
        value
end
include T


(** Functor to make constructors and updaters for lazy self-adjusting values of a specific type. *)
module Make (R : Hashtbl.SeededHashedType) : Signatures.SAType.S with type data = R.t and type t = R.t thunk = struct
    include T

    (** Value contained by lazy self-adjusting values for a specific type. *)
    type data = R.t

    (** Lazy self-adjusting values for a specific type. *)
    type t = R.t thunk

    (**/**) (* helper function to call unmemo on a thunk *)
    let unmemo m = match m.thunk with
        | MemoValue ( _, _, _, _, _, unmemo ) | MemoThunk ( _, unmemo ) -> unmemo ()
        | Value _  | Thunk _ | Const _ -> ()
    (**/**)

    (**/**) (* helper function to make a const receipt *)
    let make_const_receipt m x s k = match m.thunk with
        | MemoValue ( repair, _, _, _, _, _ ) | Value ( repair, _, _, _, _ ) ->
            repair s begin fun s -> k s begin match m.thunk with
                | MemoValue ( _, value, _, _, _, _ ) | Value ( _, value, _, _, _ ) | Const ( value, _ ) -> R.equal value x
                | MemoThunk _ | Thunk _ -> false
            end end
        | MemoThunk _ | Thunk _ ->
            k s false
        | Const ( value, _ ) ->
            k s (R.equal value x)
    (**/**)

    (** Create a lazy self-adjusting value from a constant value that does not depend on other lazy self-adjusting values. *)
    let const x =
        let rec receipt s k = make_const_receipt m x s k
        and m = { id=(!lazy_id_counter); thunk=Const ( x, receipt ) } in
        incr lazy_id_counter;
        m

    (** Update a lazy self-adjusting value with a constant value that does not depend on other lazy self-adjusting values. *)
    let update_const m x =
        unmemo m;
        let receipt s k = make_const_receipt m x s k in
        m.thunk <- Const ( x, receipt )

    (**/**) (* helper function to evaluate a thunk *)
    let evaluate_actual m f =
        (* add self to call stack and evaluate *)
        let dependencies = ref [] in
        lazy_stack := dependencies::!lazy_stack;
        let value = try
            f ()
        with exn ->
            lazy_stack := List.tl !lazy_stack;
            raise exn
        in
        lazy_stack := List.tl !lazy_stack;
        let dependencies = List.rev !dependencies in

        (* receipt/repair performs a truncated inorder traversal of the dependency graph *)
        let rec receipt s k = repair s begin fun s -> k s begin match m.thunk with
            | MemoValue ( _, value', _, _, _, _ ) | Value ( _, value', _, _, _ ) | Const ( value', _ ) -> R.equal value' value
            | MemoThunk _ | Thunk _ -> false
        end end

        and repair s k =
            if Hashtbl.mem s m.id then
                k s
            else begin
                Hashtbl.add s m.id ();
                match m.thunk with
                    | MemoValue ( _, _, _, dependencies, evaluate, _ ) | Value ( _, _, _, dependencies, evaluate ) ->
                        let rec repair s = function
                            | d::ds -> d s (fun s c -> if c then repair s ds else (ignore (evaluate ()); k s))
                            | [] -> k s
                        in
                        repair s dependencies
                    | MemoThunk ( evaluate, _ ) | Thunk evaluate ->
                        ignore (evaluate ()); k s
                    | Const _ ->
                        k s
            end
        in
        ( repair, value, receipt, dependencies )
    (**/**)

    (**/**) (** helper function to make a function to evaluate a thunk *)
    let make_evaluate m f =
        let rec evaluate () =
            let repair, value, receipt, dependencies = evaluate_actual m f in
            m.thunk <- Value ( repair, value, receipt, dependencies, evaluate );
            ( value, receipt )
        in
        evaluate
    (**/**)

    (** Create a lazy self-adjusting value from a thunk that may depend on other lazy self-adjusting values. *)
    let thunk f =
        let rec evaluate () = make_evaluate m f ()
        and m = { id=(!lazy_id_counter); thunk=Thunk evaluate } in
        incr lazy_id_counter;
        m

    (** Update a lazy self-adjusting value with a thunk that may depend on other lazy self-adjusting values. *)
    let update_thunk m f =
        unmemo m;
        let evaluate () = make_evaluate m f () in
        m.thunk <- Thunk evaluate

    (* create memoizing constructors and updaters *)
    include MemoN.Make (struct
        type data = R.t
        type t = R.t thunk

        (** Create memoizing constructor and updater for a lazy self-adjusting value. *)
        let memo (type a) (module A : Hashtbl.SeededHashedType with type t = a) f =
            let module Memotable = Weak.Make (struct
                type t = A.t * R.t thunk
                let seed = Random.bits ()
                let hash ( a, _ ) = A.hash seed a
                let equal ( a, _ ) ( a', _ ) = A.equal a a'
            end) in
            let memotable = Memotable.create 0 in

            (**/**) (* helper function to make a function to evaluate a thunk with memoization *)
            let rec make_memo_evaluate m x unmemo =
                let rec evaluate () =
                    let repair, value, receipt, dependencies = evaluate_actual m (fun () -> f memo x) in
                    m.thunk <- MemoValue ( repair, value, receipt, dependencies, evaluate, unmemo );
                    ( value, receipt )
                in
                evaluate
            (**/**)

            (* memoizing constructor *)
            and memo x =
                let rec evaluate () = make_memo_evaluate m x unmemo ()
                (* create a strong reference to binding and hide it in the closure unmemo stored in m *)
                and binding = ( x, m )
                and unmemo () = Memotable.remove memotable binding

                and m = { id=(!lazy_id_counter); thunk=MemoThunk ( evaluate, unmemo ) } in
                incr lazy_id_counter;
                snd (Memotable.merge memotable binding)
            in

            (* memoizing updater *)
            let update_memo m x =
                unmemo m;
                let rec evaluate () = make_memo_evaluate m x unmemo ()
                (* create a strong reference to binding and hide it in the closure unmemo stored in m *)
                and binding = ( x, m )
                and unmemo () = Memotable.remove memotable binding in

                if Memotable.merge memotable binding == binding then
                    m.thunk <- MemoThunk ( evaluate, unmemo )
                else
                    let evaluate = make_evaluate m (fun () -> f memo x) in
                    m.thunk <- Thunk evaluate;
            in

            ( memo, update_memo )
    end)
end
