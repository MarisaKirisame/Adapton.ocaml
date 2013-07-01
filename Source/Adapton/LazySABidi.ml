(** Bidirectional variant of lazy self-adjusting values based on {!LazySANaive}. *)

(** Types and operations common to lazy self-adjusting values containing any type. *)
module T = struct
    (** Abstract type identifying this module for self-adjusting values. *)
    type sa

    module rec TT : sig
        (** Lazy self-adjusting values containing ['a]. *)
        type 'a thunk = { (* 2 + 2 + 7 = 11 words (not including closures of receipt, repair, evaluate, and unmemo) *)
            meta : meta;
            mutable thunk : 'a thunk';
        }
        (**/**) (* auxiliary types *)
        and meta = { (* 2 words (not including Dependents.t) *)
            id : int;
            dependents : Dependents.t;
        }
        and 'a thunk' =
            | MemoValue of repair * 'a * receipt * dependency list * 'a evaluate * unmemo (* 7 words *)
            | Value of repair * 'a * receipt * dependency list * 'a evaluate (* 6 words *)
            | MemoThunk of 'a evaluate * unmemo (* 3  words *)
            | Thunk of 'a evaluate (* 2 words *)
            | Const of 'a * receipt (* 3 words *)
        and unmemo = unit -> unit
        and 'a evaluate = unit -> 'a * receipt
        and receipt = (bool -> unit) -> unit
        and repair = (unit -> unit) -> unit
        and dependency = { (* 3 words (meta shared with 'a thunk) *)
            mutable dirty : bool;
            mutable receipt : receipt;
            dependent : meta;
        }
        (**/**)
    end = TT
    (**/**) (* more auxiliary types *)
    and Dependents : WeakSet.S with type data = TT.dependency = WeakSet.Make (struct
        type t = TT.dependency
        let hash d = Hashtbl.hash d.TT.dependent.TT.id
        let equal d d' = d.TT.dependent == d'.TT.dependent
    end)
    include TT
    (**/**)


    (** This module implements self-adjusting values. *)
    let is_self_adjusting = true

    (** This module implements lazy values. *)
    let is_lazy = true


    (**/**) (* change-propagation state *)
    let lazy_id_counter = ref 0
    let lazy_stack = ref []
    (**/**)


    (** Compute the hash value of a self-adjusting value. *)
    let hash seed m = Hashtbl.seeded_hash seed m.meta.id

    (** Compute whether two self-adjusting values are equal. *)
    let equal = (==)

    (** Recompute self-adjusting values if necessary (unused by this module; a no-op). *)
    let refresh () = ()

    (** Return the value contained by a self-adjusting value, (re-)computing it if necessary. *)
    let force m =
        let value, receipt = match m.thunk with
            | MemoValue ( repair, _, _, _, _, _ ) | Value ( repair, _, _, _, _ ) ->
                (* compute the value if necessary *)
                repair (fun () -> ());
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
        (* add dependency to caller *)
        begin match !lazy_stack with
            | ( dependent, dependencies )::_ ->
                let dependency = Dependents.merge m.meta.dependents { dirty=false; receipt; dependent } in
                (* an existing dependency may be reused *)
                dependency.dirty <- false;
                dependency.receipt <- receipt;
                dependencies := dependency::!dependencies
            | _ ->
                ()
        end;
        value
end
include T


(** Functor to make constructors for lazy self-adjusting values of a specific type. *)
module Make (R : Signatures.EqualsType)
        : Signatures.SAType.S with type sa = sa and type 'a thunk = 'a thunk and type data = R.t and type t = R.t thunk = struct
    include T

    (** Value contained by lazy self-adjusting values for a specific type. *)
    type data = R.t

    (** Lazy self-adjusting values for a specific type. *)
    type t = R.t thunk

    (**/**) (* helper function to make a new thunk meta *)
    let make_meta () =
        let meta = { id=(!lazy_id_counter); dependents=Dependents.create 0 } in
        incr lazy_id_counter;
        meta
    (**/**)

    (**/**) (* helper function to unmemo a thunk *)
    let unmemo m = match m.thunk with
        | MemoValue ( _, _, _, _, _, unmemo ) | MemoThunk ( _, unmemo ) -> unmemo ()
        | Value _  | Thunk _ | Const _ -> ()
    (**/**)

    (**/**) (* helper function to dirty a thunk *)
    let dirty m =
        unmemo m;
        let rec dirty = function
            | d::ds ->
                dirty begin Dependents.fold begin fun d ds ->
                    if d.dirty then
                        ds
                    else begin
                        d.dirty <- true;
                        d.dependent.dependents::ds
                    end
                end d ds end
            | [] ->
                ()
        in
        dirty [ m.meta.dependents ]
    (**/**)

    (**/**) (* helper function to make a const receipt *)
    let make_const_receipt m x k = match m.thunk with
        | MemoValue ( repair, _, _, _, _, _ ) | Value ( repair, _, _, _, _ ) ->
            repair begin fun () -> k begin match m.thunk with
                | MemoValue ( _, value, _, _, _, _ ) | Value ( _, value, _, _, _ ) | Const ( value, _ ) -> R.equal value x
                | MemoThunk _ | Thunk _ -> false
            end end
        | MemoThunk _ | Thunk _ ->
            k false
        | Const ( value, _ ) ->
            k (R.equal value x)
    (**/**)

    (** Create a lazy self-adjusting value from a constant value that does not depend on other lazy self-adjusting values. *)
    let const x =
        let rec receipt k = make_const_receipt m x k
        and m = { meta=make_meta (); thunk=Const ( x, receipt ) } in
        m

    (** Update a lazy self-adjusting value with a constant value that does not depend on other lazy self-adjusting values. *)
    let update_const m x =
        begin match m.thunk with
            | MemoValue ( _, value, _, _, _, _ ) | Value ( _, value, _, _, _ ) | Const ( value, _ ) when not (R.equal value x) -> dirty m
            | MemoValue _ | MemoThunk _ | Value _ | Thunk _ | Const _ -> unmemo m
        end;
        let receipt k = make_const_receipt m x k in
        m.thunk <- Const ( x, receipt )

    (**/**) (* helper function to evaluate a thunk *)
    let evaluate_actual m f =
        (* add self to call stack and evaluate *)
        let dependencies = ref [] in
        lazy_stack := ( m.meta, dependencies )::!lazy_stack;
        let value = try
            f ()
        with exn ->
            lazy_stack := List.tl !lazy_stack;
            raise exn
        in
        lazy_stack := List.tl !lazy_stack;
        let dependencies = List.rev !dependencies in

        (* repair/receipt performs a truncated inorder traversal of the dependency graph *)
        let repair k = match m.thunk with
            | MemoValue ( _, _, _, dependencies, evaluate, _ ) | Value ( _, _, _, dependencies, evaluate ) ->
                let rec repair = function
                    | d::ds ->
                        if d.dirty then begin
                            d.dirty <- false;
                            d.receipt (fun c -> if c then repair ds else (ignore (evaluate ()); k ()))
                        end else
                            repair ds
                    | [] ->
                        k ()
                in
                repair dependencies
            | MemoThunk ( evaluate, _ ) | Thunk evaluate ->
                ignore (evaluate ()); k ()
            | Const _ ->
                k ()
        in

        let receipt k = repair begin fun () -> k begin match m.thunk with
            | MemoValue ( _, value', _, _, _, _ ) | Value ( _, value', _, _, _ ) | Const ( value', _ ) -> R.equal value' value
            | MemoThunk _ | Thunk _ -> false
        end end in

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
        and m = { meta=make_meta (); thunk=Thunk evaluate } in
        m

    (** Update a lazy self-adjusting value with a thunk that may depend on other lazy self-adjusting values. *)
    let update_thunk m f =
        dirty m;
        let evaluate () = make_evaluate m f () in
        m.thunk <- Thunk evaluate

    (* create memoizing constructors *)
    include MemoN.Make (struct
        type data = R.t
        type t = R.t thunk

        (** Create memoizing constructor for a lazy self-adjusting value. *)
        let memo (type a) (module A : Hashtbl.SeededHashedType with type t = a) f =
            let module Memotable = Weak.Make (struct
                type t = A.t * R.t thunk
                let seed = Random.bits ()
                let hash ( a, _ ) = A.hash seed a
                let equal ( a, _ ) ( a', _ ) = A.equal a a'
            end) in
            let memotable = Memotable.create 0 in

            (* memoizing constructor *)
            let rec memo x =
                (* create a strong reference to binding and hide it in the closure unmemo stored in m *)
                let rec binding = ( x, m )
                and unmemo () = Memotable.remove memotable binding
                and evaluate () =
                    let repair, value, receipt, dependencies = evaluate_actual m (fun () -> f memo x) in
                    m.thunk <- MemoValue ( repair, value, receipt, dependencies, evaluate, unmemo );
                    ( value, receipt )
                and m = { meta=make_meta (); thunk=MemoThunk ( evaluate, unmemo ) } in
                snd (Memotable.merge memotable binding)
            in
            memo
    end)
end

(** Tweak GC for this module. *)
let tweak_gc () = ()
