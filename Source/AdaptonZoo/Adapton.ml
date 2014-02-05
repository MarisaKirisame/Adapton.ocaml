(** Lazy incremental computation based a demand-computation graph. *)

open AdaptonInternal
open AdaptonUtil

(** Types and operations common to Adapton thunks containing any type. *)
module T = struct
    (** Abstract type identifying this module. *)
    type atype

    module rec TT : sig
        (** Adapton thunks containing ['a]. *)
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
            | MemoValue of 'a repair * 'a * receipt * dependency list * 'a evaluate * unmemo (* 7 words *)
            | Value of 'a repair * 'a * receipt * dependency list * 'a evaluate (* 6 words *)
            | MemoThunk of 'a evaluate * unmemo (* 3  words *)
            | Thunk of 'a evaluate (* 2 words *)
            | Const of 'a * receipt (* 3 words *)
        and unmemo = unit -> unit
        and 'a evaluate = unit -> 'a * receipt
        and receipt = { check : 'a . (bool -> 'a) -> 'a }
        and 'a repair = { repair : 'b . ('a * receipt -> 'b) -> 'b }
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


    (** This module implements incremental thunks. *)
    let is_incremental = true

    (** This module implements lazy thunks. *)
    let is_lazy = true


    (**/**) (* change-propagation state *)
    let lazy_id_counter = Types.Counter.make 0
    let lazy_stack = ref []
    (**/**)


    (** Return the id of an Adapton thunk. *)
    let id m = m.meta.id

    (** Compute the hash value of an Adapton thunk. *)
    let hash seed m = Hashtbl.seeded_hash seed m.meta.id

    (** Compute whether two Adapton thunks are equal. *)
    let equal = (==)

    (** Recompute Adapton thunks if necessary (unused by this module; a no-op). *)
    let refresh () = ()

    (** Return the value contained by an Adapton thunk, (re-)computing it if necessary. *)
    let force m =
        let value, receipt = match m.thunk with
            | MemoValue ( repair, _, _, _, _, _ ) | Value ( repair, _, _, _, _ ) ->
                repair.repair (fun result -> result)
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


(** Functor to make constructors for Adapton thunks of a specific type. *)
module Make (R : Hashtbl.SeededHashedType)
        : Signatures.AType.S with type atype = atype and type 'a thunk = 'a thunk and type data = R.t and type t = R.t thunk = struct
    include T

    (** Value contained by Adapton thunks for a specific type. *)
    type data = R.t

    (** Adapton thunks for a specific type. *)
    type t = R.t thunk

    (** Module representing type [data]. *)
    module Data = R

    (**/**) (* helper function to make a new thunk meta *)
    let make_meta () = { id=Types.Counter.next lazy_id_counter; dependents=Dependents.create 0 }
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
                        incr Statistics.Counts.dirty;
                        d.dirty <- true;
                        d.dependent.dependents::ds
                    end
                end d ds end
            | [] ->
                ()
        in
        dirty [ m.meta.dependents ]
    (**/**)

    (**/**) (* helper function to make a receipt check *)
    let make_check m x k = match m.thunk with
        | MemoValue ( repair, _, _, _, _, _ ) | Value ( repair, _, _, _, _ ) -> repair.repair (fun ( value, _ ) -> k (R.equal value x))
        | MemoThunk _ | Thunk _ -> k false
        | Const ( value, _ ) -> k (R.equal value x)
    (**/**)

    (** Create an Adapton thunk from a constant value that does not depend on other Adapton thunks. *)
    let const x =
        let rec check : 'a . (bool -> 'a) -> 'a = fun k -> make_check m x k
        and m = { meta=make_meta (); thunk=Const ( x, { check } ) } in
        m

    (** Update an Adapton thunk with a constant value that does not depend on other Adapton thunks. *)
    let update_const m x =
        incr Statistics.Counts.update;
        begin match m.thunk with
            | MemoValue ( _, value, _, _, _, _ ) | Value ( _, value, _, _, _ ) | Const ( value, _ ) when not (R.equal value x) -> dirty m
            | MemoValue _ | MemoThunk _ | Value _ | Thunk _ | Const _ -> unmemo m
        end;
        let check k = make_check m x k in
        m.thunk <- Const ( x, { check } )

    (**/**) (* helper function to evaluate a thunk *)
    let evaluate_actual m f =
        (* add self to call stack and evaluate *)
        incr Statistics.Counts.evaluate;
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
            | MemoValue ( _, value, receipt, dependencies, evaluate, _ ) | Value ( _, value, receipt, dependencies, evaluate ) ->
                let rec repair = function
                    | d::ds ->
                        if d.dirty then begin
                            d.dirty <- false;
                            d.receipt.check (fun c -> if c then (incr Statistics.Counts.clean; repair ds) else k (evaluate ()))
                        end else
                            repair ds
                    | [] ->
                        k ( value, receipt )
                in
                repair dependencies
            | MemoThunk ( evaluate, _ ) | Thunk evaluate ->
                k (evaluate ())
            | Const ( value, receipt ) ->
                k ( value, receipt )
        in

        let check k = make_check m value k in

        ( { repair }, value, { check }, dependencies )
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

    (** Create an Adapton thunk from a function that may depend on other Adapton thunks. *)
    let thunk f =
        let rec evaluate () = make_evaluate m f ()
        and m = { meta=make_meta (); thunk=Thunk evaluate } in
        m

    (** Update an Adapton thunk with a function that may depend on other Adapton thunks. *)
    let update_thunk m f =
        incr Statistics.Counts.update;
        dirty m;
        let evaluate () = make_evaluate m f () in
        m.thunk <- Thunk evaluate

    (* create memoizing constructors *)
    include MemoN.Make (struct
        type data = R.t
        type t = R.t thunk

        (** Create memoizing constructor for an Adapton thunk. *)
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
                (* note that m contains unmemo that indirectly holds a reference to binding (via unmemo's closure);
                    this prevents the GC from collecting binding from memotable until m itself is collected (or unmemo is removed from m) *)
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