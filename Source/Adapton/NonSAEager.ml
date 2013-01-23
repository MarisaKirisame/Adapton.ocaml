(** Eager variant of non-self-adjusting values. *)

(** Types and operations common to eager non-self-adjusting values containing any type. *)
module T = struct
    (** Eager non-self-adjusting values containing ['a]. *)
    type 'a thunk = {
        id : int;
        value : 'a;
    }

    (**/**) (* internal state *)
    let eager_id_counter = ref 0
    (**/**)

    (** Compute the hash value of a non-self-adjusting value. *)
    let hash seed m = Hashtbl.seeded_hash seed m.id

    (** Compute whether two non-self-adjusting values are equal. *)
    let equal = (==)

    (** Recompute non-self-adjusting values if necessary (not supported by this module; raises {!NonSelfAdjustingValue}). *)
    let refresh () = raise Exceptions.NonSelfAdjustingValue

    (** Return the value contained by a non-self-adjusting value, computing it if necessary. *)
    let force { value; _ } = value
end
include T


(** Functor to make constructors and updaters for eager non-self-adjusting values of a specific type. *)
module Make (R : Hashtbl.SeededHashedType) : Signatures.SAType.S with type data = R.t and type t = R.t thunk = struct
    include T

    (** Value contained by eager non-self-adjusting values for a specific type. *)
    type data = R.t

    (** Eager non-self-adjusting values for a specific type. *)
    type t = R.t thunk

    (** Create an eager non-self-adjusting value from a constant value. *)
    let const x =
        let m = { id=(!eager_id_counter); value=x } in
        incr eager_id_counter;
        m

    (** Update an eager non-self-adjusting value with a constant value (not supported by this module; raises {!NonSelfAdjustingValue}). *)
    let update_const _ _ = raise Exceptions.NonSelfAdjustingValue

    (** Create an eager non-self-adjusting value from a thunk. *)
    let thunk f =
        let m = { id=(!eager_id_counter); value=f () } in
        incr eager_id_counter;
        m

    (** Update an eager non-self-adjusting value with a thunk (not supported by this module; raises {!NonSelfAdjustingValue}). *)
    let update_thunk _ _ = raise Exceptions.NonSelfAdjustingValue

    (* create memoizing constructors and updaters *)
    include MemoN.Make (struct
        type data = R.t
        type t = R.t thunk

        (** Create non-memoizing constructor and updater for an eager non-self-adjusting value
            (updater not supported by this module; raises {!NonSelfAdjustingValue}). *)
        let memo (type a) (module A : Hashtbl.SeededHashedType with type t = a) f =
            (* non-memoizing constructor *)
            let rec memo x =
                let m = { id=(!eager_id_counter); value=f memo x } in
                incr eager_id_counter;
                m
            in
            (* non-memoizing updater *)
            let update_memo _ _ = raise Exceptions.NonSelfAdjustingValue in
            ( memo, update_memo )
    end)
end
