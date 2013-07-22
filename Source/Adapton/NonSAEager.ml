(** Eager variant of non-self-adjusting values. *)

(** Types and operations common to eager non-self-adjusting values containing any type. *)
module T = struct
    (** Abstract type identifying this module for non-self-adjusting values. *)
    type sa

    (** Eager non-self-adjusting values containing ['a]. *)
    type 'a thunk = {
        id : int;
        value : 'a;
    }

    (** This module implements non-self-adjusting values. *)
    let is_self_adjusting = false

    (** This module implements eager values. *)
    let is_lazy = false

    (**/**) (* internal state *)
    let eager_id_counter = Types.Counter.make 0
    (**/**)

    (** Return the id of a non-self-adjusting value. *)
    let id m = m.id

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


(** Functor to make constructors for eager non-self-adjusting values of a specific type. *)
module Make (R : Signatures.EqualsType)
        : Signatures.SAType.S with type sa = sa and type 'a thunk = 'a thunk and type data = R.t and type t = R.t thunk = struct
    include T

    (** Value contained by eager non-self-adjusting values for a specific type. *)
    type data = R.t

    (** Eager non-self-adjusting values for a specific type. *)
    type t = R.t thunk

    (** Create an eager non-self-adjusting value from a constant value. *)
    let const x = { id=Types.Counter.next eager_id_counter; value=x }

    (** Update an eager non-self-adjusting value with a constant value (not supported by this module; raises {!NonSelfAdjustingValue}). *)
    let update_const _ _ = raise Exceptions.NonSelfAdjustingValue

    (** Create an eager non-self-adjusting value from a thunk. *)
    let thunk f = incr Statistics.Counts.evaluate; { id=Types.Counter.next eager_id_counter; value=f () }

    (** Update an eager non-self-adjusting value with a thunk (not supported by this module; raises {!NonSelfAdjustingValue}). *)
    let update_thunk _ _ = raise Exceptions.NonSelfAdjustingValue

    (* create memoizing constructors *)
    include MemoN.Make (struct
        type data = R.t
        type t = R.t thunk

        (** Create non-memoizing constructor for an eager non-self-adjusting value. *)
        let memo (type a) (module A : Hashtbl.SeededHashedType with type t = a) f =
            (* non-memoizing constructor *)
            let rec memo x = incr Statistics.Counts.evaluate; { id=Types.Counter.next eager_id_counter; value=f memo x } in
            memo
    end)
end

(** Tweak GC for this module. *)
let tweak_gc () = ()
