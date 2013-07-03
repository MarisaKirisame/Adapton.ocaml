(** Lazy variant of non-self-adjusting values. *)

(** Types and operations common to lazy non-self-adjusting values containing any type. *)
module T = struct
    (** Abstract type identifying this module for non-self-adjusting values. *)
    type sa

    (** Lazy non-self-adjusting values containing ['a]. *)
    type 'a thunk = {
        id : int;
        thunk : 'a Lazy.t;
    }

    (** This module implements non-self-adjusting values. *)
    let is_self_adjusting = false

    (** This module implements lazy values. *)
    let is_lazy = true

    (**/**) (* internal state *)
    let lazy_id_counter = Types.Counter.make 0
    (**/**)

    (** Compute the hash value of a non-self-adjusting value. *)
    let hash seed m = Hashtbl.seeded_hash seed m.id

    (** Compute whether two non-self-adjusting values are equal. *)
    let equal = (==)

    (** Recompute non-self-adjusting values if necessary (not supported by this module; raises {!NonSelfAdjustingValue}). *)
    let refresh () = raise Exceptions.NonSelfAdjustingValue

    (** Return the value contained by a non-self-adjusting value, computing it if necessary. *)
    let force { thunk=lazy value; _ } = value
end
include T


(** Functor to make constructors for lazy non-self-adjusting values of a specific type. *)
module Make (R : Signatures.EqualsType)
        : Signatures.SAType.S with type sa = sa and type 'a thunk = 'a thunk and type data = R.t and type t = R.t thunk = struct
    include T

    (** Value contained by lazy non-self-adjusting values for a specific type. *)
    type data = R.t

    (** Lazy non-self-adjusting values for a specific type. *)
    type t = R.t thunk

    (** Create a lazy non-self-adjusting value from a constant value. *)
    let const x = { id=Types.Counter.next lazy_id_counter; thunk=lazy x }

    (** Update a lazy non-self-adjusting value with a constant value (not supported by this module; raises {!NonSelfAdjustingValue}). *)
    let update_const _ _ = raise Exceptions.NonSelfAdjustingValue

    (** Create a lazy non-self-adjusting value from a thunk. *)
    let thunk f = { id=Types.Counter.next lazy_id_counter; thunk=Lazy.from_fun f }

    (** Update a lazy non-self-adjusting value with a thunk (not supported by this module; raises {!NonSelfAdjustingValue}). *)
    let update_thunk _ _ = raise Exceptions.NonSelfAdjustingValue

    (* create memoizing constructors *)
    include MemoN.Make (struct
        type data = R.t
        type t = R.t thunk

        (** Create non-memoizing constructor for a lazy non-self-adjusting value. *)
        let memo (type a) (module A : Hashtbl.SeededHashedType with type t = a) f =
            (* non-memoizing constructor *)
            let rec memo x = { id=Types.Counter.next lazy_id_counter; thunk=lazy (f memo x) } in
            memo
    end)
end

(** Tweak GC for this module. *)
let tweak_gc () = ()
