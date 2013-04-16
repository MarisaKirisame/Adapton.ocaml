(** Eager variant of self-adjusting values by eagerly forcing {!Adapton.LazySABidi} thunks as they are created.

    Note that change propagation does not occur during {!Adapton.LazySABidi.refresh} (unlike
    {!Adapton.EagerSATotalOrder}), but during {!Adapton.LazySABidi.force} (like {!Adapton.LazySABidi}), at which
    point, not only will the given self-adjusting value be updated, but all self-adjusting values reachable from the
    the given self-adjusting values will be updated (unlike {!Adapton.LazySABidi}).
*)

(** Types and operations common to eager self-adjusting values containing any type. *)
module T = struct
    (** Abstract type identifying this module for self-adjusting values. *)
    type sa

    (** Eager self-adjusting values containing ['a]. *)
    type 'a thunk = 'a LazySABidi.thunk

    (** Compute the hash value of a self-adjusting value. *)
    let hash = LazySABidi.hash

    (** Compute whether two self-adjusting values are equal. *)
    let equal = LazySABidi.equal

    (** Recompute self-adjusting values if necessary (this is a no-op as in {!Adapton.LazySABidi}). *)
    let refresh = LazySABidi.refresh (* should be a no-op, but just in case *)

    (** Return the value contained by a self-adjusting value, (re-)computing it if necessary. *)
    let force = LazySABidi.force
end
include T

(** Functor to make constructors and updaters for lazy self-adjusting values of a specific type. *)
module Make (R : Hashtbl.SeededHashedType)
        : Signatures.SAType.S with type sa = sa and type 'a thunk = 'a thunk and type data = R.t and type t = R.t thunk = struct
    include T

    module LazySABidiR = LazySABidi.Make (R)

    (** Value contained by eager self-adjusting values for a specific type. *)
    type data = R.t

    (** Eager self-adjusting values for a specific type. *)
    type t = R.t thunk

    (**/**) (* helper functions *)
    let make_eager f x =
        let m = f x in
        ignore (force m);
        m
    (**/**)

    (** Create an eager self-adjusting value from a constant value. *)
    let const = LazySABidiR.const

    (** Update an eager self-adjusting value with a constant value. *)
    let update_const = LazySABidiR.update_const

    (** Create an eager self-adjusting value from a thunk. *)
    let thunk = make_eager LazySABidiR.thunk

    (** Update an eager self-adjusting value with a thunk. *)
    let update_thunk = LazySABidiR.update_thunk

    (* create memoizing constructors and updaters *)
    include MemoN.Make (struct
        type data = R.t
        type t = R.t thunk

        (** Create memoizing constructor and updater for an eager self-adjusting value. *)
        let memo (type a) (module A : Hashtbl.SeededHashedType with type t = a) f =
            let f memo x = f (make_eager memo) x in
            let memo, update_memo = LazySABidiR.memo (module A) f in
            ( make_eager memo, update_memo )
    end)
end
