(** Lazy variant of non-self-adjusting values. *)

(** Types and operations common to lazy non-self-adjusting values containing any type. *)
module T = struct
    (** Lazy non-self-adjusting values containing ['a]. *)
    type 'a thunk = {
        id : int;
        thunk : 'a Lazy.t;
    }

    (**/**) (* internal state *)
    let lazy_id_counter = ref 0
    (**/**)

    (** Compute the hash value of a non-self-adjusting value. *)
    let hash m = m.id

    (** Compute whether two non-self-adjusting values are equal. *)
    let equal = (==)

    (** Recompute non-self-adjusting values if necessary (unused by this module; raises Failure). *)
    let refresh () = raise Exceptions.NonSelfAdjustingValue

    (** Return the value contained by a non-self-adjusting value, computing it if necessary. *)
    let force { thunk=lazy value; _ } = value
end
include T


(** Functor to make a constructor, a mutator, and a non-memoizing constructor for lazy non-self-adjusting values of a specific type. *)
module Make (R : Hashtbl.HashedType) : Signatures.SAType.S with type data = R.t and type t = R.t thunk = struct
    include T

    (** Value contained by lazy non-self-adjusting values for a specific type. *)
    type data = R.t

    (** Lazy non-self-adjusting values for a specific type. *)
    type t = R.t thunk

    (** Create a lazy non-self-adjusting value containing a value. *)
    let create x =
        let m = { id=(!lazy_id_counter); thunk=lazy x } in
        incr lazy_id_counter;
        m

    (** Update a lazy non-self-adjusting value with a value. *)
    let update _ _ = raise Exceptions.NonSelfAdjustingValue

    (** Create a non-memoizing constructor for a lazy non-self-adjusting value. *)
    let memo (type a) (module A : Hashtbl.HashedType with type t = a) f =
        let rec memo x =
            let m = { id=(!lazy_id_counter); thunk=lazy (f memo x) } in
            incr lazy_id_counter;
            m
        in
        memo
end
