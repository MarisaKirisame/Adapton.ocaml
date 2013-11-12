(** Functor that provides a basic polymorphic API for a self-adjusting module. *)

module Make (M : Signatures.SAType) : sig
    type 'a aref
    val aref : 'a -> 'a aref
    val get : 'a aref -> 'a
    val set : 'a aref -> 'a -> unit

    type 'a athunk
    val force : 'a athunk -> 'a
    val thunk : (unit -> 'a) -> 'a athunk
    val memo : ('fn -> 'arg -> 'a) -> ('arg -> 'a athunk as 'fn)
end = struct
    module P = PolySA.Make (M)

    type 'a aref = 'a P.thunk
    let aref x = P.const x
    let get m = P.force m
    let set m x = P.update_const m x

    type 'a athunk = 'a P.thunk
    let force m = P.force m
    let thunk f = P.thunk f
    let memo f = P.memo f
end
