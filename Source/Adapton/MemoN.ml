(** Memoization helper module to create modules for self-adjusting values. *)

(** Input module type of memoization functor {!MemoN.Make}. *)
module type MemoNType = sig
    type data
    type t
    val memo : (module Hashtbl.SeededHashedType with type t = 'a) -> (('a -> t as 'f) -> 'a -> data) -> 'f * (t -> 'a -> unit)
end

(** Output module type of memoization functor {!MemoN.Make}. *)
module type S = sig
    type data
    type t
    val memo :
        (module Hashtbl.SeededHashedType with type t = 'a)
        -> (('a -> t as 'f) -> 'a -> data) -> 'f * (t -> 'a -> unit)
    val memo2 :
        (module Hashtbl.SeededHashedType with type t = 'a)
        -> (module Hashtbl.SeededHashedType with type t = 'b)
        -> (('a -> 'b -> t as 'f) -> 'a -> 'b -> data) -> 'f * (t -> 'a -> 'b -> unit)
    val memo3 :
        (module Hashtbl.SeededHashedType with type t = 'a)
        -> (module Hashtbl.SeededHashedType with type t = 'b)
        -> (module Hashtbl.SeededHashedType with type t = 'c)
        -> (('a -> 'b -> 'c -> t as 'f) -> 'a -> 'b -> 'c -> data) -> 'f * (t -> 'a -> 'b -> 'c -> unit)
    val memo4 :
        (module Hashtbl.SeededHashedType with type t = 'a)
        -> (module Hashtbl.SeededHashedType with type t = 'b)
        -> (module Hashtbl.SeededHashedType with type t = 'c)
        -> (module Hashtbl.SeededHashedType with type t = 'd)
        -> (('a -> 'b -> 'c -> 'd -> t as 'f) -> 'a -> 'b -> 'c -> 'd -> data) -> 'f * (t -> 'a -> 'b -> 'c -> 'd -> unit)
end

(** Functor to make memoizing constructor and updaters of arity of 2 or greater from a memoizing constructor and updater of arity 1. *)
module Make (M : MemoNType) = struct
    (** Create memoizing constructor and updater of arity 1. *)
    let memo = M.memo

    (** Create memoizing constructor and updater of arity 2. *)
    let memo2
            (type a) (module A : Hashtbl.SeededHashedType with type t = a)
            (type b) (module B : Hashtbl.SeededHashedType with type t = b)
            f =
        let memo, update_memo = M.memo (module struct
            type t = A.t * B.t
            let hash seed ( a, b ) = B.hash (A.hash seed a) b
            let equal ( a, b ) ( a', b' ) = A.equal a a' && B.equal b b'
        end) (fun memo ( a, b ) -> f (fun a b -> memo ( a, b )) a b) in
        let memo a b = memo ( a, b ) in
        let update_memo m a b = update_memo m ( a, b ) in
        ( memo, update_memo )

    (** Create memoizing constructor and updater of arity 3. *)
    let memo3
            (type a) (module A : Hashtbl.SeededHashedType with type t = a)
            (type b) (module B : Hashtbl.SeededHashedType with type t = b)
            (type c) (module C : Hashtbl.SeededHashedType with type t = c)
            f =
        let memo, update_memo = M.memo (module struct
            type t = A.t * B.t * C.t
            let hash seed ( a, b, c ) = C.hash (B.hash (A.hash seed a) b) c
            let equal ( a, b, c ) ( a', b', c' ) = A.equal a a' && B.equal b b' && C.equal c c'
        end) (fun memo ( a, b, c ) -> f (fun a b c -> memo ( a, b, c )) a b c) in
        let memo a b c = memo ( a, b, c ) in
        let update_memo m a b c = update_memo m ( a, b, c ) in
        ( memo, update_memo )

    (** Create memoizing constructor and updater of arity 4. *)
    let memo4
            (type a) (module A : Hashtbl.SeededHashedType with type t = a)
            (type b) (module B : Hashtbl.SeededHashedType with type t = b)
            (type c) (module C : Hashtbl.SeededHashedType with type t = c)
            (type d) (module D : Hashtbl.SeededHashedType with type t = d)
            f =
        let memo, update_memo = M.memo (module struct
            type t = A.t * B.t * C.t * D.t
            let hash seed ( a, b, c, d ) = D.hash (C.hash (B.hash (A.hash seed a) b) c) d
            let equal ( a, b, c, d ) ( a', b', c', d' ) = A.equal a a' && B.equal b b' && C.equal c c' && D.equal d d'
        end) (fun memo ( a, b, c, d ) -> f (fun a b c d -> memo ( a, b, c, d )) a b c d) in
        let memo a b c d = memo ( a, b, c, d ) in
        let update_memo m a b c d = update_memo m ( a, b, c, d ) in
        ( memo, update_memo )
end
