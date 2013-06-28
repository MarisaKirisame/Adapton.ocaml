(** Memoization helper module to create modules for self-adjusting values. *)

(** Input module type of memoization functor {!MemoN.Make}. *)
module type MemoNType = sig
    type data
    type t
    val memo : (module Hashtbl.SeededHashedType with type t = 'a) -> (('a -> t as 'f) -> 'a -> data) -> 'f
end

(** Output module type of memoization functor {!MemoN.Make}. *)
module type S = sig
    type data
    type t
    val memo :
        (module Hashtbl.SeededHashedType with type t = 'a)
        -> (('a -> t as 'f) -> 'a -> data) -> 'f
    val memo2 :
        (module Hashtbl.SeededHashedType with type t = 'a)
        -> (module Hashtbl.SeededHashedType with type t = 'b)
        -> (('a -> 'b -> t as 'f) -> 'a -> 'b -> data) -> 'f
    val memo3 :
        (module Hashtbl.SeededHashedType with type t = 'a)
        -> (module Hashtbl.SeededHashedType with type t = 'b)
        -> (module Hashtbl.SeededHashedType with type t = 'c)
        -> (('a -> 'b -> 'c -> t as 'f) -> 'a -> 'b -> 'c -> data) -> 'f
    val memo4 :
        (module Hashtbl.SeededHashedType with type t = 'a)
        -> (module Hashtbl.SeededHashedType with type t = 'b)
        -> (module Hashtbl.SeededHashedType with type t = 'c)
        -> (module Hashtbl.SeededHashedType with type t = 'd)
        -> (('a -> 'b -> 'c -> 'd -> t as 'f) -> 'a -> 'b -> 'c -> 'd -> data) -> 'f
end

(** Functor to make memoizing constructor of arity of 2 or greater from a memoizing constructor of arity 1. *)
module Make (M : MemoNType) = struct
    (** Create memoizing constructor of arity 1. *)
    let memo = M.memo

    (** Create memoizing constructor of arity 2. *)
    let memo2
            (type a) (module A : Hashtbl.SeededHashedType with type t = a)
            (type b) (module B : Hashtbl.SeededHashedType with type t = b)
            f =
        let memo = M.memo (module Types.Tuple2 (A) (B)) (fun memo ( a, b ) -> f (fun a b -> memo ( a, b )) a b) in
        fun a b -> memo ( a, b )

    (** Create memoizing constructor of arity 3. *)
    let memo3
            (type a) (module A : Hashtbl.SeededHashedType with type t = a)
            (type b) (module B : Hashtbl.SeededHashedType with type t = b)
            (type c) (module C : Hashtbl.SeededHashedType with type t = c)
            f =
        let memo = M.memo (module Types.Tuple3 (A) (B) (C)) (fun memo ( a, b, c ) -> f (fun a b c -> memo ( a, b, c )) a b c) in
        fun a b c -> memo ( a, b, c )

    (** Create memoizing constructor of arity 4. *)
    let memo4
            (type a) (module A : Hashtbl.SeededHashedType with type t = a)
            (type b) (module B : Hashtbl.SeededHashedType with type t = b)
            (type c) (module C : Hashtbl.SeededHashedType with type t = c)
            (type d) (module D : Hashtbl.SeededHashedType with type t = d)
            f =
        let memo = M.memo (module Types.Tuple4 (A) (B) (C) (D)) (fun memo ( a, b, c, d ) -> f (fun a b c d -> memo ( a, b, c, d )) a b c d) in
        fun a b c d -> memo ( a, b, c, d )
end
