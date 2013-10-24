(** Functor that provides a polymorphic API for a self-adjusting module. *)

module Make (M : Signatures.SAType) = struct
    type 'a thunk = 'a M.thunk * (module Signatures.SAType.S with type sa = M.sa and type data = 'a and type t = 'a M.thunk)

    let is_self_adjusting = M.is_self_adjusting

    let is_lazy = M.is_lazy

    let id (type a) (m, (module S) : a thunk) = S.id m

    let hash m = Hashtbl.hash (id m)

    let equal (type a) (m : a thunk) (m' : a thunk) = id m = id m'

    let refresh = M.refresh

    let force (type a) (m, (module S) : a thunk) = S.force m

    let default_hash seed x = Hashtbl.seeded_hash_param 1 100 x

    let default_equal = (==)

    let const (type a) ?(equal=default_equal) x : a thunk =
        let module S = M.Make (struct type t = a let equal = equal end) in
        (S.const x, (module S))

    let update_const (type a)  (m, (module S) : a thunk) x =
        S.update_const m x

    let thunk (type a) ?(equal=default_equal) f : a thunk =
        let module S = M.Make (struct type t = a let equal = equal end) in
        (S.thunk f, (module S))

    let update_thunk (type a) (m, (module S) : a thunk) f =
        S.update_thunk m f

    let memo (type inp) (type a)
                ?(inp_hash=default_hash) ?(inp_equal=default_equal)
                ?(equal=default_equal) f
            : inp -> a thunk =
        let module S = M.Make (struct type t = a let equal = equal end) in
        let memo = S.memo (module struct type t = inp let seed = Random.bits () let hash = inp_hash seed let equal = inp_equal end) f in
        fun a -> (memo a, (module S))

    let memo2 (type inp1) (type inp2) (type a)
                ?(inp1_hash=default_hash) ?(inp1_equal=default_equal)
                ?(inp2_hash=default_hash) ?(inp2_equal=default_equal)
                ?(equal=default_equal) f
            : inp1 -> inp2 -> a thunk =
        let module S = M.Make (struct type t = a let equal = equal end) in
        let memo2 = S.memo2
            (module struct type t = inp1 let seed = Random.bits () let hash = inp1_hash seed let equal = inp1_equal end)
            (module struct type t = inp2 let seed = Random.bits () let hash = inp2_hash seed let equal = inp2_equal end)
            f
        in
        fun a b -> (memo2 a b, (module S))

    let memo3 (type inp1) (type inp2) (type inp3) (type a)
                ?(inp1_hash=default_hash) ?(inp1_equal=default_equal)
                ?(inp2_hash=default_hash) ?(inp2_equal=default_equal)
                ?(inp3_hash=default_hash) ?(inp3_equal=default_equal)
                ?(equal=default_equal) f
            : inp1 -> inp2 -> inp3 -> a thunk =
        let module S = M.Make (struct type t = a let equal = equal end) in
        let memo3 = S.memo3
            (module struct type t = inp1 let seed = Random.bits () let hash = inp1_hash seed let equal = inp1_equal end)
            (module struct type t = inp2 let seed = Random.bits () let hash = inp2_hash seed let equal = inp2_equal end)
            (module struct type t = inp3 let seed = Random.bits () let hash = inp3_hash seed let equal = inp3_equal end)
            f
        in
        fun a b c -> (memo3 a b c, (module S))

    let memo4 (type inp1) (type inp2) (type inp3) (type inp4) (type a)
                ?(inp1_hash=default_hash) ?(inp1_equal=default_equal)
                ?(inp2_hash=default_hash) ?(inp2_equal=default_equal)
                ?(inp3_hash=default_hash) ?(inp3_equal=default_equal)
                ?(inp4_hash=default_hash) ?(inp4_equal=default_equal)
                ?(equal=default_equal) f
            : inp1 -> inp2 -> inp3 -> inp4 -> a thunk =
        let module S = M.Make (struct type t = a let equal = equal end) in
        let memo4 = S.memo4
            (module struct type t = inp1 let seed = Random.bits () let hash = inp1_hash seed let equal = inp1_equal end)
            (module struct type t = inp2 let seed = Random.bits () let hash = inp2_hash seed let equal = inp2_equal end)
            (module struct type t = inp3 let seed = Random.bits () let hash = inp3_hash seed let equal = inp3_equal end)
            (module struct type t = inp4 let seed = Random.bits () let hash = inp4_hash seed let equal = inp4_equal end)
            f
        in
        fun a b c d -> (memo4 a b c d, (module S))

    let tweak_gc = M.tweak_gc
end
