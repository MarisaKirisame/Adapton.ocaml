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

    let default_hash seed x = Hashtbl.seeded_hash_param 1 100 seed x

    let default_equal = (==)

    let const (type a) ?(hash=default_hash) ?(equal=default_equal) : a -> a thunk =
        let module S = M.Make (struct type t = a let hash = hash let equal = equal end) in
        fun x -> (S.const x, (module S))

    let update_const (type a)  (m, (module S) : a thunk) x =
        S.update_const m x

    let thunk (type a) ?(hash=default_hash)  ?(equal=default_equal) : (unit -> a) -> a thunk =
        let module S = M.Make (struct type t = a let hash = hash let equal = equal end) in
        fun f -> (S.thunk f, (module S))

    let update_thunk (type a) (m, (module S) : a thunk) f =
        S.update_thunk m f

    let memo (type inp) (type a)
                ?(inp_hash=default_hash) ?(inp_equal=default_equal)
                ?(hash=default_hash) ?(equal=default_equal)
            : ('memo -> inp -> a) -> (inp -> a thunk as 'memo) =
        let module S = M.Make (struct type t = a let hash = hash let equal = equal end) in
        fun f ->
            let f memo = f (fun a -> (memo a, (module S) : a thunk)) in
            let memo = S.memo (module struct type t = inp let hash = inp_hash let equal = inp_equal end) f in
            fun a -> (memo a, (module S))

    let memo2 (type inp1) (type inp2) (type a)
                ?(inp1_hash=default_hash) ?(inp1_equal=default_equal)
                ?(inp2_hash=default_hash) ?(inp2_equal=default_equal)
                ?(hash=default_hash) ?(equal=default_equal)
            : ('memo2 -> inp1 -> inp2 -> a) -> (inp1 -> inp2 -> a thunk as 'memo2) =
        let module S = M.Make (struct type t = a let hash = hash let equal = equal end) in
        fun f ->
            let f memo2 = f (fun a b -> (memo2 a b, (module S) : a thunk)) in
            let memo2 = S.memo2
                (module struct type t = inp1 let hash = inp1_hash let equal = inp1_equal end)
                (module struct type t = inp2 let hash = inp2_hash let equal = inp2_equal end)
                f
            in
            fun a b -> (memo2 a b, (module S))

    let memo3 (type inp1) (type inp2) (type inp3) (type a)
                ?(inp1_hash=default_hash) ?(inp1_equal=default_equal)
                ?(inp2_hash=default_hash) ?(inp2_equal=default_equal)
                ?(inp3_hash=default_hash) ?(inp3_equal=default_equal)
                ?(hash=default_hash) ?(equal=default_equal)
            : ('memo3 -> inp1 -> inp2 -> inp3 -> a) -> (inp1 -> inp2 -> inp3 -> a thunk as 'memo3) =
        let module S = M.Make (struct type t = a let hash = hash let equal = equal end) in
        fun f ->
            let f memo3 = f (fun a b c -> (memo3 a b c, (module S) : a thunk)) in
            let memo3 = S.memo3
                (module struct type t = inp1 let hash = inp1_hash let equal = inp1_equal end)
                (module struct type t = inp2 let hash = inp2_hash let equal = inp2_equal end)
                (module struct type t = inp3 let hash = inp3_hash let equal = inp3_equal end)
                f
            in
            fun a b c -> (memo3 a b c, (module S))

    let memo4 (type inp1) (type inp2) (type inp3) (type inp4) (type a)
                ?(inp1_hash=default_hash) ?(inp1_equal=default_equal)
                ?(inp2_hash=default_hash) ?(inp2_equal=default_equal)
                ?(inp3_hash=default_hash) ?(inp3_equal=default_equal)
                ?(inp4_hash=default_hash) ?(inp4_equal=default_equal)
                ?(hash=default_hash) ?(equal=default_equal)
            : ('memo4 -> inp1 -> inp2 -> inp3 -> inp4 -> a) -> (inp1 -> inp2 -> inp3 -> inp4 -> a thunk as 'memo4) =
        let module S = M.Make (struct type t = a let hash = hash let equal = equal end) in
        fun f ->
            let f memo4 = f (fun a b c d -> (memo4 a b c d, (module S) : a thunk)) in
            let memo4 = S.memo4
                (module struct type t = inp1 let hash = inp1_hash let equal = inp1_equal end)
                (module struct type t = inp2 let hash = inp2_hash let equal = inp2_equal end)
                (module struct type t = inp3 let hash = inp3_hash let equal = inp3_equal end)
                (module struct type t = inp4 let hash = inp4_hash let equal = inp4_equal end)
                f
            in
            fun a b c d -> (memo4 a b c d, (module S))

    let tweak_gc = M.tweak_gc
end
