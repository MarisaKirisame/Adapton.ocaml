(** Self-adjusting array mapped tries. *)

(**/**) (* helper parameters *)
let depth = 7
let bits = LazySparseArray.key_bits
let width = 1 lsl bits
let mask = width - 1
let mask' = lnot mask
let key_bits' = bits * (depth - 1)
let _ = assert (key_bits' < Sys.word_size - 3)
(**/**)

(** Key-width of self-adjusting array mapped tries. *)
let key_bits = bits * depth

(** Size of self-adjusting array mapped tries. *)
let size = 1 lsl key_bits

(** Functor to make self-adjusting array mapped tries, given a particular module for self-adjusting values. *)
module Make (M : Signatures.SAType)
        : Signatures.SAArrayMappedTrieType with
            type sa = M.sa
            and type 'a thunk = 'a M.thunk
        = struct

    (** Self-adjusting array mapped tries containing ['a]. *)
    type 'a saamt = 'a saamt' M.thunk

    (** Constructor tags for self-adjusting array mapped tries containing ['a]. *)
    and 'a saamt' =
        | Branches of 'a saamt' LazySparseArray.t
        | Leaves of 'a LazySparseArray.t
        | Empty

    (** Types and operations common to self-adjusting array mapped tries containing any type. *)
    module T = struct
        (** Abstract type identifying the given module for self-adjusting values used to create this module for self-adjusting array mapped tries. *)
        type sa = M.sa

        (** Self-adjusting values from the given module used to create this module for self-adjusting array mapped tries. *)
        type 'a thunk = 'a M.thunk

        (** True if this module implements self-adjusting array mapped tries. *)
        let is_self_adjusting = M.is_self_adjusting

        (** True if this module implements lazy array mapped tries. *)
        let is_lazy = M.is_lazy

        (** Compute the hash value of a self-adjusting array mapped trie. *)
        let hash = M.hash

        (** Compute whether two self-adjusting array mapped tries are equal. *)
        let equal = M.equal

        (** Recompute self-adjusting array mapped tries if necessary. *)
        let refresh = M.refresh

        (** Return the value at index [k] of a self-adjusting array mapped trie. *)
        let get xs k =
            if k < 0 || k >= size then invalid_arg "index out of bounds";
            let rec get xs s =
                let k = k lsr s land mask in
                match xs with
                    | Branches xs ->
                        begin match LazySparseArray.get xs k with
                            | Some xs -> get xs (s - bits)
                            | None -> None
                        end
                    | Leaves xs ->
                        LazySparseArray.get xs k
                    | Empty ->
                        None
            in
            get (M.force xs) key_bits'
    end
    include T

    (** Output module types of {!SAArrayMappedTrie.Make}. *)
    module type S = Signatures.SAArrayMappedTrieType.S

    (** Helper functor to make a constructor for self-adjusting array mapped tries of a specific type. *)
    module Make (R : Hashtbl.SeededHashedType)
            : S with type sa = sa and type 'a thunk = 'a thunk and type data = R.t and type t = R.t saamt = struct
        module A = M.Make (struct
            type t = R.t saamt'
            let hash seed = function
                | Branches xs -> LazySparseArray.hash (Hashtbl.seeded_hash seed `Branches) xs
                | Leaves xs -> LazySparseArray.hash (Hashtbl.seeded_hash seed `Leaves) xs
                | Empty -> Hashtbl.seeded_hash seed `Empty
            let equal xs xs' = xs == xs' || match xs, xs' with
                | Branches xs, Branches xs' -> LazySparseArray.equal xs xs'
                | Leaves xs, Leaves xs' -> LazySparseArray.equal xs xs'
                | _ -> false
        end)

        (** Value contained by self-adjusting array mapped tries for a specific type. *)
        type data = R.t

        (** Self-adjusting array mapped tries for a specific type. *)
        type t = A.t

        include T

        (** An empty self-adjusting array mapped trie. *)
        let empty = A.const Empty

        (** Create memoizing constructor and updater that adds a binding to an self-adjusting array mapped trie. *)
        let memo_add =
            let add, update_add = A.memo3 (module A) (module Types.Int) (module R) begin fun _ xs k v ->
                let rec add xs s k v =
                    (* if along k, initialize the next branch/leaf node, else lookup the subtrie under the prior AMT *)
                    if s > 0 then
                        Branches begin LazySparseArray.make begin fun d ->
                            if k lsr s land mask == d then
                                Some (add xs (s - bits) k v)
                            else
                                (* perform a partial key lookup for the corresponding subtrie under the prior AMT *)
                                let rec subtrie xs s' = match xs with
                                    | Branches xs ->
                                        if s' == s then
                                            LazySparseArray.get xs d
                                        else
                                            let k = k lsr s' land mask in
                                            begin match LazySparseArray.get xs k with
                                                | Some xs ->
                                                    subtrie xs (s' - bits)
                                                | None ->
                                                    None
                                            end
                                    | Empty ->
                                        None
                                    | Leaves _ ->
                                        assert false
                                in
                                subtrie (M.force xs) key_bits'
                        end end
                    else
                        Leaves begin LazySparseArray.make begin fun d ->
                            if k land mask == d then
                                Some v
                            else
                                get xs (k land mask' lor d)
                        end end
                in
                add xs key_bits' k v
            end in
            let add xs k v =
                if k < 0 || k >= size then invalid_arg "index out of bounds";
                add xs k v
            in
            let update_add m xs k v =
                if k < 0 || k >= size then invalid_arg "index out of bounds";
                update_add m xs k v
            in
            ( add, update_add )
    end
end
