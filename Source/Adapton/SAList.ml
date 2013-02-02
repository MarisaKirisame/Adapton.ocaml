(** Self-adjusting lists. *)

(** Functor to make self-adjusting lists, given a particular module for self-adjusting values. *)
module Make (M : Signatures.SAType)
        : Signatures.SAListType with type sa = M.sa and type 'a thunk = 'a M.thunk and type 'a salist = [ `Cons of 'a * 'b | `Nil ] M.thunk as 'b = struct

    (** Self-adjusting lists containing ['a]. *)
    type 'a salist = 'a salist' M.thunk

    (** Constructor tags for self-adjusting lists containing ['a]. *)
    and 'a salist' = [ `Cons of 'a * 'a salist | `Nil ]

    (** Types and operations common to lazy self-adjusting lists containing any type. *)
    module T = struct
        (** Abstract type identifying the given module for self-adjusting values used to create this module for self-adjusting lists. *)
        type sa = M.sa

        (** Self-adjusting values from the given module used to create this module for self-adjusting lists. *)
        type 'a thunk = 'a M.thunk

        (** Compute the hash value of a self-adjusting list. *)
        let hash = M.hash

        (** Compute whether two self-adjusting lists are equal. *)
        let equal = M.equal

        (** Return the tag of a self-adjusting list, (re-)computing it if necessary. *)
        let force = M.force

        (** Recompute self-adjusting lists if necessary. *)
        let refresh = M.refresh

        (** Create a regular list from a self-adjusting list. *)
        let to_list xs =
            let rec to_list acc xs = match force xs with
                | `Cons ( x, xs ) -> to_list (x::acc) xs
                | `Nil -> List.rev acc
            in
            to_list [] xs

        (** Create a regular list from the first [k] elements of a self-adjusting list. *)
        let take xs k =
            let rec take acc xs k = if k = 0 then List.rev acc else match force xs with
                | `Cons ( x, xs ) -> take (x::acc) xs (pred k)
                | `Nil -> List.rev acc
            in
            take [] xs k

        (** Return the head of a self-adjusting list. *)
        let hd xs = match force xs with
            | `Cons ( x, _ ) -> x
            | `Nil -> failwith "hd"

        (** Return the tail of a self-adjusting list. *)
        let tl xs = match force xs with
            | `Cons ( _, xs ) -> xs
            | `Nil -> failwith "tl"
    end
    include T

    (** Output module types of {!SAList.MakeBasic}. *)
    module type BasicS = Signatures.SAListType.BasicS

    (** Output module types of {!SAList.Make}. *)
    module type S = Signatures.SAListType.S

    (** Helper functor to make basic list constructors, updaters, and combinators for self-adjusting lists of a specific type. *)
    module MakeBasic (R : Hashtbl.SeededHashedType)
            : BasicS with type sa = sa and type 'a thunk = 'a thunk and type data = R.t and type t = R.t salist and type t' = R.t salist' = struct
        module L = M.Make (struct
            type t = R.t salist'
            let hash seed = function
                | `Cons ( x, xs ) -> hash (R.hash (Hashtbl.seeded_hash seed `Cons) x) xs
                | `Nil -> Hashtbl.seeded_hash seed `Nil
            let equal xs xs' = xs == xs' || match xs, xs' with
                | `Cons ( h, t ), `Cons ( h', t' ) -> R.equal h h' && equal t t'
                | _ -> false
        end)

        (** Self-adjusting values for a specific type, return by certain list operations. *)
        module SAData = M.Make (R)

        (** Value contained by self-adjusting lists for a specific type. *)
        type data = R.t

        (** Self-adjusting lists for a specific type. *)
        type t = L.t

        (** Tags for self-adjusting lists for a specific type. *)
        type t' = L.data

        include T

        (** Create a self-adjusting list from a constant list constructor that does not depend on other self-adjusting values. *)
        let const = L.const

        (** Update a self-adjusting list with a constant list constructor that does not depend on other self-adjusting values. *)
        let update_const = L.update_const

        (** Create a self-adjusting list from a thunk returning a list constructor that may depend on other self-adjusting values. *)
        let thunk = L.thunk

        (** Update a self-adjusting list with a thunk returning a list constructor that may depend on other self-adjusting values. *)
        let update_thunk = L.update_thunk

        include MemoN.Make (struct
            type data = L.data
            type t = L.t

            (** Create memoizing constructor and updater of a self-adjusting list. *)
            let memo = L.memo
        end)

        (** Create a self-adjusting list from a regular list. *)
        let of_list xs =
            let rec of_list acc = function
                | x::xs -> of_list (const (`Cons ( x, acc ))) xs
                | [] -> acc
            in
            of_list (const `Nil) (List.rev xs)

        (** Update the head of a self-adjusting list to push a value in front. *)
        let push x xs = match force xs with
            | `Cons ( x', xs' ) -> update_const xs (`Cons ( x, const (`Cons ( x', xs' )) ))
            | `Nil -> update_const xs (`Cons ( x, const `Nil ))

        (** Update the head of a self-adjusting list to pop a value from the front. *)
        let pop xs = match force xs with
            | `Cons ( x', xs' ) -> update_const xs (force xs'); x'
            | `Nil -> failwith "pop"

        (** Create memoizing constructor and updater that concatenate two self-adjusting lists. *)
        let memo_append =
            memo2 (module L) (module L) begin fun append xs ys -> match force xs with
                | `Cons ( x, xs ) -> `Cons ( x, append xs ys )
                | `Nil -> force ys
            end

        (** Create memoizing constructor and updater that filter a self-adjusting list with a predicate. *)
        let memo_filter f =
            memo (module L) begin fun filter xs -> match force xs with
                | `Cons ( x, xs ) -> if f x then `Cons ( x, filter xs ) else force (filter xs)
                | `Nil -> `Nil
            end

        (** Create memoizing constructor and updater that simultaneously filter and map a self-adjusting list with a predicate/mapping function. *)
        let memo_filter_map (type a) (type b) (module L : Signatures.SAListType.BasicS with type sa = sa and type data = a and type t = b) f =
            memo (module L) begin fun filter xs -> match L.force xs with
                | `Cons ( x, xs ) -> (match f x with Some y -> `Cons ( y, filter xs ) | None -> force (filter xs))
                | `Nil -> `Nil
            end

        (** Create memoizing constructor and updater that map a self-adjusting list with a mapping function. *)
        let memo_map (type a) (type b) (module L : Signatures.SAListType.BasicS with type sa = sa and type data = a and type t = b) f =
            memo (module L) begin fun map xs -> match L.force xs with
                | `Cons ( x, xs ) -> `Cons ( f x, map xs )
                | `Nil -> `Nil
            end

        (** Create memoizing constructor and updater that map a self-adjusting list with a mapping function and key. *)
        let memo_map_with_key
                (type a) (module K : Hashtbl.SeededHashedType with type t = a)
                (type b) (type c) (module L : Signatures.SAListType.BasicS with type sa = sa and type data = b and type t = c)
                f =
            memo2 (module K) (module L) begin fun map k xs -> match L.force xs with
                | `Cons ( x, xs ) -> `Cons ( f k x, map k xs )
                | `Nil -> `Nil
            end

        (** Create memoizing constructor and updater that scan (fold over prefixes of) a self-adjusting list with an scanning function. *)
        let memo_scan (type a) (type b) (module L : Signatures.SAListType.BasicS with type sa = sa and type data = a and type t = b) f =
            memo2 (module L) (module R) begin fun scan xs acc -> match L.force xs with
                | `Cons ( x, xs ) -> let acc = f x acc in `Cons ( acc, scan xs acc )
                | `Nil -> `Nil
            end

        (** Create memoizing constructor and updater that tree-folds a self-adjusting list with an associative fold function. *)
        let memo_tfold f =
            let fold_pairs, _ = L.memo2 (module Types.Int) (module L) begin fun fold_pairs seed xs -> match L.force xs with
                | `Cons ( x', xs' ) as xs'' ->
                    if L.hash seed xs mod 2 == 0 then
                        `Cons ( x', fold_pairs seed xs' )
                    else begin match L.force xs' with
                        | `Cons ( y', ys' ) ->
                            `Cons ( f x' y', fold_pairs seed ys' )
                        | `Nil ->
                            xs''
                    end
                | `Nil ->
                    `Nil
            end in
            let tfold, update_tfold = SAData.memo2 (module Types.Seeds) (module L) begin fun tfold seeds xs -> match L.force xs with
                | `Cons ( x', xs' ) ->
                    begin match L.force xs' with
                        | `Cons _ ->
                            let seed, seeds = Types.Seeds.pop seeds in
                            force (tfold seeds (fold_pairs seed xs))
                        | `Nil ->
                            x'
                    end
                | `Nil ->
                    failwith "tfold"
            end in
            let seeds = Types.Seeds.make () in
            let tfold xs = tfold seeds xs in
            let update_tfold m xs = update_tfold m seeds xs in
            ( tfold, update_tfold )
    end


    (** Functor to make various list constructors, updaters, and combinators for self-adjusting lists of a specific type. *)
    module Make (R : Hashtbl.SeededHashedType)
            : S with type sa = sa and type 'a thunk = 'a thunk and type data = R.t and type t = R.t salist and type t' = R.t salist' = struct
        module L = MakeBasic (R)
        include L

        (** Output type of memo_partition (a lazy pair of lists). *)
        module PartitionType = M.Make (Types.Tuple2 (L) (L))

        (** Helper function to split the output of memo_partition. *)
        let split_partition =
            let split_left, _ = L.memo (module PartitionType) (fun split xs -> L.force (fst (PartitionType.force xs))) in
            let split_right, _ = L.memo (module PartitionType) (fun split xs -> L.force (snd (PartitionType.force xs))) in
            fun xs ->
                ( split_left xs, split_right xs )

        (** Create memoizing constructor and updater to partition a self-adjusting list with a predicate and key. *)
        let memo_partition_with_key (type a) (module K : Hashtbl.SeededHashedType with type t = a) pred =
            PartitionType.memo2 (module K) (module L) begin fun partition k xs -> match force xs with
                | `Cons ( x, xs ) ->
                    let left, right = split_partition (partition k xs) in
                    if pred k x then
                        ( const (`Cons ( x, left )), right )
                    else
                        ( left, const (`Cons ( x, right )) )
                | `Nil ->
                    ( const `Nil, const `Nil )
            end

        (** Create memoizing constructor and updater to quicksort a self-adjusting list with a comparator. *)
        let memo_quicksort cmp =
            let partition, _ = memo_partition_with_key (module R) (fun k x -> cmp x k < 0) in
            let quicksort, update_quicksort = memo2 (module L) (module L) begin fun quicksort xs rest -> match L.force xs with
                | `Cons ( x, xs ) ->
                    let left, right = split_partition (partition x xs) in
                    L.force (quicksort left (const (`Cons ( x, quicksort right rest ))))
                | `Nil ->
                    L.force rest
            end in
            let quicksort xs = quicksort xs (const `Nil) in
            let update_quicksort m xs = update_quicksort m xs (const `Nil) in
            ( quicksort, update_quicksort )

        (**/**) (* internal type of mergesort *)
        module RunType = MakeBasic (L)
        (**/**)

        (** Create memoizing constructor and updater to mergesort a self-adjusting list with a comparator. *)
        let memo_mergesort cmp =
            let lift, _ = RunType.memo_map (module L) (fun x -> const (`Cons ( x, const `Nil ))) in
            let merge, _ = memo2 (module L) (module L) begin fun merge xs ys -> match force xs, force ys with
                | `Cons ( x', xs' ), `Cons ( y', ys' ) ->
                    if cmp x' y' < 0 then
                        `Cons ( x', merge xs' ys )
                    else
                        `Cons ( y', merge xs ys' )
                | xs'', `Nil ->
                    xs''
                | `Nil, ys'' ->
                    ys''
            end in
            let mergesort, _ = RunType.memo_tfold merge in
            memo (module L) begin fun _ xs -> match force xs with
                | `Cons _ -> force (RunType.SAData.force (mergesort (lift xs)))
                | `Nil -> `Nil
            end
    end
end
