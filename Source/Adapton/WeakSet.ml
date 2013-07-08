(** Weak set implemented as an array for smaller sizes, and an open-addressing hash table for larger sizes. *)

module type S = sig
    type data
    type t
    val create : int -> t
    val clear : t -> unit
    val merge : t -> data -> data
    val fold : (data -> 'a -> 'a) -> t -> 'a -> 'a
end

module Make (H : Hashtbl.HashedType) = struct
    let limit = 8

    type t = {
        mutable size : int;
        mutable extent : int;
        mutable array : H.t Weak.t;
    }

    type data = H.t

    let create n = { size=0; extent=0; array=Weak.create (max 1 n) }

    let clear xs = if xs.size <= limit then xs.size <- 0 else xs.array <- Weak.create xs.size

    let fold fn xs acc =
        let acc = ref acc in
        if xs.size <= limit then begin
            (* as array, fold while compacting the array *)
            let j = ref 0 in
            for i = 0 to xs.size - 1 do
                match Weak.get xs.array i with
                    | Some x as x'opt ->
                        acc := fn x !acc;
                        if !j < i then Weak.set xs.array !j x'opt;
                        incr j
                    | None ->
                        ()
            done;
            xs.size <- !j;
        end else
            (* as hash table, just fold *)
            for i = 0 to xs.size - 1 do
                match Weak.get xs.array i with
                    | Some x -> acc := fn x !acc;
                    | None -> ()
            done;
        !acc

    let rec merge xs x =
        let resize () =
            let old_size = xs.size in
            let old_array = xs.array in
            xs.array <- Weak.create (old_size * 2);
            if Weak.length xs.array <= limit then begin
                (* as array, copy and compact *)
                let j = ref 0 in
                for i = 0 to old_size - 1 do
                    match Weak.get old_array i with
                        | Some _ as x'opt -> Weak.set xs.array !j x'opt; incr j
                        | None -> ()
                done;
                xs.size <- !j
            end else begin
                (* as hash table, reinsert *)
                xs.size <- Weak.length xs.array;
                for i = 0 to old_size - 1 do
                    match Weak.get old_array i with
                        | Some x -> ignore (merge xs x)
                        | None -> ()
                done
            end
        in
        if xs.size <= limit then
            (* as array, use fold to find and compact *)
            let x'opt = fold begin fun x' x'opt -> match x'opt with
                | Some _ -> x'opt
                | None -> if H.equal x' x then Some x' else None
            end xs None in
            match x'opt with
                | Some x ->
                    x
                | None ->
                    if Weak.length xs.array <= xs.size then resize ();
                    if xs.size < limit then begin
                        Weak.set xs.array xs.size (Some x);
                        xs.size <- xs.size + 1;
                        x
                    end else
                        merge xs x
        else
            (* as hash table, perform a lookup with linear probing *)
            let i = H.hash x mod xs.size in
            let window = max limit (xs.size / 4) in
            let rec find j result =
                if j <= xs.extent then
                    let k = (i + j) mod xs.size in
                    match Weak.get xs.array k with
                        | Some x' when H.equal x x' -> x'
                        | Some _ -> find (j + 1) result
                        | None -> find (j + 1) (if result == None then Some ( j, k ) else result)
                else match result with
                    | Some ( j, k ) ->
                        xs.extent <- max j xs.extent; Weak.set xs.array k (Some x); x
                    | None ->
                        let rec find j =
                            if j < window then
                                let k = (i + j) mod xs.size in
                                match Weak.get xs.array k with
                                    | Some _ -> find (j + 1)
                                    | None -> xs.extent <- max j xs.extent; Weak.set xs.array k (Some x); x
                            else begin
                                resize (); merge xs x
                            end
                        in
                        find j
            in
            find 0 None
end
