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
        mutable extent : int;
        mutable array : H.t Weak.t;
    }

    type data = H.t

    let is_array xs = Weak.length xs.array <= limit

    let create n = { extent=0; array=Weak.create (max 1 n) }

    let clear xs = if is_array xs then xs.extent <- 0 else xs.array <- Weak.create (Weak.length xs.array)

    let fold fn xs acc =
        let acc = ref acc in
        if is_array xs then begin
            (* as array, fold while compacting the array *)
            let j = ref 0 in
            for i = 0 to xs.extent - 1 do
                match Weak.get xs.array i with
                    | Some x as x'opt ->
                        acc := fn x !acc;
                        if !j < i then Weak.set xs.array !j x'opt;
                        incr j
                    | None ->
                        ()
            done;
            xs.extent <- !j;
        end else
            (* as hash table, just fold *)
            for i = 0 to Weak.length xs.array - 1 do
                match Weak.get xs.array i with
                    | Some x -> acc := fn x !acc;
                    | None -> ()
            done;
        !acc

    let rec merge xs x =
        let resize () =
            let old_size = Weak.length xs.array in
            let old_array = xs.array in
            xs.extent <- 0;
            xs.array <- Weak.create (old_size * 2);
            if is_array xs then
                (* as array, copy and compact *)
                for i = 0 to old_size - 1 do
                    match Weak.get old_array i with
                        | Some _ as x'opt ->
                            Weak.set xs.array xs.extent x'opt;
                            xs.extent <- xs.extent + 1
                        | None ->
                            ()
                done
            else
                (* as hash table, reinsert *)
                for i = 0 to old_size - 1 do
                    match Weak.get old_array i with
                        | Some x -> ignore (merge xs x)
                        | None -> ()
                done
        in
        if is_array xs then
            (* as array, use fold to find and compact *)
            let x'opt = fold begin fun x' x'opt -> match x'opt with
                | Some _ -> x'opt
                | None -> if H.equal x' x then Some x' else None
            end xs None in
            match x'opt with
                | Some x ->
                    x
                | None ->
                    if xs.extent >= Weak.length xs.array then resize ();
                    if is_array xs then begin
                        Weak.set xs.array xs.extent (Some x);
                        xs.extent <- xs.extent + 1;
                        x
                    end else
                        merge xs x
        else
            (* as hash table, perform a lookup with linear probing *)
            let size = Weak.length xs.array in
            let i = H.hash x mod size in
            let window = max limit (size / 4) in
            let rec find j result =
                if j <= xs.extent then
                    let k = (i + j) mod size in
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
                                let k = (i + j) mod size in
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
