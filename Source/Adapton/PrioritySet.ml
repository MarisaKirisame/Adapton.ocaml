(** Mutable priority set based on simple binary tree. *)

module type S = sig
    type data
    type t
    exception Empty
    val create : unit -> t
    val add : t -> data -> unit
    val pop : t -> data
end

module Make (O : Set.OrderedType) = struct
    type data = O.t

    type t = t' ref
    and t' = Null | Node of data * t * t

    exception Empty

    let create () = ref Null

    let rec add queue x = match !queue with
        | Node ( value, left, right ) ->
            let dir = O.compare x value in
            if dir == 0 then
                ()
            else if dir < 0 then
                add left x
            else
                add right x
        | Null ->
            queue := Node ( x, ref Null, ref Null )

    let rec pop queue = match !queue with
        | Node ( value, ({ contents=Node _ } as left), _ ) ->
            pop left
        | Node ( value, { contents=Null }, right ) ->
            queue := !right;
            value
        | Null ->
            raise Empty
end
