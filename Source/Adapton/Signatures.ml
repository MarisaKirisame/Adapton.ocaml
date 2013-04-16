(** Module types for {i Adapton}. *)

(** {2 Self-adjusting values} *)

(** Output module types of modules for self-adjusting values. *)
module rec SAType : sig
    (** Module type for self-adjusting values for a specific type. *)
    module type S = sig
        type sa
        type 'a thunk
        type data
        type t
        val is_self_adjusting : bool
        val is_lazy : bool
        val hash : int -> t -> int
        val equal : t -> t -> bool
        val force : t -> data
        val refresh : unit -> unit
        val const : data -> t
        val update_const : t -> data -> unit
        val thunk : (unit -> data) -> t
        val update_thunk : t -> (unit -> data) -> unit
        include MemoN.S with type data := data and type t := t
    end
end = SAType

(** Module type for self-adjusting values. *)
module type SAType = sig
    type sa
    type 'a thunk
    val is_self_adjusting : bool
    val is_lazy : bool
    val hash : int -> 'a thunk -> int
    val equal : 'a thunk -> 'a thunk -> bool
    val force : 'a thunk -> 'a
    val refresh : unit -> unit
    module Make (R : Hashtbl.SeededHashedType) : SAType.S with type sa = sa and type 'a thunk = 'a thunk and type data = R.t and type t = R.t thunk
end

(** {2 Self-adjusting lists} *)

(** Output module types of modules for self-adjusting lists. *)
module rec SAListType : sig
    (** Module type for self-adjusting lists for a specific type containing basic types and operations. *)
    module type BasicS = sig
        type sa
        type 'a thunk
        type data
        module SAData : SAType.S with type sa = sa and type 'a thunk = 'a thunk and type data = data and type t = data thunk
        type t
        type t' = [ `Cons of data * t | `Nil ]
        val is_self_adjusting : bool
        val is_lazy : bool
        val hash : int -> t -> int
        val equal : t -> t -> bool
        val force : t -> t'
        val refresh : unit -> unit
        val to_list : t -> data list
        val take : t -> int -> data list
        val hd : t -> data
        val tl : t -> t
        val const : t' -> t
        val update_const : t -> t' -> unit
        val thunk : (unit -> t') -> t
        val update_thunk : t -> (unit -> t') -> unit
        include MemoN.S with type data := t' and type t := t
        val of_list : data list -> t
        val push : data -> t -> unit
        val pop : t -> data
        val memo_append : (t -> t -> t) * (t -> t -> t -> unit)
        val memo_filter : (data -> bool) -> (t -> t) * (t -> t -> unit)
        val memo_filter_map
            : (module SAListType.BasicS with type sa = sa and type data = 'a and type t = 'b)
                -> ('a -> data option) -> ('b -> t) * (t -> 'b -> unit)
        val memo_map
            : (module SAListType.BasicS with type sa = sa and type data = 'a and type t = 'b)
                -> ('a -> data) -> ('b -> t) * (t -> 'b -> unit)
        val memo_map_with_key
            : (module Hashtbl.SeededHashedType with type t = 'a)
                -> (module SAListType.BasicS with type sa = sa and type data = 'b and type t = 'c)
                -> ('a -> 'b -> data) -> ('a -> 'c -> t) * (t -> 'a -> 'c -> unit)
        val memo_scan
            : (module SAListType.BasicS with type sa = sa and type data = 'a and type t = 'b)
                -> ('a -> data -> data) -> ('b -> data -> t) * (t -> 'b -> data -> unit)
        val memo_tfold : (data -> data -> data) -> (t -> SAData.t) * (SAData.t -> t -> unit)
    end

    (** Module type for self-adjusting lists for a specific type. *)
    module type S = sig
        include BasicS
        val memo_quicksort : (data -> data -> int) -> (t -> t) * (t -> t -> unit)
        val memo_mergesort : (data -> data -> int) -> (t -> t) * (t -> t -> unit)
    end
end = SAListType

(** Module type for self-adjusting lists. *)
module type SAListType = sig
    type sa
    type 'a thunk
    type 'a salist
    type 'a salist' = [ `Cons of 'a * 'a salist | `Nil ]
    val is_self_adjusting : bool
    val is_lazy : bool
    val hash : int -> 'a salist -> int
    val equal : 'a salist -> 'a salist -> bool
    val force : 'a salist -> 'a salist'
    val refresh : unit -> unit
    val to_list : 'a salist -> 'a list
    val take : 'a salist -> int -> 'a list
    val hd : 'a salist -> 'a
    val tl : 'a salist -> 'a salist
    module type BasicS = SAListType.BasicS
    module type S = SAListType.S
    module MakeBasic (R : Hashtbl.SeededHashedType)
        : BasicS with type sa = sa and type 'a thunk = 'a thunk and type data = R.t and type t = R.t salist and type t' = R.t salist'
    module Make (R : Hashtbl.SeededHashedType)
        : S with type sa = sa and type 'a thunk = 'a thunk and type data = R.t and type t = R.t salist and type t' = R.t salist'
end
