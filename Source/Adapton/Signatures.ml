(** Module types for {i Adapton}. *)

(** {2 Self-adjusting values} *)

(** Output module types of modules for self-adjusting values. *)
module rec SAType : sig
    (** Module type for self-adjusting values for a specific type. *)
    module type S = sig
        type data
        type t
        val hash : t -> int
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
    type 'a thunk
    val hash : 'a thunk -> int
    val equal : 'a thunk -> 'a thunk -> bool
    val force : 'a thunk -> 'a
    val refresh : unit -> unit
    module Make (R : Hashtbl.HashedType) : SAType.S with type data = R.t and type t = R.t thunk
end

(** {2 Self-adjusting lists} *)

(** Output module types of modules for self-adjusting lists. *)
module rec SAListType : sig
    (** Module type for self-adjusting lists for a specific type. *)
    module type S = sig
        type data
        type t
        type t' = [ `Cons of data * t | `Nil ]
        val hash : t -> int
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
        val pop : t -> unit
        val memo_append : (t -> t -> t) * (t -> t -> t -> unit)
        val memo_filter : (data -> bool) -> (t -> t) * (t -> t -> unit)
        val memo_map : (module SAListType.S with type data = 'a and type t = 'b) -> ('a -> data) -> ('b -> t) * (t -> 'b -> unit)
        val memo_scan : (module SAListType.S with type data = 'a and type t = 'b) -> ('a -> data -> data) -> ('b -> data -> t) * (t -> 'b -> data -> unit)
        module PartitionType : SAType.S with type data = t * t
        val split_partition : PartitionType.t -> t * t
        val memo_partition_with_key : (data -> data -> bool) -> (data -> t -> PartitionType.t) * (PartitionType.t -> data -> t -> unit)
        val memo_quicksort : (data -> data -> int) -> (t -> t) * (t -> t -> unit)
    end
end = SAListType

(** Module type for self-adjusting lists. *)
module type SAListType = sig
    type 'a salist
    type 'a salist' = [ `Cons of 'a * 'a salist | `Nil ]
    val hash : 'a salist -> int
    val equal : 'a salist -> 'a salist -> bool
    val force : 'a salist -> 'a salist'
    val refresh : unit -> unit
    val to_list : 'a salist -> 'a list
    val take : 'a salist -> int -> 'a list
    val hd : 'a salist -> 'a
    val tl : 'a salist -> 'a salist
    module type S = SAListType.S
    module Make (R : Hashtbl.HashedType) : S with type data = R.t and type t = R.t salist and type t' = R.t salist'
end
