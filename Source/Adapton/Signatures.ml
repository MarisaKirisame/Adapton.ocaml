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
        val create : data -> t
        val update : t -> data -> unit
        val memo : (module Hashtbl.HashedType with type t = 'a) -> (('a -> t) -> 'a -> data) -> ('a -> t)
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
        val create : t' -> t
        val update : t -> t' -> unit
        val memo : (module Hashtbl.HashedType with type t = 'a) -> (('a -> t) -> 'a -> t') -> 'a -> t
        val of_list : data list -> t
        val push : data -> t -> unit
        val pop : t -> unit
        val append : t -> t -> t
        val filter : (data -> bool) -> t -> t
        val map : (module SAListType.S with type data = 'a and type t = 'b) -> ('a -> data) -> 'b -> t
        val scan : (module SAListType.S with type data = 'a and type t = 'b) -> ('a -> data -> data) -> 'b -> data -> t
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
