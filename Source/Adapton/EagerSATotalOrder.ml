(** Eager variant of self-adjusting values based on a total-order maintenance data structure.

    Implementation based on:
        Umut Acar, Guy Blelloch, Matthias Blume, Robert Harper, and Kanat Tangwongsan. "A Library for Self-Adjusting
        Computation". Electron. Notes Theor. Comput. Sci. 148, 2 (March 2006), 127-154.
        http://dx.doi.org/10.1016/j.entcs.2005.11.043
    supporting memoization and change propagation, but not adaptive memoization.
 *)


(**/**) (* helper functions *)
let nop () = ()
(**/**)


(**/**) (* total-order maintenance data structure based on:
    Dietz, Paul and Sleator, Daniel. "Two algorithms for maintaining order in a list." In Proceedings of the
        Nineteenth Annual ACM Symposium on Theory of Computing (STOC '87). http://dx.doi.org/10.1145/28395.28434
    Bender, Michael, et al. "Two simplified algorithms for maintaining order in a list." In Proceedings of the 10th
        Annual European Symposium on Algorithms (ESA '02). http://dx.doi.org/10.1007/3-540-45749-6_17
and implementation based on https://github.com/matthewhammer/ceal/blob/4b933a8/src/lib/runtime/totalorder.c
*)
module TotalOrder : sig
    type parent
    type t
    val null : t
    val create : unit -> t
    val is_valid : t -> bool
    val compare : t -> t -> int
    val add_next : t -> t
    val splice : t -> t -> unit
    val set_invalidator : t -> (unit -> unit) -> unit
    val reset_invalidator : t -> unit
end = struct
    let threshold = 1.4 (* rebalancing region threshold (inverse density) *)
    let label_bits = Sys.word_size - 2 (* use only the positive range *)
    let max_label = 1 lsl (label_bits - 1) (* use only half the positive range to avoid needing to handle overflow *)
    let gap_size = max_label / label_bits (* gap between elements after rebalancing *)
    let end_label = max_label - gap_size

    (** Top layer bidirectional linked-list of the total-order data structure that provides coarse-grained ordering. *)
    type parent = { (* 5 words *)
        mutable parent_label : int;
        mutable parent_prev : parent;
        mutable parent_next : parent;
        mutable front : t;
        mutable back : t;
    }
    (** Bottom layer bidirectional linked-list of the total-order data structure that provides fine-grained ordering. *)
    and t = { (* 5 words (not including parent and closure of invalidator) *)
        mutable label : int;
        mutable parent : parent;
        mutable next : t;
        mutable prev : t;
        mutable invalidator : unit -> unit;
    }

    (**/**) (* sentinel values *)
    let rec null_parent = {
        parent_label=(-1);
        parent_next=null_parent;
        parent_prev=null_parent;
        front=null;
        back=null;
    } and null = {
        label=(-1);
        parent=null_parent;
        prev=null;
        next=null;
        invalidator=nop;
    }
    (**/**)

    (** Create a new total order and return its initial element. *)
    let create () =
        let rec ts = {
            label=0;
            parent={
                parent_label=0;
                parent_next=null_parent;
                parent_prev=null_parent;
                front=ts;
                back=ts;
            };
            prev=null;
            next=null;
            invalidator=nop;
        } in
        ts

    (** Return if a total-order element is valid (i.e., has not been removed). *)
    let is_valid ts = ts.label >= 0 && ts.parent.parent_label >= 0

    (**/**) (* helper functions *)
    let neg = (lor) min_int
    let pos = (land) (lnot min_int)
    let invalidate ts =
        ts.label <- neg ts.label;
        ts.invalidator ();
        (* help GC mark phase by cutting the object graph *)
        ts.invalidator <- nop;
        ts.prev <- null;
        ts.next <- null
    let invalidate_parent parent =
        parent.parent_label <- neg parent.parent_label;
        let rec invalidate_ts ts = if ts != null then begin
            let next = ts.next in
            invalidate ts;
            invalidate_ts next
        end in
        invalidate_ts parent.front;
        (* help GC mark phase by cutting the object graph *)
        parent.parent_prev <- null_parent;
        parent.parent_next <- null_parent;
        parent.front <- null;
        parent.back <- null
    (**/**)

    (** Compare two total-order elements. *)
    let compare ts ts' =
        let p = Pervasives.compare (pos ts.parent.parent_label) (pos ts'.parent.parent_label) in
        if p != 0 then p else Pervasives.compare (pos ts.label) (pos ts'.label)

    (** Add a new total-order element after the given element. *)
    let add_next ts =
        if not (is_valid ts) then
            failwith "add_next"
        else
            let parent = ts.parent in
            let ts' = if ts.next != null then begin
                let next = ts.next in
                let ts' = { label=(ts.label + next.label) lsr 1; parent; prev=ts; next; invalidator=nop } in
                next.prev <- ts';
                ts.next <- ts';
                ts'
            end else begin
                let ts' = { label=(ts.label + max_label) lsr 1; parent; prev=ts; next=null; invalidator=nop } in
                ts.next <- ts';
                ts'
            end in

            if ts.label == ts'.label then begin
                (* redistribute all elements under a parent such that they are spaced by [gap_size],
                   adding new parents as necessary to accomodate the redistribution *)
                let rec rebalance label parent prev next =
                    if label < end_label then begin
                        next.label <- label;
                        next.parent <- parent;
                        if next.next != null then
                            rebalance (label + gap_size) parent next next.next
                        else
                            parent.back <- next
                    end else begin
                        (* add a new parent *)
                        parent.back <- prev;
                        prev.next <- null;
                        next.prev <- null;
                        let parent' = if parent.parent_next != null_parent then begin
                            let parent_next = parent.parent_next in
                            let parent' = {
                                parent_label=(parent.parent_label + parent_next.parent_label) lsr 1;
                                parent_prev=parent_next.parent_prev;
                                parent_next;
                                front=next;
                                back=next;
                            } in
                            parent_next.parent_prev <- parent';
                            parent.parent_next <- parent';
                            parent'
                        end else begin
                            let parent' = {
                                parent_label=(parent.parent_label + max_label) lsr 1;
                                parent_prev=parent;
                                parent_next=null_parent;
                                front=next;
                                back=next;
                            } in
                            parent.parent_next <- parent';
                            parent'
                        end in

                        if parent.parent_label == parent'.parent_label then begin
                            (* identify a region around the parent that satisfies the rebalancing threshold *)
                            let rec expand lower upper count mask tau =
                                let lo_label = lower.parent_label land (lnot mask) in
                                let hi_label = lower.parent_label lor mask in
                                let rec expand_lower lower count = if lower.parent_prev != null_parent then
                                    let lower' = lower.parent_prev in
                                    if lower'.parent_label >= lo_label then
                                        expand_lower lower' (count + 1)
                                    else
                                        ( lower, count )
                                else begin
                                    if lower.parent_label != lo_label then
                                        lower.parent_label <- lo_label;
                                    ( lower, count )
                                end in
                                let rec expand_upper upper count = if upper.parent_next != null_parent then
                                    let upper' = upper.parent_next in
                                    if upper'.parent_label <= hi_label then
                                        expand_upper upper' (count + 1)
                                    else
                                        ( upper, count )
                                else begin
                                    if upper.parent_label != hi_label then
                                        upper.parent_label <- hi_label;
                                    ( upper, count )
                                end in
                                let lower, count = expand_lower lower count in
                                let upper, count = expand_upper upper count in
                                if tau < float_of_int count /. float_of_int (mask + 1) then
                                    expand lower upper count ((mask lsl 1) lor 1) (tau /. threshold)
                                else
                                    ( lower, upper, lo_label, (mask + 1) / count )
                            in
                            let lower, upper, label, delta = expand parent parent' 2 1 (1. /. threshold) in

                            (* evenly redistribute the parents in the region *)
                            let rec rebalance parent label =
                                parent.parent_label <- label;
                                if parent != upper && parent.parent_next != null_parent then
                                    rebalance parent.parent_next (label + delta)
                            in
                            rebalance lower label
                        end;
                        rebalance 0 parent' next next
                    end
                in
                rebalance 0 parent parent.front parent.front
            end;
            ts'

    (** Splice two elements [ts] and [ts'] in a total-order such that [ts] is immediately followed by [ts'],
        removing all elements between them. *)
    let splice ts ts' =
        if compare ts ts' > 0 then
            failwith "splice"
        else begin
            if ts.parent != ts'.parent then begin
                if ts.parent.parent_next == null_parent then failwith "splice";

                (* invalidate all parents between ts and ts' *)
                let rec invalidate_next parent =
                    if parent == ts'.parent then
                        ()
                    else if parent != null_parent then begin
                        let next = parent.parent_next in
                        invalidate_parent parent;
                        invalidate_next next
                    end else
                        failwith "splice"
                in
                invalidate_next ts.parent.parent_next;
                ts'.parent.parent_prev <- ts.parent;
                ts.parent.parent_next <- ts'.parent;
                ts'.parent.front <- ts';
                ts.parent.back <- ts;

                (* invalidate all elements before ts' under the same parent *)
                let rec invalidate_prev ts = if ts != null then begin
                    let prev = ts.prev in
                    invalidate ts;
                    invalidate_prev prev
                end in
                invalidate_prev ts'.prev;
                ts'.prev <- null;

                (* invalidate all elements after ts under the same parent *)
                let rec invalidate_next ts = if ts != null then begin
                    let next = ts.next in
                    invalidate ts;
                    invalidate_next next
                end in
                invalidate_next ts.next;
                ts.next <- null
            end else if ts != ts' then begin
                if ts.next == null then failwith "splice";

                (* invalidate all elements between ts and ts' *)
                let rec invalidate_next ts =
                    if ts == ts' then
                        ()
                    else if ts != null then begin
                        let next = ts.next in
                        invalidate ts;
                        invalidate_next next
                    end else
                        failwith "splice"
                in
                invalidate_next ts.next;
                ts'.prev <- ts;
                ts.next <- ts'
            end
        end

    (** Set an invalidator function for the given total-order element. *)
    let set_invalidator ts invalidator =
        ts.invalidator <- invalidator

    (** Reset the invalidator function for the given total-order element. *)
    let reset_invalidator ts =
        ts.invalidator <- nop
end
(**/**)


(** Types and operations common to eager self-adjusting values containing any type. *)
module T = struct
    (** Abstract type identifying this module for self-adjusting values. *)
    type sa

    (** Eager self-adjusting values containing ['a]. *)
    type 'a thunk = { (* 2 + 17 = 19 words *)
        mutable value : 'a;
        meta : meta;
    }
    (**/**) (* auxiliary types *)
    and meta = { (* 7 + 5 + 5 = 17 words (not including closures of evaluate and unmemo as well as WeakDyn.t) *)
        id : int;
        mutable evaluate : unit -> unit;
        mutable unmemo : unit -> unit;
        mutable start_timestamp : TotalOrder.t;
        mutable end_timestamp : TotalOrder.t;
        mutable dependencies : meta list;
        dependents : meta WeakDyn.t; (* doesn't have to be a set since it is cleared and dependents are immediately re-evaluated and re-added if updated *)
    }
    (**/**)


    (** This module implements self-adjusting values. *)
    let is_self_adjusting = true

    (** This module implements eager values. *)
    let is_lazy = false


    (**/**) (* internal state and helper functions *)

    (* priority set based on simple binary tree: usually, the size is quite small, but duplicate insertion occur frequently *)
    module PriorityQueue = struct
        type queue = Empty | Node of meta * queue * queue

        let empty = Empty

        let rec insert queue meta = match queue with
            | Empty ->
                Node ( meta, Empty, Empty )
            | Node ( meta', left, right ) ->
                if meta == meta' then
                    queue
                else if TotalOrder.compare meta.start_timestamp meta'.start_timestamp <= 0 then
                    Node ( meta', insert left meta, right )
                else
                    Node ( meta', left, insert right meta )

        exception Queue_is_empty

        let rec extract = function
            | Node ( meta, (Node _ as left), right ) ->
                let meta', left = extract left in
                ( meta', Node ( meta, left, right ) )
            | Node ( meta, Empty, right ) ->
                ( meta, right )
            | Empty ->
                raise Queue_is_empty
    end

    let eager_id_counter = ref 0
    let eager_stack = ref []
    let eager_queue = ref PriorityQueue.empty
    let eager_start = TotalOrder.create ()
    let eager_now = ref eager_start
    let eager_finger = ref eager_start

    let add_timestamp () =
        let timestamp = TotalOrder.add_next !eager_now in
        eager_now := timestamp;
        timestamp

    let rec dequeue () =
        let meta, queue = PriorityQueue.extract !eager_queue in
        eager_queue := queue;
        if TotalOrder.is_valid meta.start_timestamp then begin
            meta
        end else
            dequeue ()

    let enqueue meta = if TotalOrder.is_valid meta.start_timestamp then
        eager_queue := PriorityQueue.insert !eager_queue meta

    let enqueue_dependents dependents =
        eager_queue := WeakDyn.fold (fun d q -> if TotalOrder.is_valid d.start_timestamp then PriorityQueue.insert q d else q) dependents !eager_queue;
        WeakDyn.clear dependents
    (**/**)

    (** Compute the hash value of a self-adjusting value. *)
    let hash seed m = Hashtbl.seeded_hash seed m.meta.id

    (** Compute whether two self-adjusting values are equal. *)
    let equal = (==)

    (** Recompute self-adjusting values if necessary. *)
    let refresh () =
        let last_now = !eager_now in
        try
            let rec refresh () =
                let meta = dequeue () in
                eager_now := meta.start_timestamp;
                eager_finger := meta.end_timestamp;
                meta.evaluate ();
                TotalOrder.splice !eager_now meta.end_timestamp;
                refresh ()
            in
            refresh ()
        with PriorityQueue.Queue_is_empty ->
            eager_now := last_now;
            eager_finger := eager_start

    (** Return the value contained by a self-adjusting value, computing it if necessary. *)
    let force m =
        (* add dependency to caller *)
        begin match !eager_stack with
            | dependent::_ ->
                WeakDyn.add m.meta.dependents dependent;
                dependent.dependencies <- m.meta::dependent.dependencies
            | [] ->
                ()
        end;
        m.value
end
include T


(** Functor to make constructors and updaters for eager self-adjusting values of a specific type. *)
module Make (R : Signatures.EqualsType)
        : Signatures.SAType.S with type sa = sa and type 'a thunk = 'a thunk and type data = R.t and type t = R.t thunk = struct
    include T

    (** Value contained by eager self-adjusting values for a specific type. *)
    type data = R.t

    (** Eager self-adjusting values for a specific type. *)
    type t = R.t thunk

    (**/**) (* helper functions *)
    let invalidator meta () =
        (* help GC mark phase by cutting the object graph *)
        meta.evaluate <- nop;
        meta.unmemo <- nop;
        meta.start_timestamp <- TotalOrder.null;
        meta.end_timestamp <- TotalOrder.null;
        meta.dependencies <- [];
        WeakDyn.clear meta.dependents
    (**/**)

    (** Create an eager self-adjusting value from a constant value. *)
    let const x =
        let m = {
            value=x;
            meta={
                id=(!eager_id_counter);
                evaluate=nop;
                unmemo=nop;
                start_timestamp=TotalOrder.null;
                end_timestamp=TotalOrder.null;
                dependencies=[];
                dependents=WeakDyn.create 0;
            };
        } in
        incr eager_id_counter;
        m

    (** Update an eager self-adjusting value with a constant value. *)
    let update_const m x =
        m.meta.unmemo ();
        m.meta.unmemo <- nop;
        m.meta.evaluate <- nop;
        TotalOrder.reset_invalidator m.meta.start_timestamp;
        m.meta.start_timestamp <- TotalOrder.null;
        m.meta.end_timestamp <- TotalOrder.null;
        m.meta.dependencies <- [];
        if not (R.equal m.value x) then begin
            m.value <- x;
            enqueue_dependents m.meta.dependents
        end

    (**/**) (* helper function to evaluate a thunk *)
    let evaluate_meta meta f =
        meta.dependencies <- [];
        eager_stack := meta::!eager_stack;
        let value = try
            f ()
        with exn ->
            eager_stack := List.tl !eager_stack;
            raise exn
        in
        eager_stack := List.tl !eager_stack;
        value

    let evaluate_actual m f =
        let x = evaluate_meta m.meta f in
        if not (R.equal m.value x) then begin
            m.value <- x;
            enqueue_dependents m.meta.dependents
        end
    (**/**)

    (** Create an eager self-adjusting value from a thunk. *)
    let thunk f =
        let meta = {
            id=(!eager_id_counter);
            evaluate=nop;
            unmemo=nop;
            start_timestamp=add_timestamp ();
            end_timestamp=TotalOrder.null;
            dependencies=[];
            dependents=WeakDyn.create 0;
        } in
        TotalOrder.set_invalidator meta.start_timestamp (invalidator meta);
        incr eager_id_counter;
        let m = { value=evaluate_meta meta f; meta } in
        meta.end_timestamp <- add_timestamp ();
        meta.evaluate <- (fun () -> evaluate_actual m f);
        m

    (** Update an eager self-adjusting value with a thunk. *)
    let update_thunk m f =
        m.meta.evaluate <- nop;
        m.meta.unmemo ();
        m.meta.unmemo <- nop;
        TotalOrder.reset_invalidator m.meta.start_timestamp;
        m.meta.start_timestamp <- add_timestamp ();
        m.meta.end_timestamp <- TotalOrder.null;
        TotalOrder.set_invalidator m.meta.start_timestamp (invalidator m.meta);
        let x = evaluate_meta m.meta f in
        if not (R.equal m.value x) then begin
            m.value <- x;
            enqueue_dependents m.meta.dependents
        end;
        m.meta.end_timestamp <- add_timestamp ();
        m.meta.evaluate <- (fun () -> evaluate_actual m f)

    (* create memoizing constructors *)
    include MemoN.Make (struct
        type data = R.t
        type t = R.t thunk

        (** Create memoizing constructor for an eager self-adjusting value. *)
        let memo (type a) (module A : Hashtbl.SeededHashedType with type t = a) f =
            let module Binding = struct
                type t = { key : A.t; mutable value : R.t thunk option }
                let seed = Random.bits ()
                let hash a = A.hash seed a.key
                let equal a a' = A.equal a.key a'.key
            end in
            let module Memotable = Weak.Make (Binding) in
            let memotable = Memotable.create 0 in

            (* memoizing constructor *)
            let rec memo x =
                let binding = Memotable.merge memotable Binding.({ key=x; value=None }) in
                match binding.Binding.value with
                    | Some m when TotalOrder.is_valid m.meta.start_timestamp
                            && TotalOrder.compare m.meta.start_timestamp !eager_now > 0
                            && TotalOrder.compare m.meta.end_timestamp !eager_finger < 0 ->
                        TotalOrder.splice !eager_now m.meta.start_timestamp;
                        eager_now := m.meta.end_timestamp;
                        m
                    | _ ->
                        let m = thunk (fun () -> f memo x) in
                        m.meta.unmemo <- (fun () -> Memotable.remove memotable binding);
                        binding.Binding.value <- Some m;
                        m
            in

            memo
    end)
end

(** Tweak GC for this module. *)
let tweak_gc () =
    let open Gc in
    let control = get () in
    set { control with
        minor_heap_size = max control.minor_heap_size (2 * 1024 * 1024);
        major_heap_increment = max control.minor_heap_size (4 * 1024 * 1024);
    }
