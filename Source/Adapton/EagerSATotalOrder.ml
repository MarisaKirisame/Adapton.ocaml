(** Eager variant of self-adjusting values based on a total-order maintenance data structure.

    Implementation based on:
        Umut Acar, Guy Blelloch, Matthias Blume, Robert Harper, and Kanat Tangwongsan. "A Library for Self-Adjusting
        Computation". Electron. Notes Theor. Comput. Sci. 148, 2 (March 2006), 127-154.
        http://dx.doi.org/10.1016/j.entcs.2005.11.043
    supporting memoization and change propagation, but not adaptive memoization.
 *)

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
    val create : unit -> t
    val is_valid : t -> bool
    val remove : t -> unit
    val compare : t -> t -> int
    val add_next : t -> t
    val splice : t -> t -> unit
end = struct
    let threshold = 1.4 (* rebalancing region threshold (inverse density) *)
    let label_bits = Sys.word_size - 2 (* use only the positive range *)
    let max_label = 1 lsl (label_bits - 1) (* use only half the positive range to avoid needing to handle overflow *)
    let gap_size = max_label / label_bits (* gap between elements after rebalancing *)
    let end_label = max_label - gap_size

    (** Top layer bidirectional linked-list of the total-order data structure that provides coarse-grained ordering. *)
    type parent = {
        mutable parent_label : int;
        mutable parent_prev : parent option;
        mutable parent_next : parent option;
        mutable front : t;
        mutable back : t;
    }
    (** Bottom layer bidirectional linked-list of the total-order data structure that provides fine-grained ordering. *)
    and t = {
        mutable label : int;
        mutable parent : parent;
        mutable next : t option;
        mutable prev : t option;
    }

    (** Create a new total order and return its initial element. *)
    let create () =
        let rec ts = {
            label=0;
            parent={
                parent_label=0;
                parent_next=None;
                parent_prev=None;
                front=ts;
                back=ts;
            };
            prev=None;
            next=None;
        } in
        ts

    (** Return if a total-order element is valid (i.e., has not been removed). *)
    let is_valid ts = ts.label >= 0 && ts.parent.parent_label >= 0

    (**/**) (* helper functions *)
    let neg = (lor) min_int
    let pos = (land) (lnot min_int)
    (**/**)

    (** Remove an element from a total-order. *)
    let remove ts = if is_valid ts then begin
        ts.label <- neg ts.label;
        match ts.prev, ts.next with
            | (Some ts as prev), (Some ts' as next) ->
                ts'.prev <- prev;
                ts.next <- next
            | Some ts, None ->
                ts.parent.back <- ts;
                ts.next <- None
            | None, Some ts ->
                ts.parent.front <- ts;
                ts.prev <- None
            | None, None ->
                let parent = ts.parent in
                parent.parent_label <- neg parent.parent_label;
                begin match parent.parent_prev, parent.parent_next with
                    | (Some parent as prev), (Some parent' as next) ->
                        parent'.parent_prev <- prev;
                        parent.parent_next <- next
                    | Some parent, None ->
                        parent.parent_next <- None
                    | None, Some parent ->
                        parent.parent_prev <- None
                    | None, None ->
                        ()
                end
    end

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
            let ts' = match ts.next with
                | Some ts'' as next ->
                    let ts' = { label=(ts.label + ts''.label) lsr 1; parent; prev=ts''.prev; next } in
                    let ts'opt = Some ts' in
                    ts''.prev <- ts'opt;
                    ts.next <- ts'opt;
                    ts'
                | None ->
                    let ts' = { label=(ts.label + max_label) lsr 1; parent; prev=Some ts; next=None } in
                    ts.next <- Some ts';
                    ts'
            in

            if ts.label == ts'.label then begin
                (* redistribute all elements under a parent such that they are spaced by [gap_size],
                   adding new parents as necessary to accomodate the redistribution *)
                let rec rebalance label parent prev next =
                    if label < end_label then begin
                        next.label <- label;
                        next.parent <- parent;
                        match next.next with
                            | Some ts'' -> rebalance (label + gap_size) parent next ts''
                            | None -> parent.back <- next
                    end else begin
                        (* add a new parent *)
                        parent.back <- prev;
                        prev.next <- None;
                        next.prev <- None;
                        let parent' = match parent.parent_next with
                            | Some parent'' as parent_next->
                                let parent' = {
                                    parent_label=(parent.parent_label + parent''.parent_label) lsr 1;
                                    parent_prev=parent''.parent_prev;
                                    parent_next;
                                    front=next;
                                    back=next;
                                } in
                                let parent'opt = Some parent' in
                                parent''.parent_prev <- parent'opt;
                                parent.parent_next <- parent'opt;
                                parent'
                            | None ->
                                let parent' = {
                                    parent_label=(parent.parent_label + max_label) lsr 1;
                                    parent_prev=Some parent;
                                    parent_next=None;
                                    front=next;
                                    back=next;
                                } in
                                parent.parent_next <- Some parent';
                                parent'
                        in

                        if parent.parent_label == parent'.parent_label then begin
                            (* identify a region around the parent that satisfies the rebalancing threshold *)
                            let rec expand lower upper count mask tau =
                                let lo_label = lower.parent_label land (lnot mask) in
                                let hi_label = lower.parent_label lor mask in
                                let rec expand_lower lower count = match lower.parent_prev with
                                    | Some lower' ->
                                        if lower'.parent_label >= lo_label then
                                            expand_lower lower' (count + 1)
                                        else
                                            ( lower, count )
                                    | None ->
                                        if lower.parent_label != lo_label then
                                            lower.parent_label <- lo_label;
                                        ( lower, count )
                                in
                                let rec expand_upper upper count = match upper.parent_next with
                                    | Some upper' ->
                                        if upper'.parent_label <= hi_label then
                                            expand_upper upper' (count + 1)
                                        else
                                            ( upper, count )
                                    | None ->
                                        if upper.parent_label != hi_label then
                                            upper.parent_label <- hi_label;
                                        ( upper, count )
                                in
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
                                if parent != upper then
                                    match parent.parent_next with
                                        | Some parent' -> rebalance parent' (label + delta)
                                        | None -> ()
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
            if ts.parent != ts'.parent then match ts.parent.parent_next with
                | Some parent ->
                    (* invalidate all parents between ts and ts' *)
                    let rec invalidate_next parent = match parent.parent_next with
                        | Some parent' when parent' == ts'.parent ->
                            parent
                        | Some parent' ->
                            parent'.parent_label <- neg parent'.parent_label;
                            invalidate_next parent'
                        | _ ->
                            let x = failwith "splice" in x
                    in
                    let parent' = invalidate_next ts.parent in
                    ts'.parent.parent_prev <- parent.parent_prev;
                    ts.parent.parent_next <- parent'.parent_next;
                    ts'.parent.front <- ts';
                    ts.parent.back <- ts;

                    (* invalidate all elements before ts' under the same parent *)
                    let rec invalidate_prev ts = match ts.prev with
                        | Some ts ->
                            ts.label <- neg ts.label;
                            invalidate_prev ts
                        | None ->
                            ()
                    in
                    invalidate_prev ts';
                    ts'.prev <- None;

                    (* invalidate all elements after ts under the same parent *)
                    let rec invalidate_next ts = match ts.next with
                        | Some ts ->
                            ts.label <- neg ts.label;
                            invalidate_next ts
                        | None ->
                            ()
                    in
                    invalidate_next ts;
                    ts.next <- None
                | None ->
                    failwith "splice"
            else if ts != ts' then match ts.next with
                | Some ts'' ->
                    (* invalidate all elements between ts and ts' *)
                    let rec invalidate_next ts = match ts.next with
                        | Some ts'' when ts'' == ts' ->
                            ts
                        | Some ts'' ->
                            ts''.label <- neg ts''.label;
                            invalidate_next ts''
                        | _ ->
                            failwith "splice"
                    in
                    let ts''' = invalidate_next ts in
                    ts'.prev <- ts''.prev;
                    ts.next <- ts'''.next
                | None ->
                    failwith "splice"
        end
end
(**/**)


(** Types and operations common to eager self-adjusting values containing any type. *)
module T = struct
    (** Abstract type identifying this module for self-adjusting values. *)
    type sa

    module rec TT : sig
        (** Eager self-adjusting values containing ['a]. *)
        type 'a thunk = {
            mutable value : 'a;
            meta : meta;
        }
        (**/**) (* auxiliary types *)
        and meta = {
            id : int;
            mutable evaluate : unit -> unit;
            mutable unmemo : unit -> unit;
            start_timestamp : TotalOrder.t;
            mutable end_timestamp : TotalOrder.t;
            mutable dependencies : meta list;
            dependents : Dependents.t;
        }
        (**/**)
    end = TT
    and Dependents : Weak.S with type data = TT.meta = Weak.Make (struct
        type t = TT.meta
        let hash d = Hashtbl.hash d.TT.id
        let equal = (==)
    end)
    include TT


    (** This module implements self-adjusting values. *)
    let is_self_adjusting = true

    (** This module implements eager values. *)
    let is_lazy = false


    (**/**) (* internal state and helper functions *)

    (* priority queue adapted from http://caml.inria.fr/pub/docs/manual-ocaml/manual004.html *)
    module PriorityQueue = struct
        type queue = Empty | Node of meta * queue * queue

        let empty = Empty

        let rec insert queue meta = match queue with
            | Empty ->
                Node ( meta, Empty, Empty )
            | Node ( meta', left, right ) as node ->
                if meta == meta' then
                    node
                else if TotalOrder.compare meta.start_timestamp meta'.start_timestamp <= 0 then
                    Node ( meta, insert left meta', right )
                else
                    Node ( meta', insert left meta, right )

        exception Queue_is_empty

        let rec remove_top = function
            | Empty ->
                raise Queue_is_empty
            | Node ( _, left, Empty ) ->
                left
            | Node ( _, Empty, right ) ->
                right
            | Node ( _, (Node ( meta, _, _ ) as left), (Node ( meta', _, _ ) as right) ) ->
                if TotalOrder.compare meta.start_timestamp meta'.start_timestamp <= 0 then
                    Node ( meta, remove_top left, right )
                else
                    Node ( meta', left, remove_top right )

        let extract = function
            | Empty -> raise Queue_is_empty
            | Node ( meta, _, _ ) as queue -> ( meta, remove_top queue )
    end

    let eager_id_counter = ref 0
    let eager_stack = ref []
    let eager_queue = ref PriorityQueue.empty
    let eager_start = TotalOrder.create ()
    let eager_now = ref eager_start
    let eager_finger = ref eager_start
    let eager_remove = ref []

    let finalise meta =
        eager_remove := meta.start_timestamp::meta.end_timestamp::!eager_remove

    let flush_remove () =
        if !eager_remove != [] then begin
            let remove = !eager_remove in
            eager_remove := [];
            List.iter TotalOrder.remove remove
        end

    let set_eager_now timestamp =
        flush_remove ();
        eager_now := timestamp

    let add_timestamp () =
        let timestamp = TotalOrder.add_next !eager_now in
        set_eager_now timestamp;
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
        eager_queue := Dependents.fold (fun d q -> if TotalOrder.is_valid d.start_timestamp then PriorityQueue.insert q d else q) dependents !eager_queue;
        Dependents.clear dependents
    (**/**)

    (** Compute the hash value of a self-adjusting value. *)
    let hash seed m = Hashtbl.seeded_hash seed m.meta.id

    (** Compute whether two self-adjusting values are equal. *)
    let equal = (==)

    (** Recompute self-adjusting values if necessary. *)
    let refresh () = try
        let rec refresh () =
            let meta = dequeue () in
            let last_finger = !eager_finger in
            set_eager_now meta.start_timestamp;
            eager_finger := meta.end_timestamp;
            meta.evaluate ();
            TotalOrder.splice !eager_now meta.end_timestamp;
            eager_finger := last_finger;
            refresh ()
        in
        refresh ()
    with PriorityQueue.Queue_is_empty ->
        ()

    (** Return the value contained by a self-adjusting value, computing it if necessary. *)
    let force m =
        (* add dependency to caller *)
        begin match !eager_stack with
            | dependent::_ ->
                ignore (Dependents.merge m.meta.dependents dependent);
                dependent.dependencies <- m.meta::dependent.dependencies
            | [] ->
                ()
        end;
        m.value
end
include T


(** Functor to make constructors and updaters for eager self-adjusting values of a specific type. *)
module Make (R : Hashtbl.SeededHashedType)
        : Signatures.SAType.S with type sa = sa and type 'a thunk = 'a thunk and type data = R.t and type t = R.t thunk = struct
    include T

    (** Value contained by eager self-adjusting values for a specific type. *)
    type data = R.t

    (** Eager self-adjusting values for a specific type. *)
    type t = R.t thunk

    (**/**) (* helper functions *)
    let nop () = ()
    (**/**)

    (** Create an eager self-adjusting value from a constant value. *)
    let const x =
        let start_timestamp = add_timestamp () in
        let end_timestamp = add_timestamp () in
        let m = {
            value=x;
            meta={
                id=(!eager_id_counter);
                evaluate=nop;
                unmemo=nop;
                start_timestamp;
                end_timestamp;
                dependencies=[];
                dependents=Dependents.create 0;
            };
        } in
        Gc.finalise finalise m.meta;
        incr eager_id_counter;
        m

    (** Update an eager self-adjusting value with a constant value. *)
    let update_const m x =
        m.meta.unmemo ();
        m.meta.unmemo <- nop;
        m.meta.evaluate <- nop;
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
            end_timestamp=eager_start;
            dependencies=[];
            dependents=Dependents.create 0;
        } in
        Gc.finalise finalise meta;
        incr eager_id_counter;
        let m = { value=evaluate_meta meta f; meta } in
        meta.end_timestamp <- add_timestamp ();
        meta.evaluate <- (fun () -> evaluate_actual m f);
        m

    (** Update an eager self-adjusting value with a thunk. *)
    let update_thunk m f =
        m.meta.unmemo ();
        m.meta.unmemo <- nop;
        m.meta.evaluate <- (fun () -> evaluate_actual m f);
        enqueue m.meta

    (* create memoizing constructors and updaters *)
    include MemoN.Make (struct
        type data = R.t
        type t = R.t thunk

        (** Create memoizing constructor and updater for an eager self-adjusting value. *)
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
                        set_eager_now m.meta.end_timestamp;
                        m
                    | _ ->
                        let m = thunk (fun () -> f memo x) in
                        m.meta.unmemo <- (fun () -> Memotable.remove memotable binding);
                        binding.Binding.value <- Some m;
                        m
            in

            (* memoizing updater *)
            let update_memo m x =
                update_thunk m (fun () -> f memo x);
                let binding = Memotable.merge memotable Binding.({ key=x; value=None }) in
                if binding.Binding.value == None then begin
                    m.meta.unmemo <- (fun () -> Memotable.remove memotable binding);
                    binding.Binding.value <- Some m
                end
            in

            ( memo, update_memo )
    end)
end
