(** Eager incremental computation based on self-adjusting computation with a total-order maintenance data structure.

    Implementation based on:
        Umut Acar, Guy Blelloch, Matthias Blume, Robert Harper, and Kanat Tangwongsan. "A Library for Self-Adjusting
        Computation". Electron. Notes Theor. Comput. Sci. 148, 2 (March 2006), 127-154.
        http://dx.doi.org/10.1016/j.entcs.2005.11.043
    supporting memoization and change propagation, but not adaptive memoization.
 *)

open AdaptonInternal
open AdaptonUtil

(** Types and operations common to EagerTotalOrder thunks containing any type. *)
module T = struct
    (** Abstract type identifying this module. *)
    type atype

    (** EagerTotalOrder thunks containing ['a]. *)
    type 'a thunk = { (* 3 + 16 = 19 words *)
        id : int;
        mutable value : 'a;
        meta : meta;
    }
    (**/**) (* auxiliary types *)
    and meta = { (* 5 + 5 + 5 = 15 words (not including closures of evaluate and unmemo as well as WeakBucket.t) *)
        mutable evaluate : unit -> unit;
        mutable unmemo : unit -> unit;
        mutable start_timestamp : TotalOrder.t; (* for const thunks, {start,end}_timestamp == TotalOrder.null and evaluate == nop *)
        mutable end_timestamp : TotalOrder.t; (* while evaluating non-const thunk, end_timestamp == TotalOrder.null *)
        dependents : meta WeakBucket.t;
        (* dependents doesn't have to be a set since it is cleared and dependents are immediately re-evaluated and re-added if updated;
            also, start_timestamp invalidators should provide strong references to dependencies to prevent the GC from breaking the dependents graph *)
    }
    (**/**)


    (** This module implements incremental thunks. *)
    let is_incremental = true

    (** This module implements eager values. *)
    let is_lazy = false


    (**/**) (* internal state and helper functions *)

    (* use a priority set because, although the size is usually quite small, duplicate insertions occur frequently *)
    module PriorityQueue = PrioritySet.Make (struct
        type t = meta
        let compare meta meta' = TotalOrder.compare meta.start_timestamp meta'.start_timestamp
    end)

    let eager_id_counter = Types.Counter.make 0
    let eager_stack = ref []
    let eager_queue = PriorityQueue.create ()
    let eager_start = TotalOrder.create ()
    let eager_now = ref eager_start
    let eager_finger = ref eager_start

    let add_timestamp () =
        let timestamp = TotalOrder.add_next !eager_now in
        eager_now := timestamp;
        timestamp

    let unqueue meta =
        if PriorityQueue.remove eager_queue meta then
            incr Statistics.Counts.clean

    let dequeue () = PriorityQueue.pop eager_queue

    let enqueue_dependents dependents =
        ignore begin WeakBucket.fold begin fun d () ->
            if TotalOrder.is_valid d.start_timestamp then
                if PriorityQueue.add eager_queue d then
                    incr Statistics.Counts.dirty
        end dependents () end;
        WeakBucket.clear dependents
    (**/**)


    (** Return the id of an EagerTotalOrder thunk. *)
    let id m = m.id

    (** Compute the hash value of an EagerTotalOrder thunk. *)
    let hash seed m = Hashtbl.seeded_hash seed m.id

    (** Compute whether two EagerTotalOrder thunks are equal. *)
    let equal = (==)

    (** Recompute EagerTotalOrder thunks if necessary. *)
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
        with PriorityQueue.Empty ->
            eager_now := last_now;
            eager_finger := eager_start

    (** Return the value contained by an EagerTotalOrder thunk, computing it if necessary. *)
    let force m =
        (* add dependency to caller *)
        begin match !eager_stack with
            | dependent::_ -> WeakBucket.add m.meta.dependents dependent
            | [] -> ()
        end;
        m.value
end
include T


(** Functor to make constructors and updaters for EagerTotalOrder thunks of a specific type. *)
module Make (R : Hashtbl.SeededHashedType)
        : Signatures.AType.S with type atype = atype and type 'a thunk = 'a thunk and type data = R.t and type t = R.t thunk = struct
    include T

    (** Value contained by EagerTotalOrder thunks for a specific type. *)
    type data = R.t

    (** EagerTotalOrder thunks for a specific type. *)
    type t = R.t thunk

    (** Module representing type [data]. *)
    module Data = R

    (**/**) (* helper functions *)
    let nop () = ()
    let invalidator meta ts =
        (* help GC mark phase by cutting the object graph *)
        (* no need to call unmemo since the memo entry will be replaced when it sees start_timestamp is invalid;
            also, no need to replace {start,end}_timestamp with null since they are already cut by TotalOrder during invalidation *)
        meta.unmemo <- nop;
        meta.evaluate <- nop;
        unqueue meta;
        WeakBucket.clear meta.dependents
    let update m x = if not (R.equal m.value x) then begin
        m.value <- x;
        enqueue_dependents m.meta.dependents
    end
    (**/**)

    (** Create an EagerTotalOrder thunk from a constant value. *)
    let const x =
        incr Statistics.Counts.create;
        let m = {
            id=Types.Counter.next eager_id_counter;
            value=x;
            meta={
                evaluate=nop;
                unmemo=nop;
                start_timestamp=TotalOrder.null;
                end_timestamp=TotalOrder.null;
                dependents=WeakBucket.create 0;
            };
        } in
        m

    (** Update an EagerTotalOrder thunk with a constant value. *)
    let update_const m x =
        incr Statistics.Counts.update;
        if m.meta.start_timestamp != TotalOrder.null then begin
            (* no need to call unmemo since the memo entry will be replaced when it sees start_timestamp is invalid *)
            m.meta.unmemo <- nop;
            m.meta.evaluate <- nop;
            unqueue m.meta;
            TotalOrder.reset_invalidator m.meta.start_timestamp;
            TotalOrder.splice ~inclusive:true m.meta.start_timestamp m.meta.end_timestamp;
            m.meta.start_timestamp <- TotalOrder.null;
            m.meta.end_timestamp <- TotalOrder.null
        end;
        update m x

    (**/**) (* helper function to evaluate a thunk *)
    let evaluate_meta meta f =
        incr Statistics.Counts.evaluate;
        eager_stack := meta::!eager_stack;
        let value = try
            f ()
        with exn ->
            eager_stack := List.tl !eager_stack;
            raise exn
        in
        eager_stack := List.tl !eager_stack;
        value

    let make_evaluate m f = fun () -> update m (evaluate_meta m.meta f)
    (**/**)

    (** Create an EagerTotalOrder thunk from a function that may depend on other EagerTotalOrder thunks. *)
    let thunk f =
        incr Statistics.Counts.create;
        let meta = {
            evaluate=nop;
            unmemo=nop;
            start_timestamp=add_timestamp ();
            end_timestamp=TotalOrder.null;
            dependents=WeakBucket.create 0;
        } in
        let m = { id=Types.Counter.next eager_id_counter; value=evaluate_meta meta f; meta } in
        meta.end_timestamp <- add_timestamp ();
        TotalOrder.set_invalidator meta.start_timestamp (invalidator meta);
        meta.evaluate <- make_evaluate m f;
        m

    (** Update an EagerTotalOrder thunk with a function that may depend on other EagerTotalOrder thunks. *)
    let update_thunk m f =
        incr Statistics.Counts.update;
        if m.meta.start_timestamp != TotalOrder.null then begin
            m.meta.unmemo ();
            m.meta.unmemo <- nop;
            m.meta.evaluate <- nop;
            unqueue m.meta;
            TotalOrder.reset_invalidator m.meta.start_timestamp;
            TotalOrder.splice ~inclusive:true m.meta.start_timestamp m.meta.end_timestamp;
            m.meta.end_timestamp <- TotalOrder.null
        end;
        m.meta.start_timestamp <- add_timestamp ();
        let evaluate = make_evaluate m f in
        evaluate ();
        m.meta.end_timestamp <- add_timestamp ();
        TotalOrder.set_invalidator m.meta.start_timestamp (invalidator m.meta);
        m.meta.evaluate <- evaluate

    (* create memoizing constructors *)
    include MemoN.Make (struct
        type data = R.t
        type t = R.t thunk

        (** Create memoizing constructor for an EagerTotalOrder thunk. *)
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
                        incr Statistics.Counts.hit;
                        TotalOrder.splice !eager_now m.meta.start_timestamp;
                        eager_now := m.meta.end_timestamp;
                        m
                    | _ ->
                        (* note that m.meta.unmemo indirectly holds a reference to binding (via unmemo's closure);
                            this prevents the GC from collecting binding from memotable until m itself is collected *)
                        incr Statistics.Counts.create;
                        incr Statistics.Counts.miss;
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
