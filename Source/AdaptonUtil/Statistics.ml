(** Adapton performance statistics and measurement function. *)

module Counts = struct
    let update = ref 0
    let dirty = ref 0
    let clean = ref 0
    let evaluate = ref 0
end

let word_size = Sys.word_size / 8

let word_bytes words = float_of_int (word_size * words)

let word_megabytes words = word_bytes words /. 1048576.

let get_time = Unix.gettimeofday

let get_heap_stack () = Gc.(let s = quick_stat () in ( s.heap_words, s.stack_size ))

(**/**) (* helper values/functions *)
let heap_stacks = ref []
let top_stack = ref 0
let _ = Gc.create_alarm begin fun () ->
    let heap, stack = Gc.(let s = quick_stat () in ( s.heap_words, s.stack_size )) in
    List.iter (fun ( h, s ) -> h := max heap !h; s := max stack !s) !heap_stacks;
    top_stack := max stack !top_stack
end
(**/**)

let get_top_heap_stack () = ( Gc.((quick_stat ()).top_heap_words), !top_stack )

type t = {
    time : float; (** Elapsed time in seconds. *)
    heap : float; (** Max heap delta in bytes. *)
    stack : float; (** Max stack delta in bytes. *)
    update : float; (** Thunks updated. *)
    evaluate : float; (** Thunks re-evaluated. *)
    dirty : float; (** For {!Adapton}, dependencies to be checked; for {!EagerTotalOrder}, thunks scheduled for re-evaluation. *)
    clean : float; (** For {!Adapton}, dependencies checked clean; for {!EagerTotalOrder}, thunks unscheduled for re-evaluation (due to invalidation). *)
}

type u = {
    time' : float;
    heap' : int;
    stack' : int;
    update' : int;
    evaluate' : int;
    dirty' : int;
    clean' : int;
}

let zero = { time'=0.; heap'=0; stack'=0; update'=0; evaluate'=0; dirty'=0; clean'=0 }

let add s s' = {
    time'=s.time' +. s'.time';
    heap'=s.heap' + s'.heap';
    stack'=s.stack' + s'.stack';
    update'=s.update' + s'.update';
    evaluate'=s.evaluate' + s'.evaluate';
    dirty'=s.dirty' + s'.dirty';
    clean'=s.clean' +s'.clean';
}

let measure f =
    let heap' = ref 0 in
    let stack' = ref 0 in
    let update = !Counts.update in
    let dirty = !Counts.dirty in
    let clean = !Counts.clean in
    let evaluate = !Counts.evaluate in
    let old_heap_stacks = !heap_stacks in
    heap_stacks := ( heap', stack' )::old_heap_stacks;
    let heap, stack = get_heap_stack () in
    heap' := max heap !heap';
    stack' := max stack !stack';
    let time = get_time () in
    let x = f () in
    let time' = get_time () -. time in
    heap_stacks := old_heap_stacks;
    let measurement = {
        time';
        heap'=(!heap' - heap);
        stack'=(!stack' - stack);
        update'=(!Counts.update - update);
        evaluate'=(!Counts.evaluate - evaluate);
        dirty'=(!Counts.dirty - dirty);
        clean'=(!Counts.clean - clean);
    } in
    ( x, measurement )

let finish s k =
    let k = float_of_int k in {
        time=s.time' /. k;
        heap=word_bytes s.heap' /. k;
        stack=word_bytes s.stack' /. k;
        update=float_of_int s.update' /. k;
        evaluate=float_of_int s.evaluate' /. k;
        dirty=float_of_int s.dirty' /. k;
        clean=float_of_int s.clean' /. k;
    }
