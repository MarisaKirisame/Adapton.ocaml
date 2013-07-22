(** Adapton performance statistics and measurement function. *)

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
}

type u = {
    time' : float;
    heap' : int;
    stack' : int;
}

let zero = { time'=0.; heap'=0; stack'=0 }

let add s s' = { time'=s.time' +. s'.time'; heap'=s.heap' + s'.heap'; stack'=s.stack' + s'.stack' }

let measure f =
    let heap' = ref 0 in
    let stack' = ref 0 in
    let old_heap_stacks = !heap_stacks in
    heap_stacks := ( heap', stack' )::old_heap_stacks;
    let heap, stack = get_heap_stack () in
    heap' := max heap !heap';
    stack' := max stack !stack';
    let time = get_time () in
    let x = f () in
    let time' = get_time () -. time in
    heap_stacks := old_heap_stacks;
    let heap' = !heap' - heap in
    let stack' = !stack' - stack in
    ( x, { time'; heap'; stack' } )

let finish s k =
    let k = float_of_int k in {
        time=s.time' /. k;
        heap=word_bytes s.heap' /. k;
        stack=word_bytes s.stack' /. k;
    }
