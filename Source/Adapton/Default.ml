(** Default modules for self-adjusting values and applications. *)

(** Default module for self-adjusting values. *)
module SA = LazySABidi

(** Default module for self-adjusting lists. *)
module SAList = SAList.Make (SA)

(**/**)(* tweak GC for self-adjusting computations *)
let _ =
    let open Gc in
    let control = get () in
    set { control with
        minor_heap_size = max control.minor_heap_size (2 * 1024 * 1024);
        major_heap_increment = max control.minor_heap_size (4 * 1024 * 1024);
    }
(**/**)
