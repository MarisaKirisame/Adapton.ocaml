
type stats_test_param = int * [`Switch|`No_switch]
type func =
  | F_repl
  | F_stats_test of stats_test_param

let num_sheets     = ref 20
let func           = ref F_repl
let verbose_errors = ref false
let print_passes   = ref true
let print_ast_db   = ref false
let stats_out      = ref "as2-stats.out"
let stateless_eval = ref true
let num_changes    = ref 10


let rec args = [
  ("--stateless-eval",  Arg.Set stateless_eval, " use stateless evaluation semantics" ) ;
  ("--stateful-eval",   Arg.Clear stateless_eval, " use stateful evaluation semantics" ) ;

  ("--repl",              Arg.Unit begin fun _ -> func := F_repl end, " functionality/mode: read-eval-print-loop (REPL)") ;

  ("--stats-test",        Arg.Int begin fun n -> num_sheets := n; func := F_stats_test (n, `No_switch) end, " controls functionality/mode: run a predefined script, of a given size and record statisitics") ;
(*  ("--stats-test-switch", Arg.Int begin fun n -> num_sheets := n; func := F_stats_test (n, `Switch) end,    " functionality/mode: run a predefined script (that switches), of a given size and record statisitics") ; *)
  ("--num-sheets",        Arg.Int begin fun i -> num_sheets := i end, " set the total number of sheets (default: 20)" ) ;
  ("--num-changes",       Arg.Int begin fun i -> num_changes := i end, " set the number changes in the test script (default: 10)") ;
  ("--stats-out",         Arg.String begin fun s -> stats_out := s end, " write out stats to the given file" ) ;

  ("--Random.self_init", Arg.Unit begin fun _ -> Random.self_init () end, " initialize the Random module's number generator" ) ;
  ("--verbose",          Arg.Set verbose_errors, " give verbose (contextual) errors") ;
  ("--ast-db",           Arg.Set print_ast_db, " give verbose debugging information in formulae") ;
]

let cur_filename = ref ""

let lexbuf : Lexing.lexbuf ref = ref (Lexing.from_string "")

let set_lexbuf lb = lexbuf := lb
let get_lex_pos _ = (!lexbuf).Lexing.lex_curr_p

module Prov = struct
  type loc = Lexing.position
  type loc_range = loc * loc

  and prov =
    | Synth
    | Root of loc_range
    | Stepped of prov

  let rec sprint_prov prefix = function
    | Synth -> prefix^"synth"
    | Stepped p -> sprint_prov prefix p
    | Root (loc1, loc2) ->
        Printf.sprintf "%sFile \"%s\", line %d, characters %d-%d"
          prefix
          loc1.Lexing.pos_fname
          loc1.Lexing.pos_lnum
          (loc1.Lexing.pos_cnum - loc1.Lexing.pos_bol)
          (loc2.Lexing.pos_cnum - loc2.Lexing.pos_bol)

          (*
    | Subst (n, p1, p2) ->
        Printf.sprintf "%sSubstitution of `%s' at\n%s%sfor original `%s' at\n%s"
          prefix n (sprint_prov (prefix^"  ") p2)
          prefix n (sprint_prov (prefix^"  ") p1) *)
end
