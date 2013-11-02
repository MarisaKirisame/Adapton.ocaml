
type stats_test_param = int
type func =
  | F_repl
  | F_stats_test of stats_test_param

let func           = ref F_repl
let verbose_errors = ref false
let print_passes   = ref true
let print_ast_db   = ref false

let rec args = [
  ("--help",       Arg.Unit begin fun _ -> Arg.usage args "blah" ; exit 0 end, "print this help message" ) ;
  ("--verbose",    Arg.Set verbose_errors, "give verbose (contextual) errors") ;
  ("--ast-db",     Arg.Set print_ast_db, "give verbose debugging information in formulae") ;
  ("--repl",       Arg.Unit begin fun _ -> func := F_repl end, "functionality/mode: read-eval-print-loop (REPL)") ;
  ("--Random.self_init", Arg.Unit begin fun _ -> Random.self_init () end, "initialize the Random module's number generator" ) ;
  ("--stats-test", Arg.Int begin fun n -> func := F_stats_test n end,
   "functionality/mode: run a predefined script, of a given size and record statisitics") ;
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
