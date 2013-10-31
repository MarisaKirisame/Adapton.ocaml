exception Error of exn * (int * int * string)
exception Internal_error

let ps = print_string

let help () =
  ps "=========================================================================\n" ;
  ps "AS2 HELP:                                                                \n" ;
  ps "-------------------------------------------------------------------------\n" ;
  ps "Commands:                                                                \n" ;
  ps " 'help'            -- this help                                          \n" ;
  ps " 'exit'            -- use excel instead                                  \n" ;
  ps "                                                                         \n" ;
  ps " = frm .           -- set formula of current cell to frm                 \n" ;
  ps "                      Note: start with equals; terminate with a dot.     \n" ;
  ps "                                                                         \n" ;
  ps " 'goto' coord      -- goto a specific (sheet), row and column            \n" ;
  ps " 'next' nav-thing  -- next col/row/sheet                                 \n" ;
  ps " 'prev' nav-thing  -- prev col/row/sheet                                 \n" ;
  ps "                                                                         \n" ;
  ps "-------------------------------------------------------------------------\n" ;
  ps "Formula & Coordinate Syntax                                              \n" ;
  ps "                                                                         \n" ;
  ps " Nav. thing   nav-thing ::= 'row' | 'col' | 'sheet'                      \n" ;
  ps "                                                                         \n" ;
  ps " Formulae     frm       ::= func ( reg )                                 \n" ;
  ps "                         | frm binop frm | num | coord | ( frm )         \n" ;
  ps "                                                                         \n" ;
  ps " Functions    func      ::= 'sum' | 'max' | 'min'                        \n" ;
  ps " Binops       binop     ::= + | - | / | *                                \n" ;
  ps " Regions      reg       ::= lr | 'sheet' num ! lr                        \n" ;
  ps " Coordinates  coord     ::= lc | 'sheet' num ! lc                        \n" ;
  ps " Local coord  lc        ::= letters num                                  \n" ;
  ps " Local region lr        ::= lc : lc                                      \n" ;
  ps "-------------------------------------------------------------------------\n" ;
  ps " All keywords, above in quotes, are also valid in all caps               \n" ;
  ps "-------------------------------------------------------------------------\n" ;
  ()

let parse_channel : string -> in_channel -> Ast.cmd =
  fun filename channel ->
    let lexbuf = Lexing.from_channel channel in
    let pos = lexbuf.Lexing.lex_curr_p in
    let _ =
      lexbuf.Lexing.lex_curr_p <-
        { pos with
            Lexing.pos_fname = filename ;
            Lexing.pos_lnum = 1 ;
        }
    in
    let _ = Global.set_lexbuf lexbuf in
    let ast : Ast.cmd =
      try Parser.cmd Lexer.token lexbuf
      with
        | exn -> begin
            let curr = lexbuf.Lexing.lex_curr_p in
            let line = curr.Lexing.pos_lnum in
            let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
            let tok = Lexing.lexeme lexbuf in
            raise (Error (exn, (line, cnum, tok)))
          end
    in
    ast

let _ =
  let db  = Interp.empty (100,10,10) in
  let cur = Interp.cursor (1,(1,1)) db in
  
  let measure f = 
    let module S = Adapton.Statistics in
    let x, m = S.measure f
    in
    begin Printf.printf "time=%f, heap=%d, stack=%d, upd=%d, eval=%d, dirty=%d, clean=%d\n"
        m.S.time' m.S.heap' m.S.stack' 
        m.S.update' m.S.evaluate' m.S.dirty' m.S.clean' 
      ;
      x 
    end
  in

  (* REPL seed: *)
  let rec repl cur =
    let handler () = begin
      Printf.printf "= " ;
      Ast.Pretty.pp_formula' (Interp.get_frm cur) ;
      Printf.printf "\n"
      ;
      Ast.Pretty.pp_pos (Interp.get_pos cur)
      ;
      Printf.printf "> %!" ;
      let cmd' =        
        try
          Some ( parse_channel "<stdin>" stdin )
        with
          | Error (_, (line, col, token)) ->
              ( Printf.eprintf "line %d, character %d: syntax error at %s\n%!"
                  (* filename *) line col
                  ( if token = "\n"
                    then "newline"
                    else Printf.sprintf "`%s'" token ) ;
                None
              )
      in
      begin match cmd' with
        | None -> ps "Oops! Try 'help' for reference information.\n" ; cur
            
        | Some (Ast.C_print) -> 
            let (sht,_) = Interp.get_pos cur in
            Interp.print_region (sht,((1,1),(10,10))) db stdout ;
            cur

        | Some (Ast.C_help) -> help () ; cur
        | Some (Ast.C_exit) -> exit (1)
            
        | Some (Ast.C_scramble Ast.Sf_none)  -> Interp.scramble       cur ; cur
        | Some (Ast.C_scramble Ast.Sf_dense) -> Interp.scramble_dense cur ; cur

        | Some ((Ast.C_nav nc) as cmd) ->
            ps "navigation command: " ; Ast.Pretty.pp_cmd cmd ; ps "\n" ;
            (Interp.move nc cur)
              
        | Some ((Ast.C_mut mc) as cmd) ->
            ps "mutation command: " ; Ast.Pretty.pp_cmd cmd ; ps "\n" ;
            (Interp.write mc cur)
      end
    end
    in
    let cur = measure handler in
    (repl cur) (* repl is a tail-recursive loop. *)
  in
  (* enter repl *)
  (repl cur)

(* Not in use: FILE processing *)

let process_file : string -> Ast.cmd = fun filename ->
  let _ = Global.cur_filename := filename in
  let input =
    if filename = "-"
    then stdin
    else open_in filename
  in
  let cmd =
    try parse_channel filename input
    with
      | Error (_, (line, col, token)) ->
          ( Printf.eprintf "File %s, line %d, character %d: syntax error at %s\n%!"
              filename line col
              ( if token = "\n"
                then "newline"
                else Printf.sprintf "`%s'" token ) ;
            exit (-1) )
  in
  Ast.Pretty.pp_cmd cmd ;
  cmd

let run () =
  if false then
    let input_files : string list ref = ref [] in
    if !Global.print_passes then Printf.eprintf "parsing input files...\n%!" ;
    let _ = Arg.parse Global.args
      (fun filename -> input_files := filename :: !input_files)
      "usage: m3pc [options] [input files]"
    in
    if !input_files = [] then (
      Printf.eprintf "no input files given!\n" ;
      exit (-1);
    );
    let _ = List.map process_file (List.rev (!input_files)) in
    (** TODO -- emit/do something! **)
    ()
    ;
    if !Global.print_passes then
      Printf.eprintf "done.\n%!"
    else ()



