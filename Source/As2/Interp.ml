(* Goals:

   basic goals:
   -- movement
   -- writing (creating new formula; eventually, type-checking them too)
   -- reading (evaluating formula)
   -- visual display (ascii text to terminal)

   intermediate goals:
   -- integration with Adapton
   -- load/save to disk
   -- replay test scripts

   longer-term goals:
   -- type checker
   -- explore circular references:
      -- loan amortization demo
      -- game of life demo

   Questions:
   -- sorting (?) -- how does that fit into this demo?
   -- lazy lists -- are these needed / relevant here?
*)

module A = Adapton.PolySA.Make(Adapton.LazySABidi)

module type INTERP = sig
  type cell
  type db
  type cur

  val empty : int * int * int -> db
  val eval  : Ast.sht -> Ast.formula -> db -> Ast.const

  type 'a fold_body = ( cur -> 'a -> 'a )

  type 'a foldees = { fold_cell      : 'a fold_body ;
                      fold_row_begin : 'a fold_body ;
                      fold_row_end   : 'a fold_body ; }

  val fold_region  : Ast.absolute_region -> db -> 'a foldees -> 'a -> 'a
  val print_region : Ast.absolute_region -> db -> out_channel -> unit

  (* Cursor-based interaction: *)
  val cursor  : Ast.pos -> db -> cur
  val move    : Ast.nav_cmd -> cur -> cur
  val get_frm : cur -> Ast.formula
  val get_pos : cur -> Ast.pos

(*
  val load  : string -> db
  val save  : db -> string -> unit
*)

  val read   : cur -> Ast.const
  val write  : Ast.mut_cmd -> cur -> cur
end

module Interp : INTERP = struct
  open Ast

  exception NYI

  module Coord = struct
    type t = pos
    let compare c1 c2 = compare c1 c2
    let equals c1 c2 = (c1 = c2)
  end

  module M = Map.Make(Coord)

  type cell = { mutable cell_frm : formula ; }

  (* these mutable field below for cells need not be instrumented by
     adapton bc the changes will only be monotonic (will only add new
     stuff, will not alter the associations/identity of old stuff). *)
  type db = { nshts  : int ;
              ncols  : int ;
              nrows  : int ;
              mutable cells : cell M.t ;
            }

  type cur = { db : db ;
               pos : pos ; }


  let empty (nshts,ncols,nrows) =
    { nshts = nshts ;
      ncols = ncols ;
      nrows = nrows ;
      cells = M.empty ;
    }

(*
  let load filename =
    raise NYI

  let save db filename =
    raise NYI
*)

  let cursor pos db = { pos = pos ; db = db }

  let get_pos cur = cur.pos

  (* get the formula at the cursor -- do a map lookup *)
  let get_frm cur =
    try (M.find cur.pos cur.db.cells).cell_frm with
      | Not_found -> F_const Undef

  let sht_of_reg (s,_) = s
  let sht_of_pos (s,_) = s

  let pos_is_valid : pos -> db -> bool =
    fun (s,(c,r)) {nshts;ncols;nrows} ->
      ( s > 0 && c > 0 && r > 0 &&
          s <= nshts && c <= ncols && r <= nrows )

  (* move with no bounds checks. *)
  let move_raw navcmd = fun cur ->
    { cur with pos =
        let pos = cur.pos in
        match navcmd with
          | C_goto (Abs pos) -> pos
          | C_goto (Lcl lc)  -> (fst pos, lc)
          | C_prev (Nav_sht) -> (fst pos - 1, snd pos)
          | C_next (Nav_sht) -> (fst pos + 1, snd pos)
          | C_next (Nav_col) -> (fst pos, (fst (snd pos) + 1, (snd (snd pos))))
          | C_prev (Nav_col) -> (fst pos, (fst (snd pos) - 1, (snd (snd pos))))
          | C_prev (Nav_row) -> (fst pos, (fst (snd pos), (snd (snd pos)) - 1))
          | C_next (Nav_row) -> (fst pos, (fst (snd pos), (snd (snd pos)) + 1))
    }

  let move navcmd : cur -> cur = fun cur ->
    let cur' = move_raw navcmd cur in
    { cur with pos =
        if pos_is_valid cur'.pos cur.db
        then cur'.pos else cur.pos }

  type 'a fold_body = ( cur -> 'a -> 'a )
  type 'a foldees = { fold_cell      : 'a fold_body ;
                      fold_row_begin : 'a fold_body ;
                      fold_row_end   : 'a fold_body ; }

  (* Folding a region: move in row-major order through the region.
     uses the foldee callbacks to fold a parametric accumulator
     through the region's structure. *)
  let fold_region : Ast.absolute_region -> db -> 'a foldees -> 'a -> 'a =

    fun (sht, (lc1, (max_col, max_row))) db foldees x ->
      let cur =
        cursor (sht, lc1) db
      in
      let within_region {pos=(s,(c,r))} =
        ((c <= max_col) && (r <= max_row))
      in
      let rec loop_rows : cur -> 'a -> 'a = fun cur x ->
        (* fold cols on current row *)
        let rec loop_cols : cur -> 'a -> (cur * 'a) = fun cur x ->
          if within_region cur then
            let x   = foldees.fold_cell cur x in
            let cur = move_raw (Ast.C_next Ast.Nav_col) cur in
            loop_cols cur x
          else
            cur, x
        in
        (* fold all remaining rows: *)
        if not (within_region cur) then x
        else
          let   x = foldees.fold_row_begin cur x in
          let _,x = loop_cols cur x in
          let   x = foldees.fold_row_end cur x in
          let cur = move_raw (Ast.C_next Ast.Nav_row) cur in
          loop_rows cur x
      in
      loop_rows cur x

  (* create an absolute coord *)
  let absolute : sht -> coord -> pos
    = fun s -> function
      | Abs(s',(c,r)) -> (s',(c,r))
      | Lcl(c,r)      -> (s,(c,r))


  (* -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- *)
  (* -- formula evaluation -- *)
  (* ADAPTON: Memoize this function, as well as eval_rec (where the sheet remains fixed). *)
  let rec eval sht frm db =

    let to_app_form : 'a list -> ('a list -> 'a list)
      = fun xs -> (fun xs_tail -> xs @ xs_tail)
    in

    let snoc : ('a list -> 'a list) -> 'a -> ('a list -> 'a list)
      = fun xs y -> (fun tl -> xs (y :: tl))
    in

    (* lookup and evaluate an absolute coordinate. *)
    let rec lookup : pos -> const =
      fun ((s,lc) as pos) ->
        let frm =
          try (M.find pos db.cells).cell_frm with
            | Not_found -> F_const Undef
        in
        if s <> sht then eval s frm db
        else eval_rec frm

    (* evaluate on same sheet *)
    and eval_rec : formula -> const = function
      | F_const c -> c
      | F_paren f -> eval_rec f
      | F_coord coord -> lookup (absolute sht coord)
      | F_func(f,r) ->
          let r = match r with
            | R_lcl lr -> (sht,lr)
            | R_abs reg -> reg
          in
          let cells = fold_region r db {
            fold_row_begin = begin fun cur x -> x end ;
            fold_row_end   = begin fun cur x -> x end ;
            fold_cell      = begin fun cur cells -> snoc cells (eval_rec (get_frm cur)) end
          } (to_app_form [])
          in
          begin match cells [] with
            | []    -> Undef
            | x::xs ->
                (* ADAPTON: replace with Adapton Tree-Fold. *)
                List.fold_right begin fun x y -> match f, x, y with
                  | Fn_sum, Num x, Num y -> Num ( Num.add_num x y )
                  | Fn_max, Num x, Num y -> Num ( if Num.gt_num x y then x else y )
                  | Fn_min, Num x, Num y -> Num ( if Num.gt_num x y then y else x )
                  | _     , Undef, _     -> Undef
                  | _     , _,     Undef -> Undef
                end xs x
          end

      | F_binop(bop,f1,f2) -> begin
          let c1 = eval_rec f1 in
          let c2 = eval_rec f2 in
          begin match bop, c1, c2 with
            | Bop_add, Num n1, Num n2 -> Num (Num.add_num n1 n2)
            | Bop_sub, Num n1, Num n2 -> Num (Num.sub_num n1 n2)
            | Bop_div, Num n1, Num n2 -> Num (Num.div_num n1 n2)
            | Bop_mul, Num n1, Num n2 -> Num (Num.mult_num n1 n2)
            | _, Undef, _     -> Undef
            | _, _    , Undef -> Undef
          end
        end
    in eval_rec frm

  (* -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- *)
  (* -- pretty printing -- *)
  let print_region : Ast.absolute_region -> db -> out_channel -> unit =
    fun reg db out ->
      let ps = print_string in
      fold_region reg db {
        fold_row_begin = begin fun cur x -> ()      end ;
        fold_row_end   = begin fun _ _   -> ps "\n" end ;
        fold_cell =
          begin fun cur _ ->
            let frm = get_frm cur in
            ps "| " ;
            Printf.fprintf out "%10s"
              (Pretty.string_of_const (eval (sht_of_reg reg) frm db)) ;
            ps " |"
          end } ()

  let read cur =
    eval (sht_of_pos cur.pos) (get_frm cur) cur.db

  let write mutcmd cur =
    begin match mutcmd with
      | C_set frm -> begin
          cur.db.cells <- M.add cur.pos { cell_frm=frm } cur.db.cells
        end
    end
    ; cur


end

include Interp
