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

module type INTERP = sig
  type cell
  type db
  type cur

  val empty : int * int * int -> db
  val eval  : db -> Ast.sht -> Ast.formula -> Ast.const Ast.A.thunk

  type 'a fold_body = ( cur -> 'a -> 'a )

  type 'a foldees = { fold_cell      : 'a fold_body ;
                      fold_row_begin : 'a fold_body ;
                      fold_row_end   : 'a fold_body ; }

  val fold_region  : Ast.absolute_region -> db -> 'a foldees -> 'a -> 'a
  val print_region : Ast.absolute_region -> db -> out_channel -> unit

  (* Cursor-based interaction: *)
  val cursor  : Ast.pos -> db -> cur
  val move    : Ast.nav_cmd -> cur -> cur
  val get_frm : cur -> Ast.formula'
  val get_pos : cur -> Ast.pos
(*
  val load  : string -> db
  val save  : db -> string -> unit
*)

  val read   : cur -> Ast.const
  val write  : Ast.mut_cmd -> cur -> cur

  val scramble : cur -> unit
  val scramble_dense : cur -> unit
end

module Interp : INTERP = struct
  open Ast

  exception NYI

  module Coord = struct
    type t = pos
    let compare c1 c2 = compare c1 c2
    let equals c1 c2 = (c1 = c2)
  end

  module Mp = Map.Make(Coord)

  type cell = { cell_frm : formula' }

  (* these mutable field below for cells need not be instrumented by
     adapton bc the changes will only be monotonic (will only add new
     stuff, will not alter the associations/identity of old stuff). *)
  type db = { nshts  : int ;
              ncols  : int ;
              nrows  : int ;
              mutable cells : cell Mp.t ;
            }

  type cur = { db : db ;
               pos : pos ; }


  let empty (nshts,ncols,nrows) =
    { nshts = nshts ;
      ncols = ncols ;
      nrows = nrows ;
      cells = Mp.empty ;
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
    try (Mp.find cur.pos cur.db.cells).cell_frm with
        (* TODO -- create the cell on demand. *)
      | Not_found -> A.const (F_const Undef)

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
  
  (* lookup and evaluate an absolute coordinate. *)
  let lookup_cell : db -> pos -> cell = 
    fun db pos ->
      try (Mp.find pos db.cells) with
        | Not_found -> begin
            let undef_frm = A.const (F_const Undef) in
            let undef_cell = { cell_frm = undef_frm } in
            (* Monotonic side effect: 
               create and remember a new formula, initially holding Undef. *)
            db.cells <- Mp.add pos undef_cell db.cells ;
            undef_cell
          end

  (* -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- *)
  (* -- formula evaluation -- *)
  (* ADAPTON memoizes this function, based on the sheet and formula arguments. *)
  let eval db = 
    let eval_memoized = A.memo2
      ~inp2_equal:Ast.frm_equal
      ~inp2_hash:Ast.frm_hash
      begin fun eval_memo sht (frm : formula) ->
      
        let to_app_form : 'a list -> ('a list -> 'a list)
          = fun xs -> (fun xs_tail -> xs @ xs_tail)
        in
        
        let snoc : ('a list -> 'a list) -> 'a -> ('a list -> 'a list)
          = fun xs y -> (fun tl -> xs (y :: tl))
        in
        
        (* lookup and evaluate an absolute coordinate. *)
        let lookup_eval : pos -> const = 
          fun pos ->
            let frm = (lookup_cell db pos).cell_frm in           
            A.force (eval_memo sht (A.force frm))
        in

        let eval_memo' sht frm = eval_memo sht (A.force frm)
        in

        (* evaluate given formula *)
        match frm with
          | F_const c -> c
          | F_paren f -> A.force (eval_memo' sht f)
          | F_coord coord -> lookup_eval (absolute sht coord)
          | F_func(f,r) ->
              let r = match r with
                | R_lcl lr -> (sht,lr)
                | R_abs reg -> reg
              in
              let cells = fold_region r db {
                fold_row_begin = begin fun cur x -> x end ;
                fold_row_end   = begin fun cur x -> x end ;
                fold_cell      = begin fun cur cells -> snoc cells (eval_memo' sht (get_frm cur)) end
              } (to_app_form [])
              in
              begin match cells [] with
                | []    -> Undef
                | x::xs ->
                    let x = A.force x in
                    List.fold_right begin fun x y -> 
                      let x = A.force x in
                      match f, x, y with
                        | Fn_sum,  Num x, Num y -> Num ( Num.add_num x y )
                        | Fn_max,  Num x, Num y -> Num ( if Num.gt_num x y then x else y )
                        | Fn_min,  Num x, Num y -> Num ( if Num.gt_num x y then y else x )
                        | _, Fail, _            -> Fail
                        | _, _   , Fail         -> Fail 
                        | _      , Undef, _     -> Undef
                        | _      , _,     Undef -> Undef
                    end xs x
              end
                
          | F_binop(bop,f1,f2) -> begin
              let c1 = A.force (eval_memo' sht f1) in
              let c2 = A.force (eval_memo' sht f2) in
              try
                begin match bop, c1, c2 with
                  | Bop_add, Num n1, Num n2 -> Num (Num.add_num n1 n2)
                  | Bop_sub, Num n1, Num n2 -> Num (Num.sub_num n1 n2)
                  | Bop_div, Num n1, Num n2 -> Num (Num.div_num n1 n2)
                  | Bop_mul, Num n1, Num n2 -> Num (Num.mult_num n1 n2)
                  | _, Fail , _     -> Fail
                  | _, _    , Fail  -> Fail 
                  | _, Undef, x     -> Undef
                  | _, x    , Undef -> Undef
                end
              with
                | Failure _ -> Fail
            end
      end
    in
    eval_memoized

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
            let frm = A.force (get_frm cur) in
            ps "| " ;
            Printf.fprintf out "%10s"
              (Pretty.string_of_const (A.force(eval db (sht_of_reg reg) frm))) ;
            ps " |"
          end } ()

  let read cur =
    A.force (eval cur.db (sht_of_pos cur.pos) (A.force (get_frm cur)))

  let write mutcmd cur =
    begin match mutcmd with
      | C_set frm -> begin
          let cell = lookup_cell cur.db cur.pos in
          A.update_const cell.cell_frm (A.force frm)
        end
    end
    ; cur

  let scramble cur =
    let db = cur.db in
    for s = 1 to db.nshts do
      for r = 1 to db.ncols do
        for c = 1 to db.nrows do
          let cell = lookup_cell db (s,(c,r)) in
          if s <= 1 || r <= 1 || c <= 1 then
            A.update_const cell.cell_frm 
              (Ast.F_const (Ast.Num (Num.num_of_int (Random.int 10000))))
          else
            let rnd max = (Random.int (max - 1)) + 1 in
            let s1, s2 = rnd s, rnd s in
            let c1, c2 = rnd db.ncols, rnd db.ncols in
            let r1, r2 = rnd db.nrows, rnd db.nrows in
            let b = match Random.int 4 with
              | 0 -> Ast.Bop_add
              | 1 -> Ast.Bop_sub 
              | 2 -> Ast.Bop_div
              | 3 -> Ast.Bop_mul
              | _ -> invalid_arg "oops"
            in
            let f1 = Ast.F_coord (Abs (s1, (c1, r1))) in
            let f2 = Ast.F_coord (Abs (s2, (c2, r2))) in
            let f3 = Ast.F_binop (b, (memo_frm f1), (memo_frm f2)) in
            A.update_const cell.cell_frm f3
        done
      done
    done

  let scramble_dense cur =
    let db = cur.db in
    for s = 1 to db.nshts do
      for r = 1 to db.ncols do
        for c = 1 to db.nrows do
          let cell = lookup_cell db (s,(c,r)) in
          if s <= 1 || r <= 1 || c <= 1 then
            A.update_const cell.cell_frm 
              (Ast.F_const (Ast.Num (Num.num_of_int (Random.int 10000))))
          else
            let rnd max = (Random.int (max - 1)) + 1 in
            let c1, c2 = rnd db.ncols, rnd db.ncols in
            let r1, r2 = rnd db.nrows, rnd db.nrows in
            let b = match Random.int 4 with
              | 0 -> Ast.Bop_add
              | 1 -> Ast.Bop_sub 
              | 2 -> Ast.Bop_div
              | 3 -> Ast.Bop_mul
              | _ -> invalid_arg "oops"
            in
            (* dense ==> use the previous sheet. *)
            let f1 = Ast.F_coord (Abs (s - 1, (c1, r1))) in
            let f2 = Ast.F_coord (Abs (s - 1, (c2, r2))) in
            let f3 = Ast.F_binop (b, (memo_frm f1), (memo_frm f2)) in
            A.update_const cell.cell_frm f3
        done
      done
    done

end

include Interp
