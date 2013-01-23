(** Naive variant of lazy self-adjusting values. *)

(** Types and operations common to lazy self-adjusting values containing any type. *)
module T = struct
    (** Lazy self-adjusting values containing ['a]. *)
    type 'a thunk = {
        id : int;
        mutable thunk : 'a thunk';
    }
    (**/**) (* auxiliary types *)
    and 'a thunk' =
        | Const of 'a * receipt
        | Var of 'a var
    and 'a var = {
        mutable result : 'a result option;
        repair : repair;
        unmemo : unit -> unit;
    }
    and 'a result = {
        value : 'a;
        receipt : receipt;
        dependencies : receipt list;
    }
    and receipt = visited -> (visited -> bool -> unit) -> unit
    and repair = visited -> (visited -> unit) -> unit
    and visited = (int, unit) Hashtbl.t
    (**/**)


    (**/**) (* change-propagation state *)
    let lazy_id_counter = ref 0
    let lazy_stack = ref []
    (**/**)


    (** Compute the hash value of a self-adjusting value. *)
    let hash m = m.id

    (** Compute whether two self-adjusting values are equal. *)
    let equal = (==)

    (** Recompute self-adjusting values if necessary (unused by this module; a no-op). *)
    let refresh () = ()

    (** Return the value contained by a self-adjusting value, (re-)computing it if necessary. *)
    let force m =
        let value, receipt = match m.thunk with
            | Const ( value, receipt ) ->
                ( value, receipt )
            | Var v ->
                (* compute the value if necessary *)
                v.repair (Hashtbl.create 0) (fun _ -> ());
                match v.result with
                    | Some { value; receipt; _ } ->
                        ( value, receipt )
                    | None ->
                        failwith "repair did not compute result"
        in
        (* add receipt to caller *)
        begin match !lazy_stack with
            | h::_ ->
                h := receipt::!h
            | _ ->
                ()
        end;
        value
end
include T


(** Functor to make constructors and updaters for lazy self-adjusting values of a specific type. *)
module Make (R : Hashtbl.HashedType) : Signatures.SAType.S with type data = R.t and type t = R.t thunk = struct
    include T

    (** Value contained by lazy self-adjusting values for a specific type. *)
    type data = R.t

    (** Lazy self-adjusting values for a specific type. *)
    type t = R.t thunk

    (** Create a lazy self-adjusting value from a constant value that does not depend on other lazy self-adjusting values. *)
    let const x =
        let rec receipt s k = match m.thunk with
            | Var { repair; _ } ->
                repair s begin fun s -> k s begin match m.thunk with
                    | Const ( value, _ ) | Var { result=Some { value; _ }; _ } -> R.equal value x
                    | Var _ -> false
                end end
            | Const _ ->
                k s begin match m.thunk with
                    | Const ( value, _ ) | Var { result=Some { value; _ }; _ } -> R.equal value x
                    | Var _ -> false
                end
        and thunk = Const ( x, receipt )
        and id = !lazy_id_counter
        and m = { id; thunk } in
        incr lazy_id_counter;
        m

    (** Update a lazy self-adjusting value with a constant value that does not depend on other lazy self-adjusting values. *)
    let update_const m x =
        begin match m.thunk with
            | Var v -> v.unmemo ()
            | Const _ -> ()
        end;
        let receipt s k = match m.thunk with
            | Var { repair; _ } ->
                repair s begin fun s -> k s begin match m.thunk with
                    | Const ( value, _ ) | Var { result=Some { value; _ }; _ } -> R.equal value x
                    | Var _ -> false
                end end
            | Const _ ->
                k s begin match m.thunk with
                    | Const ( value, _ ) | Var { result=Some { value; _ }; _ } -> R.equal value x
                    | Var _ -> false
                end
        in
        m.thunk <- Const ( x, receipt )

    (** Create a lazy self-adjusting value from a thunk that may depend on other lazy self-adjusting values. *)
    let thunk f =
        let rec evaluate () =
            (* add self to call stack and evaluate *)
            let dependencies = ref [] in
            lazy_stack := dependencies::!lazy_stack;
            let value = try
                f ()
            with exn ->
                lazy_stack := List.tl !lazy_stack;
                raise exn
            in
            lazy_stack := List.tl !lazy_stack;
            v.result <- Some { value; receipt=receipt value; dependencies=List.rev !dependencies }

        (* receipt/repair performs an truncated inorder traversal of the dependency graph *)
        and receipt x s k = repair s begin fun s -> k s begin match m.thunk with
            | Const ( value, _ ) | Var { result=Some { value; _ }; _ } -> R.equal value x
            | Var _ -> false
        end end

        and repair s k =
            if Hashtbl.mem s id then
                k s
            else begin
                Hashtbl.add s id ();
                match v.result with
                    | None ->
                        evaluate ()
                    | Some { dependencies; _ } ->
                        let rec repair s = function
                            | d::ds -> d s (fun s c -> if c then repair s ds else (evaluate (); k s))
                            | [] -> k s
                        in
                        repair s dependencies
            end

        and unmemo () = ()

        and v = { result=None; repair; unmemo }
        and thunk = Var v
        and id = !lazy_id_counter
        and m = { id; thunk } in
        incr lazy_id_counter;
        m

    (** Update a lazy self-adjusting value with a thunk that may depend on other lazy self-adjusting values. *)
    let update_thunk m f =
        begin match m.thunk with
            | Var v -> v.unmemo ()
            | Const _ -> ()
        end;
        let rec evaluate () =
            (* add self to call stack and evaluate *)
            let dependencies = ref [] in
            lazy_stack := dependencies::!lazy_stack;
            let value = try
                f ()
            with exn ->
                lazy_stack := List.tl !lazy_stack;
                raise exn
            in
            lazy_stack := List.tl !lazy_stack;
            v.result <- Some { value; receipt=receipt value; dependencies=List.rev !dependencies }

        (* receipt/repair performs an truncated inorder traversal of the dependency graph *)
        and receipt x s k = repair s begin fun s -> k s begin match m.thunk with
            | Const ( value, _ ) | Var { result=Some { value; _ }; _ } -> R.equal value x
            | Var _ -> false
        end end

        and repair s k =
            if Hashtbl.mem s m.id then
                k s
            else begin
                Hashtbl.add s m.id ();
                match v.result with
                    | None ->
                        evaluate ()
                    | Some { dependencies; _ } ->
                        let rec repair s = function
                            | d::ds -> d s (fun s c -> if c then repair s ds else (evaluate (); k s))
                            | [] -> k s
                        in
                        repair s dependencies
            end

        and unmemo () = ()

        and v = { result=None; repair; unmemo } in
        m.thunk <- Var v

    (* create memoizing constructors and updaters *)
    include MemoN.Make (struct
        type data = R.t
        type t = R.t thunk

        (** Create memoizing constructor and updater for a lazy self-adjusting value. *)
        let memo (type a) (module A : Hashtbl.HashedType with type t = a) f =
            let module Memotable = Weak.Make (struct
                type t = A.t * R.t thunk
                let hash ( a, _ ) = A.hash a
                let equal ( a, _ ) ( a', _ ) = A.equal a a'
            end) in
            let memotable = Memotable.create 0 in

            (* memoizing constructor *)
            let rec memo x =
                let rec evaluate () =
                    (* add self to call stack and evaluate *)
                    let dependencies = ref [] in
                    lazy_stack := dependencies::!lazy_stack;
                    let value = try
                        f memo x
                    with exn ->
                        lazy_stack := List.tl !lazy_stack;
                        raise exn
                    in
                    lazy_stack := List.tl !lazy_stack;
                    v.result <- Some { value; receipt=receipt value; dependencies=List.rev !dependencies }

                (* receipt/repair performs an truncated inorder traversal of the dependency graph *)
                and receipt x s k = repair s begin fun s -> k s begin match m.thunk with
                    | Const ( value, _ ) | Var { result=Some { value; _ }; _ } -> R.equal value x
                    | Var _ -> false
                end end

                and repair s k =
                    if Hashtbl.mem s m.id then
                        k s
                    else begin
                        Hashtbl.add s m.id ();
                        match v.result with
                            | None ->
                                evaluate ()
                            | Some { dependencies; _ } ->
                                let rec repair s = function
                                    | d::ds -> d s (fun s c -> if c then repair s ds else (evaluate (); k s))
                                    | [] -> k s
                                in
                                repair s dependencies
                    end

                (* create a strong reference to binding and hide it in the closure unmemo stored in m *)
                and binding = ( x, m )
                and unmemo () = Memotable.remove memotable binding

                and v = { result=None; repair; unmemo }
                and thunk = Var v
                and id = !lazy_id_counter
                and m = { id; thunk } in
                incr lazy_id_counter;
                snd (Memotable.merge memotable binding)
            in

            (* memoizing updater *)
            let update_memo m x =
                begin match m.thunk with
                    | Var v -> v.unmemo ()
                    | Const _ -> ()
                end;
                let rec evaluate () =
                    (* add self to call stack and evaluate *)
                    let dependencies = ref [] in
                    lazy_stack := dependencies::!lazy_stack;
                    let value = try
                        f memo x
                    with exn ->
                        lazy_stack := List.tl !lazy_stack;
                        raise exn
                    in
                    lazy_stack := List.tl !lazy_stack;
                    v.result <- Some { value; receipt=receipt value; dependencies=List.rev !dependencies }

                (* receipt/repair performs an truncated inorder traversal of the dependency graph *)
                and receipt x s k = repair s begin fun s -> k s begin match m.thunk with
                    | Const ( value, _ ) | Var { result=Some { value; _ }; _ } -> R.equal value x
                    | Var _ -> false
                end end

                and repair s k =
                    if Hashtbl.mem s m.id then
                        k s
                    else begin
                        Hashtbl.add s m.id ();
                        match v.result with
                            | None ->
                                evaluate ()
                            | Some { dependencies; _ } ->
                                let rec repair s = function
                                    | d::ds -> d s (fun s c -> if c then repair s ds else (evaluate (); k s))
                                    | [] -> k s
                                in
                                repair s dependencies
                    end

                (* create a strong reference to binding and hide it in the closure unmemo stored in m *)
                and binding = ( x, m )
                and unmemo () = Memotable.remove memotable binding

                and v = { result=None; repair; unmemo }
                and thunk = Var v in
                m.thunk <- thunk;
                ignore (Memotable.merge memotable binding)
            in

            ( memo, update_memo )
    end)
end
