(** Bidirectional variant of lazy self-adjusting values based on {!LazySAObject}. *)

(** Types and operations common to lazy self-adjusting values containing any type. *)
module T = struct
    (** Abstract type identifying this module for self-adjusting values. *)
    type sa

    (**/**) (* auxiliary types *)
    module rec TT : sig
        type 'a state =
            | MemoValue of 'a * receipt * dependency list * (unit -> 'a) * unmemo (* 6 words *)
            | Value of 'a * receipt * dependency list * (unit -> 'a) (* 5 words *)
            | MemoThunk of (unit -> 'a) * unmemo (* 3 words *)
            | Thunk of (unit -> 'a) (* 2 words *)
            | Const of 'a * receipt (* 3 words *)
        and unmemo = unit -> unit
        and receipt = (bool -> unit) -> unit
        and dependent = < dependents : Dependents.t >
        and dependency = { (* 3 words (dependent is 'a thunk) *)
            mutable dirty : bool;
            mutable receipt : receipt;
            dependent : dependent;
        }
    end = TT
    and Dependents : Weak.S with type data = TT.dependency = Weak.Make (struct
        type t = TT.dependency
        let hash d = Hashtbl.hash d.TT.dependent
        let equal d d' = d.TT.dependent == d'.TT.dependent
    end)
    include TT
    (**/**)


    (** This module implements self-adjusting values. *)
    let is_self_adjusting = true

    (** This module implements lazy values. *)
    let is_lazy = true


    (**/**) (* change-propagation state *)
    let lazy_stack = ref []
    (**/**)


    (** Lazy self-adjusting values containing ['a]. *)
    class virtual ['a] thunk equal init = object (self) (* (2 + 2) + 6 = 10 words (not including closure of unmemo and receipt) *)
        val mutable thunk : 'a state = init (* 'a state: 6 words *)
        val dependents : Dependents.t = Dependents.create 0

        method dependents = dependents

        method private dirty =
            begin match thunk with
                | MemoValue ( _, _, _, _, unmemo ) | MemoThunk ( _, unmemo ) -> unmemo ()
                | Value _ | Thunk _ | Const _ -> ()
            end;
            let rec dirty = function
                | d::ds ->
                    dirty begin Dependents.fold begin fun d ds ->
                        if d.dirty then
                            ds
                        else begin
                            d.dirty <- true;
                            d.dependent#dependents::ds
                        end
                    end d ds end
                | [] ->
                    ()
            in
            dirty [ dependents ]

        method update_const x =
            self#dirty;
            thunk <- Const ( x, self#make_receipt x )

        method update_thunk f =
            self#dirty;
            thunk <- Thunk f

        method update_memo_thunk f unmemo =
            self#dirty;
            thunk <- MemoThunk ( f, unmemo )

        method force =
            let value, receipt = match thunk with
                | MemoValue _ | Value _ ->
                    (* compute the value if necessary *)
                    self#repair (fun _ -> ());
                    begin match thunk with
                        | MemoValue ( value, receipt, _, _, _ ) | Value ( value, receipt, _, _ ) ->
                            ( value, receipt )
                        | MemoThunk _ | Thunk _ | Const _ ->
                            failwith "repair did not compute result"
                    end;
                | MemoThunk ( f, _ ) | Thunk f ->
                    self#evaluate f
                | Const ( value, receipt ) ->
                    ( value, receipt )
            in
            (* add dependency to caller *)
            begin match !lazy_stack with
                | ( dependent, dependencies )::_ ->
                    let dependency = Dependents.merge dependents { dirty=false; receipt; dependent } in
                    (* an existing dependency may be reused *)
                    dependency.dirty <- false;
                    dependency.receipt <- receipt;
                    dependencies := dependency::!dependencies
                | _ ->
                    ()
            end;
            value

        method private make_receipt x k = self#repair begin fun () -> k begin match thunk with
            | MemoValue ( x', _, _, _, _ ) | Value ( x', _, _, _ ) | Const ( x', _ )  -> equal x x'
            | MemoThunk _ | Thunk _ -> false
        end end

        method private evaluate f =
            (* add self to call stack and evaluate *)
            let dependencies = ref [] in
            lazy_stack := ( ( self :> dependent ), dependencies )::!lazy_stack;
            let value = try
                f ()
            with exn ->
                lazy_stack := List.tl !lazy_stack;
                raise exn
            in
            lazy_stack := List.tl !lazy_stack;
            let dependencies = List.rev !dependencies in
            let receipt = self#make_receipt value in
            begin match thunk with
                | MemoValue ( _ , _, _, _, unmemo ) | MemoThunk ( _, unmemo ) ->
                    thunk <- MemoValue ( value, receipt, dependencies, f, unmemo )
                | Value _ | Thunk _ ->
                    thunk <- Value ( value, receipt, dependencies, f )
                | Const _ ->
                    failwith "evaluating Const"
            end;
            ( value, receipt )

        (* receipt/repair performs a truncated inorder traversal of the dependency graph *)
        method private repair k = match thunk with
            | MemoValue ( _ , _, dependencies, f, _ ) | Value ( _ , _, dependencies, f ) ->
                let rec repair = function
                    | d::ds ->
                        if d.dirty then begin
                            d.dirty <- false;
                            d.receipt (fun c -> if c then repair ds else (ignore (self#evaluate f); k ()))
                        end else
                            repair ds
                    | [] ->
                        k ()
                in
                repair dependencies
            | MemoThunk ( f, _ ) | Thunk f ->
                ignore (self#evaluate f); k ()
            | Const _ ->
                k ()
    end


    (** Compute the hash value of a self-adjusting value. *)
    let hash = Hashtbl.seeded_hash

    (** Compute whether two self-adjusting values are equal. *)
    let equal = (==)

    (** Recompute self-adjusting values if necessary (unused by this module; a no-op). *)
    let refresh () = ()

    (** Return the value contained by a self-adjusting value, (re-)computing it if necessary. *)
    let force m = m#force
end
include T


(** Functor to make a constructor, a mutator, and a memoizing constructor for lazy self-adjusting values of a specific type. *)
module Make (R : Hashtbl.SeededHashedType)
        : Signatures.SAType.S with type sa = sa and type 'a thunk = 'a thunk and type data = R.t and type t = R.t thunk = struct
    include T

    (** Value contained by lazy self-adjusting values for a specific type. *)
    type data = R.t

    (** Lazy self-adjusting values for a specific type. *)
    class t init = object (self)
        inherit [R.t] thunk R.equal init
    end

    (** Create a lazy self-adjusting value from a constant value that does not depend on other lazy self-adjusting values. *)
    let const =
        let uninit = Thunk (fun () -> failwith "uninit") in
        fun x ->
            object (self)
                (* initial instance fields cannot refer to self, so temporarily place a dummy thunk and replace it in the initializer *)
                inherit t uninit
                initializer
                    thunk <- Const ( x, self#make_receipt x )
            end

    (** Update a lazy self-adjusting value with a constant value that does not depend on other lazy self-adjusting values. *)
    let update_const m x = m#update_const x

    (** Create a lazy self-adjusting value from a thunk that may depend on other lazy self-adjusting values. *)
    let thunk f = new t (Thunk f)

    (** Update a lazy self-adjusting value with a thunk that may depend on other lazy self-adjusting values. *)
    let update_thunk m f = m#update_thunk f

    (** Local exception to signal memoization hit. *)
    exception MemoHit of t

    (* create memoizing constructors *)
    include MemoN.Make (struct
        type data = R.t
        type t = R.t thunk

        (** Create a memoizing constructor and updater for a lazy self-adjusting value. *)
        let memo =
            fun (type a) (module A : Hashtbl.SeededHashedType with type t = a) f ->
                let module Memotable = Weak.Make (struct
                    type t = A.t * R.t thunk
                    let seed = Random.bits ()
                    let hash ( a, _ ) = A.hash seed a
                    let equal ( a, _ ) ( a', _ ) = A.equal a a'
                end) in
                let memotable = Memotable.create 0 in

                let rec memo x = try
                    let f () = f memo x in
                    object (self)
                        inherit t (Thunk f)
                        initializer
                            let binding = ( x, self ) in
                            let _, other as binding' = Memotable.merge memotable binding in
                            if binding' == binding then
                                let unmemo () = Memotable.remove memotable binding in
                                thunk <- MemoThunk ( f, unmemo )
                            else
                                raise (MemoHit other)
                    end
                with MemoHit other ->
                    other
                in

                let update_memo m x =
                    let f () = f memo x in
                    let binding = ( x, m ) in
                    let _, other as binding' = Memotable.merge memotable binding in
                    if binding' == binding then begin
                        let unmemo () = Memotable.remove memotable binding in
                        m#update_memo_thunk f unmemo
                    end else if m != other then
                        m#update_thunk f
                in
                ( memo, update_memo )
    end)
end
