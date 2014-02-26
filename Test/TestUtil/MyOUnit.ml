
(* include the test type *)
type test = OUnit.test = TestCase of (unit -> unit) | TestList of test list | TestLabel of string * test

(* redefine the type of OUnit.bracket to catch some errors *)
let bracket : (unit -> 'a) -> ('a -> unit) -> ('a -> unit) -> unit -> unit = OUnit.bracket

exception MyOUnitFailure
exception MyOUnitSkip of string

(* error message buffer *)
let buffer = Buffer.create 4096
let formatter = Format.formatter_of_buffer buffer
let assert_log format = Format.fprintf formatter format


(* test wrapper that sets up the log and reports unexpected exceptions *)
let wrap_test testfn = fun () ->
    (* enable backtrace *)
    let prev_backtrace = Printexc.backtrace_status () in
    Printexc.record_backtrace true;
    (* reset printing boxes *)
    Format.pp_print_flush formatter ();
    Buffer.clear buffer;
    (* indent *)
    assert_log "@\n  @[";
    (* run test *)
    try testfn () with e -> begin
        begin match e with
            | MyOUnitFailure -> ()
            | MyOUnitSkip _ -> raise e
            | _ ->
                (* report unexpected exceptions *)
                let b = Str.split (Str.regexp "\n") (Printexc.get_backtrace ()) in
                let line_printer ff l =
                    ignore (List.fold_left (fun b e -> Format.fprintf ff "%(%)%s" b e; "@\n") "" l)
                in
                assert_log "@[<2>Unexpected exception: %s@\n@[<v>%a@]@]" (Printexc.to_string e) line_printer b
        end;
        Printexc.record_backtrace prev_backtrace;
        Format.pp_print_flush formatter ();
        ignore (OUnit.assert_failure (Buffer.contents buffer))
    end;
    Printexc.record_backtrace prev_backtrace


(* test wrapper that forks the process before running the test *)
let fork_test testfn = fun () ->
    let helper () =
        (* close stdin: automated tests really shouldn't rely on input *)
        Unix.close Unix.stdin;
        try wrap_test testfn (); `None with
            | Failure s -> `Failure s
            | MyOUnitSkip s -> `Skip s
    in
    try match UnixPlus.fork_call helper with
        | `None -> ()
        | `Failure s -> failwith s
        | `Skip s -> OUnit.skip_if true s
    with UnixPlus.ForkCallException e ->
        raise e


(** Test wrapper that creates a temporary file and passes it to the test. The temporary file will be automatically
    deleted after the test.
        @param base is the base name to use for the temporary file
        @param ext is the extension to use for the temporary file
        @param test is the test to apply to the file, given as a tuple of the full path and an out channel
        @return a [TestCase] applied to the file
*)

let test_with_temp_file base ext test =
    bracket begin fun () ->
        Filename.open_temp_file base ext
    end test (fun (filename, _) -> Unix.unlink filename)


(** Test wrapper that creates a temporary file, fills it some content, and passes it to the test. The temporary file
    will be automatically deleted after the test.
        @param base is the base name to use for the temporary file
        @param ext is the extension to use for the temporary file
        @param content is a string to fill the file with
        @param test is the test to apply to the file, given as a full path
        @return a [TestCase] applied to the file
*)
let test_string_as_file base ext content test =
    test_with_temp_file base ext begin fun (path, out) ->
        output_string out content;
        close_out out;
        test path
    end


(** Convenience test helper for testing all files in a directory recursively.
        @param path is the path to the directory in which find files
        @param test is the test to apply to each file found, given as a path relative from the original directory
        @return a [TestList] of tests applied to files found
 *)
let test_dir path test =
    let rec test_dir relpath dir tests =
        (* first, sort down *)
        Array.sort (fun x y -> -(String.compare x y)) dir;
        (* then, iterate left-to-right, so output list will become sorted up *)
        Array.fold_left begin fun tests filename ->
            if filename.[0] = '.' then (* skip hidden files *)
                tests
            else begin
                let relpath = Filename.concat relpath filename in
                let fullpath = Filename.concat path relpath in
                if Sys.is_directory fullpath then
                    test_dir relpath (Sys.readdir fullpath) tests (* recurse into directories *)
                else
                    (test relpath)::tests (* add files *)
            end
        end tests dir
    in
    TestList (if Sys.file_exists path then test_dir "" (Sys.readdir path) [] else [])


(* redefine OUnit functions to use the above buffer, test wrapper, and Format-based printer *)
let (>:) = OUnit.(>:)
let (>::) label testfn = OUnit.(>::) label (fork_test testfn)
let (>:::) = OUnit.(>:::)
let run_test_tt_main = OUnit.run_test_tt_main

let test_permutations list test =
    if List.length list > 4 then invalid_arg "permutation limited to 4 items";
    (* from: http://caml.inria.fr/pub/ml-archives/caml-list/2001/06/d4059d1cf784e6eeff6978245ffcb319.fr.html *)
    let rec distribute elt = function
        | (hd::tl) as list -> (elt::list)::(List.map (fun x -> hd::x) (distribute elt tl))
        | [] -> [ [elt] ]
    and permute = function
        | x::rest -> List.flatten (List.map (distribute x) (permute rest))
        | [] -> [ [] ]
    in
    let permutations = permute list in
    TestList (List.map test permutations)

let skip msg =
    raise (MyOUnitSkip msg)

let assert_failure format =
    Format.kfprintf (fun _ -> raise MyOUnitFailure) formatter format

let assert_bool msg flag =
    if not flag then assert_failure "@[%s@]@." msg

let assert_string msg =
    if not (msg = "") then assert_failure "@[%s@]@." msg

let assert_raises ?msg exn fn =
    try
        fn ();
        let exn = Printexc.to_string exn in
        begin match msg with
            | Some m -> assert_failure "@[%s@]@\n @[<2>expected exception not raised:@ %s@]" m exn
            | None -> assert_failure "@[<2>expected exception not raised:@ %s@]" exn
        end
    with exn' ->
        if exn' <> exn then
            let exn = Printexc.to_string exn in
            let exn' = Printexc.to_string exn' in
            match msg with
                | Some m -> assert_failure "@[%s@]@\n @[@[<2>expected exception:@ %s@]@ @[<2>but got:@ %s@]@]" m exn exn'
                | None -> assert_failure "@[@[<2>expected exception:@ %s@]@ @[<2>but got:@ %s@]@]" exn exn'

let assert_equal ?(eq=Pervasives.(=)) ?printer ?msg expected actual =
    if not (eq expected actual) then begin match printer, msg with
        | Some p, Some m -> assert_failure "@[%s@]@\n  @[@[<2>expected:@ %a@]@ @[<2>but got:@ %a@]@]" m p expected p actual
        | Some p, None -> assert_failure "@[@[<2>expected:@ %a@]@ @[<2>but got:@ %a@]@]" p expected p actual
        | None, Some m -> assert_failure "@[%s@]@\n  not equal" m
        | None, None -> assert_failure "not equal"
    end

let assert_at_least ?(cmp=Pervasives.compare) ?printer ?msg expected actual =
    if cmp expected actual > 0 then begin match printer, msg with
        | Some p, Some m -> assert_failure "@[%s@]@\n  @[@[<2>expected at least:@ %a@]@ @[<2>but got:@ %a@]@]" m p expected p actual
        | Some p, None -> assert_failure "@[@[<2>expected at least:@ %a@]@ @[<2>but got:@ %a@]@]" p expected p actual
        | None, Some m -> assert_failure "@[%s@]@\n  less than" m
        | None, None -> assert_failure "less than"
    end

let assert_at_most ?(cmp=Pervasives.compare) ?printer ?msg expected actual =
    if cmp expected actual < 0 then begin match printer, msg with
        | Some p, Some m -> assert_failure "@[%s@]@\n  @[@[<2>expected at most:@ %a@]@ @[<2>but got:@ %a@]@]" m p expected p actual
        | Some p, None -> assert_failure "@[@[<2>expected at most:@ %a@]@ @[<2>but got:@ %a@]@]" p expected p actual
        | None, Some m -> assert_failure "@[%s@]@\n  more than" m
        | None, None -> assert_failure "more than"
    end


(* assert that a result matches a pattern (given as a function with a refutable pattern) *)
let assert_match ?printer expected_match actual =
    try expected_match actual with Match_failure _ -> begin match printer with
        | Some p -> assert_failure "@[<2>Did not match:@ %a@]" p actual
        | None -> assert_failure "Match failure"
    end


(* assert that collection contains elements in list, given a membership function mem *)
let assert_mem ?printer mem list collection =
    let bad = List.filter (fun el -> not (mem el collection)) list in
    if bad != [] then begin match printer with
        | Some p -> assert_failure "@[<v2>Not in:@ %a@]" (fun ff -> List.iter (Format.fprintf ff "@[%a@]@ " p)) bad
        | None -> assert_failure "Not in collection"
    end


(* assert a time limit for the test (note: uses [Unix.setitimer Unix.ITIMER_PROF]) *)
let assert_time_limit time_limit fn =
    let old_handler = Sys.signal Sys.sigprof (Sys.Signal_handle (fun _ -> assert_failure "Exceeded time limit (%f seconds)" time_limit)) in
    ignore (Unix.setitimer Unix.ITIMER_PROF { Unix.it_interval = 0.; Unix.it_value = time_limit; });
    try
        let result = fn () in
        ignore (Unix.setitimer Unix.ITIMER_PROF { Unix.it_interval = 0.; Unix.it_value = 0.; });
        Sys.set_signal Sys.sigprof old_handler;
        result
    with e ->
        ignore (Unix.setitimer Unix.ITIMER_PROF { Unix.it_interval = 0.; Unix.it_value = 0.; });
        Sys.set_signal Sys.sigprof old_handler;
        raise e


(*
 * Convenience functions comparing as well as printing option and list types
 *)

let option_equal eq x y = match x, y with
    | Some x, Some y -> eq x y
    | None, None -> true
    | _ -> false

let option_printer printer ff = function
    | Some x -> Format.fprintf ff "Some (@[%a@]@,)" printer x
    | None -> Format.fprintf ff "None"

let rec list_equal eq x y = match x, y with
    | x::xs, y::ys when eq x y -> list_equal eq xs ys
    | [], [] -> true
    | _ -> false

let list_printer printer sep ff list =
    ignore (List.fold_left (fun b x -> Format.fprintf ff "%(%)@[%a@]" b printer x; sep) "" list)

let array_printer printer sep ff arr =
    ignore (Array.fold_left (fun b x -> Format.fprintf ff "%(%)@[%a@]" b printer x; sep) "" arr)


(*
 * QuickCheck
 *)

module QC = struct
    (* generators *)
    let bool = object
        method generate rng size = Random.State.bool rng
        method print = Format.pp_print_bool
    end
    let char = object
        method generate rng size = char_of_int (Random.State.int rng 256)
        method print = Format.pp_print_char
    end
    let string = object
        method generate rng size =
            let n = Random.State.int rng size in
            let s = String.create n in
            for i = 0 to n - 1 do s.[i] <- char_of_int (Random.State.int rng 256) done;
            s
        method print = Format.pp_print_string
    end
    let int = object
        method generate rng size = -size + Random.State.int rng (size * 2)
        method print = Format.pp_print_int
    end
    let float = object
        method generate rng size = let size = float_of_int size in -.size +. Random.State.float rng (size *. 2.)
        method print = Format.pp_print_float
    end
    let pair gx gy = object
        method generate rng size = ( gx#generate rng size, gy#generate rng size )
        method print ff ( x, y ) = Format.fprintf ff "(@[%a@],@ @[%a@]@,)" gx#print x gy#print y
    end
    let triple gx gy gz = object
        method generate rng size = ( gx#generate rng size, gy#generate rng size, gz#generate rng size )
        method print ff ( x, y, z ) = Format.fprintf ff "(@[%a@],@ @[%a@],@ @[%a@]@,)" gx#print x gy#print y gz#print z
    end
    let quad gw gx gy gz = object
        method generate rng size = ( gw#generate rng size, gx#generate rng size, gy#generate rng size, gz#generate rng size )
        method print ff ( w, x, y, z ) = Format.fprintf ff "(@[%a@],@ @[%a@],@ @[%a@],@ @[%a@]@,)" gw#print w gx#print x gy#print y gz#print z
    end
    let list ?(sep=format_of_string ";@ ") gx = object
        method generate rng size =
            let rec make n acc = if n > 0 then make (pred n) (gx#generate rng size::acc) else acc in
            make (Random.State.int rng size) []
        method print ff = Format.fprintf ff "[@[%a@]@,]" (list_printer gx#print sep)
    end
    let array ?(sep=format_of_string ";@ ") gx = object
        method generate rng size = Array.init (Random.State.int rng size) (fun k -> gx#generate rng size)
        method print ff = Format.fprintf ff "[|@[%a@]@,|]" (array_printer gx#print sep)
    end
    let option gx = object
        method generate rng size = if Random.State.bool rng then Some (gx#generate rng size) else None
        method print ff = option_printer gx#print ff
    end

    (* test predicate *)
    let forall ?(rng=Random.State.make_self_init ()) ?(count=100) ?(size=fun n -> 3 + n / 2) ?(incl=[]) ?(where=fun _ -> true) input testfn = fun () ->
        let test x =
            try
                testfn x
            with MyOUnitFailure as e ->
                assert_log "@\n@[<2>on input: @[%a@]@]" input#print x;
                raise e
        in
        List.iter test incl;
        let rec gen k =
            let x = input#generate rng (size k) in
            if where x then x else gen (k + 1)
        in
        for k = 1 to count do
            test (gen k)
        done
end
