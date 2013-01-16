open Ocamlbuild_plugin

let _ = dispatch begin function
    | After_rules ->
        (* missing rule as of OCamlbuild 4.00.1 *)
        flag [ "ocaml"; "rectypes"; "pack" ] (A "-rectypes");
    | _ ->
        ()
end
