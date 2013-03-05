(** Lists of all modules for self-adjusting values and applications. *)

(** List of all names and modules for self-adjusting values. *)
let sa_list = [
    ( "LazySABidi", (module LazySABidi : Signatures.SAType) );
    ( "LazySABidiObject", (module LazySABidiObject : Signatures.SAType) );
    ( "LazySANaive", (module LazySANaive : Signatures.SAType) );
    ( "LazySAObject", (module LazySAObject : Signatures.SAType) );
    ( "EagerSATotalOrder", (module EagerSATotalOrder : Signatures.SAType) );
    ( "NonSAEager", (module NonSAEager : Signatures.SAType) );
    ( "NonSALazy", (module NonSALazy : Signatures.SAType) );
]

(** List of all names and modules for self-adjusting lists. *)
let salist_list = List.map begin fun ( name, sa ) ->
    ( "SAList (" ^ name ^ ")", (module SAList.Make ((val sa : Signatures.SAType)) : Signatures.SAListType)  )
end sa_list
