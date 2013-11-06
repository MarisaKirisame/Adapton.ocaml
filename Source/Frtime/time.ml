
type time = float

let to_seconds (t : time) : float = t
let from_seconds (s : float) : time = s

let get_time = Unix.gettimeofday

let cmp_time = (<)

let min_time (l : time list) : time = 
	List.fold_left min max_float l

let max_time (l : time list) : time = 
	List.fold_left max min_float l

module TimeType (M : Adapton.Signatures.SAType) = M.Make( Adapton.Types.Float)

