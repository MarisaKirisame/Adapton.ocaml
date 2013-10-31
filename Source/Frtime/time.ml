
type time = float

let get_time = Unix.gettimeofday

let cmp_time = (<)

let min_time (l : time list) : time = 
	List.fold_left min max_float l

