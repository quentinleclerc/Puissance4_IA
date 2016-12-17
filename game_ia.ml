open Game

let memory = Hashtbl.create 100000

let cache f =
  fun arg ->
    if Hashtbl.mem memory arg then Hashtbl.find memory arg
    else
      begin
        let res = f arg in
        Hashtbl.add memory arg res ;
        res
      end

let find_max  pl list_mov_res =
	let rec loop acu  = function
			| [] -> acu  
			| (m, r) :: t -> 
				match acu with
				| (_, racu) ->
					match compare pl r racu with
						| Greater -> loop acu t
						| _ -> loop (m,r) t
	in loop (List.hd list_mov_res) list_mov_res 


(* Stupid IA: it take the first possible valid move. *)
let rec best_move_without_cache state =
	match result state with 
		| Some x -> (None, x)
		| None -> 
			let all_mov = List.filter (is_valid state) (all_moves state) in
			let current_player = turn state	in

			let rec calc_list_mov_res l =
				match l with
				| [] -> []
				| h :: t -> (h, snd (cache best_move_without_cache (play state h) ) )  :: calc_list_mov_res t
			in 

			match find_max current_player (calc_list_mov_res all_mov) with
			| (m, r) -> (Some m, r)



let best_move = cache best_move_without_cache 

