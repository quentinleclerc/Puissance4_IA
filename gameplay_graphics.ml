open Gamebase
open Game_graphics
open Graphics
;;


init_graph () ;;

(* Interactively ask for the player's move. 
 * Returns Some move, or None when the move is invalid. *)

let ask_move state =
  Printf.printf "  => Your move ? %!" ;
  match readmove () with 
  | None ->
      Printf.printf "\n Cannot read this move \n\n!" ;
      None
  | Some m -> 
    if not (is_valid state m) then 
        begin
          Printf.printf "\n This move is invalid: %s\n\n" (move2s m);
          None          
        end
      else Some m

(* Get the move from the IA. *)
let ia_move state =
  let (mov, _) = Game_ia.best_move state in
  match mov with
  | None -> assert false
  | Some m -> m
  
(*** Each player in turn. ***)


(* run part *)
let rec run with_ia state = 

(* Print state & which player to play. *)
Printf.printf "\n%s\n %s to play.\n\n%!" (state2s state) (player2s (turn state)) ;
(* affiche_matrice (get_matrix (state)) ;*)
state2graph state ;


match result state with
| Some r ->
  (* Game is finished. Print result. *)
  Printf.printf "*** %s ***\n%!" (result2s r) ;
(*moveto (size_x () / 2) (size_y () / 2) ;
  set_text_size 100 ;
  draw_string (result2s r) ;
*)
  win2graph r ;
  wait_next_event [Button_down] ; 
  ()
  
| None ->
  (* Game is not finished. Play one turn. *)

  let state' =
    if with_ia && turn state = Comput
    then play state (ia_move state) 
    
    else
      begin match ask_move state with
        | None -> state (* Invalid move, play same state again. *)
        | Some mov -> play state mov
      end
  in
  run with_ia state'


(* initialize graphics part and run the game *)
let () = run true initial ;;


