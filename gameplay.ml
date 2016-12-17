open Gamebase
open Game
open Functory.Network
open Functory.Network.Same


let () = Functory.Control.set_debug true 

(********************************************************)
(* Interactively ask for the player's move. 
 * Returns Some move, or None when the move is invalid. *)

let ask_move state =
  Printf.printf "  => Your move ? %!" ;  
  let line = read_line () in

  match readmove line with
  | None ->
    Printf.printf "\n Cannot read this move: %s\n\n%!" line ;
    None
    
  | Some mov ->
    if not (is_valid state mov) then
      begin
        Printf.printf "\n This move is invalid: %s\n\n%!" (move2s mov) ;
        None
      end
    else Some mov

(* Get the move from the IA. *)
let ia_move state =
  let (mov, _) = Game_ia.best_move state in
  match mov with
  | None -> assert false
  | Some m -> m
  
(*** Each player in turn. ***)
    
let rec run with_ia state =
  
  (* Print state & which player to play. *)
  Printf.printf "\n%s\n %s to play.\n\n%!" (state2s state) (player2s (turn state)) ;
  
  match result state with
  | Some r ->
    (* Game is finished. Print result. *)
    Printf.printf "*** %s ***\n%!" (result2s r) ;
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


(* ANCIEN MAIN *)
let () = run true initial

(********************************************************)
(* **************** Distributed part ****************** *)
let hostname = Unix.gethostname ()

(* The map function *)
let themap n = Printf.sprintf "#%d (computed on %s)" n hostname

(* The fold function *)
let thefold acu zm = (Printf.sprintf "%s (concat on %s)" zm hostname ) ^ "\n" ^ acu

let main () = 
  Printf.printf "Main\n%!" ;

  (* Declare machines as workers. 
  * You have to launch the current program (in worker mode) on these machines by yourself. *)
  declare_workers ~n:2 "localhost" ;
  declare_workers ~n:2 "localhost" ;

  
  (* Distributed computation *)
  let result = map_fold_ac ~f:themap ~fold:thefold "" bigloop in

  Printf.printf "Computation done \n%!" result ;
  ()

(*** Entry point of the program. ***)
let () =
  
  (* Sys.argv are the command-line arguments. *)
  match Sys.argv with

  (* If there is one argument equal to "master" *)
  | [| _ ; "master" |] -> 
     Printf.printf "I am the master.\n%!" ;
     

  (* Otherwise, we are a worker. *)
  | _ -> 
     Printf.printf "I am a worker.\n%!" ;
     Functory.Network.Same.Worker.compute ()


