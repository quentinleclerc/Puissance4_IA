open Functory.Network
open Functory.Network.Same

let () = Functory.Control.set_debug true 

(*****************************************************)

let hostname = Unix.gethostname ()

(* The map function *)
let zemap n = Printf.sprintf "#%d (computed on %s)" n hostname

(* The fold function *)
let zefold acu zm = (Printf.sprintf "%s (concat on %s)" zm hostname ) ^ "\n" ^ acu

(* Build a test list. *)
let rec loop n acu = if n = 0 then acu else loop (n-1) (n :: acu)

(* This function main is launched by the _master_ only. *)
let main () =
  Printf.printf "Main\n%!" ;

  (* Declare machines as workers. 
   * You have to launch the current program (in worker mode) on these machines by yourself. *)
  declare_workers ~n:2 "localhost" ;
  declare_workers ~n:2 "localhost" (*"geitp109-5"*) ;

  let bigl = loop 100 [] in
  Printf.printf "List computed\n%!" ;
  
  (* Distributed computation *)
  let result = map_fold_ac ~f:zemap ~fold:zefold "" bigl in

  Printf.printf "Computation done : result = %s\n%!" result ;
  ()


(*** Entry point of the program. ***)
let () =
  
  (* Sys.argv are the command-line arguments. *)
  match Sys.argv with

  (* If there is one argument equal to "master" *)
  | [| _ ; "master" |] -> 
     Printf.printf "I am the master.\n%!" ;
     main ()

  (* Otherwise, we are a worker. *)
  | _ -> 
     Printf.printf "I am a worker.\n%!" ;
     Functory.Network.Same.Worker.compute ()

