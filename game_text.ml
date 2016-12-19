open Gamebase

(* These types are abstract in game.mli *)
type case = Rouge | Jaune | Vide 

type state = case array array * player

type move = int (* Column number where the player want to put the token *)

type result = Win of player | Drawn

(* Create a 6x7 grid *) 
let lignes = 4

let colonnes = 4

let plateau_init = Array.make_matrix lignes colonnes Vide ;;


(* Printers *)
let case2s case = 
  match case with  
    | Jaune -> "J" 
    | Rouge -> "R"
    | Vide -> " " 

let state2s (plat, play) = Printf.sprintf "Current \n%s // %s to play" (matrix2s plat case2s) (player2s play) 

let move2s n = Printf.sprintf "Jeton inséré dans la colonne %d" (n + 1) 

let result2s st = 
  match st with 
  | (Win p) -> (player2s p) ^ " wins"
  | Drawn -> "Tie game"

(* Reader *)
let readmove s = try Some ((int_of_string s) - 1) with _ -> None

(* You have to provide these. *)
let initial = (plateau_init, Comput) 

let turn (_, p) = p

let is_valid st mov = 
  	match st with 
    	| (plat, _) -> (mov >= 0)  && (mov < colonnes) && (plat.(lignes - 1).(mov) = Vide)

let find_height st mov = 
  let rec aux acu = 
    match st with
      | (plat, _) ->  
          if plat.(acu).(mov) = Vide then acu 
          else aux (acu + 1)
  in aux 0

let play st mov =

  match st with 
  | (plat, play) ->
    let v = if play = Human then Rouge else Jaune in 
    if (is_valid st mov)
      then 
        let aux =
          let new_plat = clone_matrix plat in
          assert (new_plat.(find_height st mov).(mov) = Vide);
          (*Printf.printf "find_height st mov = %d\n%!" (find_height st mov) ;*)
          new_plat.(find_height st mov).(mov) <- v ;
          new_plat
        in 
      (aux, next play)
    else st


let rec make_int_list taille =
  if taille = 0 then [0] 
  else taille :: make_int_list (taille - 1)

let all_moves st = make_int_list (colonnes - 1)


(* Test the 4-cases alignement for one case, which coordinate is (x,y) in a m matrix,
and return true if one is found, false either way *)
let test_alignement m (l, c) =
if m.(l).(c) = Vide then false
else 
  let test_diago_sup_droite = 
    if c <= (colonnes - 4) && l <= (lignes - 4) 
    then m.(l).(c) = m.(l+1).(c+1) && m.(l+1).(c+1) = m.(l+2).(c+2) && m.(l+2).(c+2) = m.(l+3).(c+3) 
    else false
  in  
  
  let test_diago_sup_gauche = 
    if l <= (lignes - 4) && c >= 3 
    then m.(l).(c) = m.(l+1).(c-1) && m.(l+1).(c-1) = m.(l+2).(c-2) && m.(l+2).(c-2) = m.(l+3).(c-3)
    else false
  in 

  let test_colonne_haut =
    if l <= (lignes - 4) 
    then m.(l).(c) = m.(l+1).(c) && m.(l+1).(c) = m.(l+2).(c) && m.(l+2).(c) = m.(l+3).(c) 
    else false
  in

  let test_ligne_droite =
    if c <= (colonnes - 4)
    then m.(l).(c) = m.(l).(c+1) && m.(l).(c+1) = m.(l).(c+2) && m.(l).(c+2) = m.(l).(c+3)
    else false 
  in 
  test_diago_sup_gauche || test_diago_sup_droite || test_ligne_droite || test_colonne_haut


(* each cell is identified by a number from 0 to (number of lines * number of columns) - 1
as in this example : nb_lignes = 3, nb_columns = 7
14 15 16 17  18  19   20  | 21
7  8  9  10  11  12   13 
0  1   2   3   4   5   6   
*)

(* used to go through a matrix from (0,0) to (lines, columns) *)
let num_to_coord num = 
  let l = (num / colonnes)  in
  let c = num mod colonnes in 
  (l, c)

(* check if the board si full, returns boolean *)
let check_m_full (plat, play) =
  if (find_cell plat (fun x -> x = Vide) = None) then true else false 

let result (plat, play) =  
    let rec aux acu =
      if acu = (lignes * colonnes) then 
          if check_m_full (plat, play) then Some Drawn
          else None 
      else 
          if (test_alignement plat (num_to_coord acu)) then Some (Win (next play))
          else aux (acu+1)  
    in aux 0

(* This type was given in game.mli.
 * We have to repeat it here. *)
type comparison = Equal | Greater | Smaller

let compare pla r1 r2 = match pla, r1, r2 with
    | (pla, Drawn, Win pl) -> if pla = pl then Greater else Smaller
    | (pla, Win pl, Drawn) -> if pla = pl then Smaller else Greater
    | (_, Drawn, Drawn) -> Equal
  	| (pla, Win pl2, Win pl3) -> if pl2 == pl3 then Equal
      								else if pla == pl3 then Greater
      								else Smaller
        								
let worst_for pl = Win (next pl)