open Game 
open Graphics
open Printf
open Graphic_image 
open Images 
open Png
open Gif 
;;

open_graph "" ;;
resize_window 686 687 ;;
set_window_title "Puissance 4" ;;
x = 0 y = 640 

let plat = load_as_rgb24 "puissance4.png" [] 
let jeton_r = load_as_rgb24 "jeton_rouge.png" [] 
let jeton_j = load_as_rgb24 "jeton_jaune.png" []
;;

while 0<1 do
  let e = wait_next_event [Button_down; Key_pressed] in
  	draw_image plat 0 0 ;
	draw_image jeton_r 10 10 ;
	draw_image jeton_j 100 10 ;  
  printf "x=%d y=%d %s %s %c %02x\n" e.mouse_x e.mouse_y
    (if e.button then "down" else "up")
    (if e.keypressed then "char" else "nochar")
    e.key (int_of_char e.key); flush stdout;         
done
