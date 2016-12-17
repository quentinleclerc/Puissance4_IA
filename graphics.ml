open Graphics;;
open Printf;;
open_graph "";;
while 0<1 do
  let e = wait_next_event [Button_down; Key_pressed] in
  printf "x=%d y=%d %s %s %c %02x\n" e.mouse_x e.mouse_y
    (if e.button then "down" else "up")
    (if e.keypressed then "char" else "nochar")
    e.key (int_of_char e.key); flush stdout;         
done

