open Graphics
open Unix 

(*let set_background_color color =
  set_color color;
  fill_rect 0 0 (size_x ()) (size_y ())

let () =
  open_graph " 640x480";  (* Ouvre une fenêtre de 640x480 pixels *)

  set_background_color (rgb 100 100 0);  (* Définit la couleur de fond une fois *)

  let pos_x = ref 1 in
  while true do
    Unix.sleepf (1.0 /. 60.0);  (* Pause pour maintenir le nombre de fps *)

    (* Dessinez quelque chose ici *)
    set_background_color (rgb 100 100 0);
    set_color (rgb 50 50 50);
    fill_rect !pos_x !pos_x 50 50;

    pos_x := !pos_x + 1;
  done*)

let color = rgb 100 100 0 in
Graphics.set_color color;
Graphics.fill_rect 0 0 (Graphics.size_x ()) (Graphics.size_y ());
Graphics.draw_rect 100 100 100 100 ;
(*Graphics.fill_rect p1 p2 p3 p4 ;*)