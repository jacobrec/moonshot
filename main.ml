open Raylib
open Moonshot



let setup () =
  init_window screen_width screen_height "test1";
  set_target_fps 60;

  let vc = Vector2.create in
  let bodies = [
      {Moonshot.Body.pos=vc 0.0 0.0; mass=900.0; radius=10.0;};
      {Moonshot.Body.pos=vc 30.0 0.0; mass=300.0; radius=5.0;};
    ] in

  let movables = [] in
  let enemies = [
      {Moonshot.Enemy.loc={Moonshot.Body.body={
                             Moonshot.Body.pos=vc 30.0 (-6.0); mass=1.0; radius=1.0;};
                  vel=vc 0.0 0.0}; action=Standing}
    ] in
  let cam = Camera2D.create (vc (float_of_int (screen_width / 2)) (float_of_int (screen_height / 2)))
              (vc 0.0 0.0) 0.0 1.0 in (* offset target rotation zoom *)
  let make_player x y =
    {
      Moonshot.Player.feet={
        Moonshot.Body.body={pos=vc x y; mass=  10.0; radius=0.5;}; vel=vc 1.0 0.0};
      Moonshot.Player.head={
          Moonshot.Body.body={pos=vc x (y +. 1.0); mass= -3.5; radius=0.5;}; vel=vc (-1.0) 0.0};
      input=Moonshot.Player.None
    } in
  { Moonshot.Model.static=bodies;
    fading=[];
    enemies;
    cam;
    player=make_player (-30.0) 0.0;
    bullets=movables }

let rec loop model =
  if window_should_close () then close_window () else
    model
    |> Input.input
    |> Update.update (get_frame_time ())
    |> Draw.draw
    |> loop


let () = setup () |> loop
