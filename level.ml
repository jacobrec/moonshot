open Moonshot

let vc = Raylib.Vector2.create

let make_level px py bodies enemies =
  let movables = [] in
  let cam = Raylib.Camera2D.create (vc (float_of_int (screen_width / 2)) (float_of_int (screen_height / 2)))
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
    player=make_player px py;
    bullets=movables }


let level_zero _ =
  let bodies = [
      {Moonshot.Body.pos=vc 0.0 0.0; mass=900.0; radius=10.0;};
      {Moonshot.Body.pos=vc 30.0 0.0; mass=300.0; radius=5.0;};
    ] in
  let enemies = [
      {Moonshot.Enemy.loc={Moonshot.Body.body={
                             Moonshot.Body.pos=vc 30.0 (-6.0); mass=1.0; radius=1.0;};
                  vel=vc 0.0 0.0}; action=Standing}
    ] in
  make_level (-30.0) 0.0 bodies enemies

let level_one _ =
  let bodies = [
      {Moonshot.Body.pos=vc (-10.0) ( 0.0); mass=500.0; radius=7.0;};
      {Moonshot.Body.pos=vc ( 10.0) ( 10.0); mass=500.0; radius=7.0;};
      {Moonshot.Body.pos=vc ( 10.0) (-10.0); mass=500.0; radius=7.0;};
    ] in
  let enemies = [
      {Moonshot.Enemy.loc={Moonshot.Body.body={
                             Moonshot.Body.pos=vc 30.0 (-6.0); mass=1.0; radius=1.0;};
                  vel=vc 0.0 0.0}; action=Standing}
    ] in
  make_level (-30.0) 0.0 bodies enemies


let load i =
  let levels = [
      level_zero;
      level_one;
    ] in
  match List.nth_opt levels i with
  | Some l -> l ()
  | _ -> level_zero ()

