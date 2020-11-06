open Moonshot

let vc = Raylib.Vector2.create

let make_level name px py bodies enemies star_reqs =
  let movables = [] in
  let cam = Raylib.Camera2D.create (vc (float_of_int (screen_width / 2)) (float_of_int (screen_height / 2)))
              (vc 0.0 0.0) 0.0 1.0 in (* offset target rotation zoom *)
  let make_player x y =
    {
      Player.feet={
        Body.body={pos=vc x y; mass=  10.0; radius=0.5;}; vel=vc 1.0 0.0};
      Player.head={
          Body.body={pos=vc x (y +. 1.0); mass= -3.5; radius=0.5;}; vel=vc (-1.0) 0.0};
      input=Player.None;
      Player.health=6;
    } in
  { Model.static=bodies;
    name;
    fading=[];
    enemies;
    cam;
    player=make_player px py;
    runtime=0.0;
    shots_taken=0;
    longest_bullet=0.0;
    star_reqs;
    bullets=movables }


(* Level design plan *)
(* There will be multiple worlds
 * Each world will have 10 + 2 levels.
 *   10 Regular, needed to be completed before next world and 2 weird ones for fun.
 * Every level should be able to be completed in under a minute
 * Completing a level should be easy, 3 stars should be hard
 * The first few levels from each world should introduce concepts
 * Then middle few should be relativly easy to get 3 stars
 * The last few should be hard to get 3 stars
 * Each world should introduce one or more new concepts
 *
 * World 1: Intro
 * Introduce shooting, moving, jumping, double jumping, planet gravity, and painful planets
 * 1-1) Shooting
 * 1-2) Moving
 * 1-3) Jumping
 * 1-4) Planet gravities
 * 1-5) Double Jumping
 * 1-6) Painful Planets
 * 1-7) Friendly Fire
 * 1-8) Stuck on black holes
 * 1-9) Challenge 1
 * 1-10) Challenge 2
 * 1-C1) Hurty planet special
 * 1-C2) Zero G shooting Challenge
*)
let level_zero _ =
  let bodies = [
      {Body.is_painful=true;
       body={Body.pos=vc 0.0 0.0; mass=900.0; radius=10.0;}};
      {Body.is_painful=false;
       body={Body.pos=vc 30.0 0.0; mass=300.0; radius=5.0;}};
    ] in
  let enemies = [
      {Enemy.loc={Body.body={Body.pos=vc 30.0 (-6.0); mass=1.0; radius=1.0;};
                  vel=vc 0.0 0.0}; action=Standing}
    ] in
  make_level "Zero" (-30.0) 0.0 bodies enemies {health=5; time=3.0; shots=1}

let level_one _ =
  let bodies = [
      {Body.is_painful=false; body={Body.pos=vc 0.0 0.0; mass=900.0; radius=10.0;}};
      {Body.is_painful=false; body={Body.pos=vc 40.0 0.0; mass=1200.0; radius=10.0;}};
      {Body.is_painful=false; body={Body.pos=vc 70.0 (-10.0); mass=600.0; radius=7.0;}};
      {Body.is_painful=false; body={Body.pos=vc 70.0 10.0; mass=600.0; radius=7.0;}};
      {Body.is_painful=false; body={Body.pos=vc 60.0 0.0; mass=100.0; radius=3.0;}};
      {Body.is_painful=true;  body={Body.pos=vc 90.0 (-20.0); mass=500.0; radius=6.0;}};
      {Body.is_painful=true;  body={Body.pos=vc 30.0 30.0; mass=500.0; radius=1.0;}};
      {Body.is_painful=false; body={Body.pos=vc 110.0 30.0; mass=500.0; radius=1.0;}};
    ] in
  let enemies = [
      {Enemy.loc={Body.body={Body.pos=vc 40.0 11.0; mass=1.0; radius=1.0;};
                  vel=vc 0.0 0.0}; action=Standing};
      {Enemy.loc={Body.body={Body.pos=vc 70.0 (-18.0); mass=1.0; radius=1.0;};
                  vel=vc 0.0 0.0}; action=Standing};
      {Enemy.loc={Body.body={Body.pos=vc 110.0 32.0; mass=1.0; radius=1.0;};
                  vel=vc 0.0 0.0}; action=Standing};
      {Enemy.loc={Body.body={Body.pos=vc 110.0 28.0; mass=1.0; radius=1.0;};
                  vel=vc 0.0 0.0}; action=Standing};
    ] in
  make_level "one" (-10.0) 0.0 bodies enemies {health=6; time=12.0; shots=3}

let level_two _ =
  let bodies = [
      {Body.is_painful=false; body={Body.pos=vc (-15.0) (  0.0); mass=500.0; radius=7.0;}};
      {Body.is_painful=false; body={Body.pos=vc ( 15.0) ( 15.0); mass=500.0; radius=7.0;}};
      {Body.is_painful=false; body={Body.pos=vc ( 15.0) (-15.0); mass=500.0; radius=7.0;}};
      {Body.is_painful=false; body={Body.pos=vc ( 30.0) (  0.0); mass=500.0; radius=7.0;}};
      {Body.is_painful=false; body={Body.pos=vc (-55.0) (  0.0); mass=300.0; radius=2.0;}};

      {Body.is_painful=false; body={Body.pos=vc ( 50.0) (  0.0); mass=200.0; radius=4.0;}};
      {Body.is_painful=false; body={Body.pos=vc ( 70.0) (  0.0); mass=200.0; radius=4.0;}};
      {Body.is_painful=false; body={Body.pos=vc ( 85.0) (  7.0); mass=200.0; radius=4.0;}};
      {Body.is_painful=false; body={Body.pos=vc (100.0) ( 14.0); mass=200.0; radius=4.0;}};
      {Body.is_painful=false; body={Body.pos=vc (110.0) ( 25.0); mass=200.0; radius=4.0;}};
      {Body.is_painful=false; body={Body.pos=vc (120.0) ( 38.0); mass=200.0; radius=4.0;}};
      {Body.is_painful=false; body={Body.pos=vc (125.0) ( 55.0); mass=200.0; radius=4.0;}};
    ] in
  let enemies = [
      {Enemy.loc={Body.body={Body.pos=vc (-58.0) 0.0; mass=1.0; radius=1.0;};
                  vel=vc 0.0 0.0}; action=Standing};
      {Enemy.loc={Body.body={Body.pos=vc (0.4) 0.0; mass=1.0; radius=1.0;};
                  vel=vc 0.0 0.0}; action=Standing};
      {Enemy.loc={Body.body={Body.pos=vc 29.0 4.0; mass=1.0; radius=1.0;};
                  vel=vc 0.0 0.0}; action=Standing};
      {Enemy.loc={Body.body={Body.pos=vc 125.0 60.0; mass=1.0; radius=1.0;};
                  vel=vc 0.0 0.0}; action=Standing};
      {Enemy.loc={Body.body={Body.pos=vc 100.0 18.0; mass=1.0; radius=1.0;};
                  vel=vc 0.0 0.0}; action=Standing};
    ] in
  make_level "Two" (-30.0) 0.0 bodies enemies {health=6; time=25.0; shots=4}

let load i =
  let levels = [
      level_zero;
      level_one;
      level_two;
    ] in
  match List.nth_opt levels i with
  | Some l -> l ()
  | _ -> level_zero ()

