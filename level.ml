open Moonshot

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
 * 1-11) Hurty planet special
 * 1-12) Zero G shooting Challenge
 *
 * World 2: Strange new worlds
 * Introduce sticky planets, bouncy planets, reverse gravity planets
 * 2-1) Stuck
 * 2-2) Stuck II
 * 2-3) Stuck III
 * 2-4) Reverse G
 * 2-5) Reverse G II
 * 2-6) Reverse G III
 * 2-7) Bouncy Planet
 * 2-8) Bouncy Planet II
 * 2-9) Bouncy Planet III
 * 2-10) Challenge
 * 2-11) Challenge II
 * 2-12) Challenge III
 *
 * World 3: Hawth: The icy world
 * Introduce icy planets, fire shot powerup, icy enemies
 * 3-1) Slippin
 * 3-2) Slipping II
 * 3-3) Slipping III
 * 3-4) Melting I
 * 3-5) Melting II
 * 3-6) Melting III
 * 3-7) New Enemy I
 * 3-8) New Enemy II
 * 3-9) New Enemy III
 * 3-10) Challenge
 * 3-11) Challenge II
 * 3-12) Challenge III
 *)

let vc = Raylib.Vector2.create

let level_map = Hashtbl.create 24

let world_names =
  ["Getting Started"; "Strange new worlds"; "Hawth: The icy world"]

let make_slime x y =
  { Enemy.loc=
      {Body.body= {Body.pos= vc x y; mass= 1.0; radius= 1.0}; vel= vc 0.0 0.0}
  ; action= Standing
  ; angle= 0.0 }

let make_moving_slime x y vx vy =
  { Enemy.loc=
      {Body.body= {Body.pos= vc x y; mass= 1.0; radius= 1.0}; vel= vc vx vy}
  ; action= Standing
  ; angle= 0.0 }

let make_ice_slime x y =
  { Enemy.loc=
      {Body.body= {Body.pos= vc x y; mass= 1.0; radius= 1.0}; vel= vc 0.0 0.0}
  ; action= Shielded
  ; angle= 0.0 }

let make_level ?(powerups = []) id name start_text px py bodies enemies
    star_reqs =
  let movables = [] in
  let cam =
    Raylib.Camera2D.create
      (vc
         (float_of_int (screen_width / 2))
         (float_of_int (screen_height / 2)))
      (vc 0.0 0.0) 0.0 1.0
  in
  (* offset target rotation zoom *)
  let make_player x y =
    { Player.feet=
        {Body.body= {pos= vc x y; mass= 10.0; radius= 1.0}; vel= vc 0.0 0.0}
    ; Player.head=
        { Body.body= {pos= vc x (y +. 1.0); mass= -3.5; radius= 1.0}
        ; vel= vc 0.0 0.0 }
    ; input= Player.None
    ; Player.health= 6
    ; animation_state= Player.Standing
    ; last_damaged_at= 0.0 }
  in
  let stars = Starfield.create 8 6400 in
  let level =
    { Model.static= bodies
    ; stars
    ; name
    ; id
    ; start_text
    ; fading= []
    ; enemies
    ; cam
    ; powerups
    ; special_shots= []
    ; player= make_player px py
    ; runtime= 0.0
    ; shots_taken= 0
    ; longest_bullet= 0.0
    ; star_reqs
    ; bullets= movables }
  in
  if Hashtbl.mem level_map id then (
    print_endline
      "ERROR: Level id is already taken. Please choose a unique ID" ;
    invalid_arg "make_level" ) ;
  Hashtbl.add level_map id level

let () =
  let bodies = [] in
  let enemies = [] in
  make_level 0 "404" "Level not found" 0.0 0.0 bodies enemies
    {health= 8; time= -10.0; shots= -1}

let blank_level = Hashtbl.find level_map 0

module World1 = struct
  let () =
    (* Level 1 *)
    let bodies =
      [ { Body.surface= Body.Normal
        ; body= {Body.pos= vc (-30.0) 0.0; mass= 900.0; radius= 15.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 30.0 0.0; mass= 1500.0; radius= 15.0} } ]
    in
    let enemies = [make_slime 15.0 0.0] in
    make_level 1 "One"
      "Click and drag away from your player to aim like a slingshot. \
       Release to fire."
      (-15.0) 0.0 bodies enemies
      {health= 6; time= 5.0; shots= 1}

  let () =
    (* Level 2 *)
    let bodies =
      [ { Body.surface= Body.Normal
        ; body= {Body.pos= vc 0.0 0.0; mass= 1800.0; radius= 15.0} } ]
    in
    let enemies = [make_slime 15.0 0.0] in
    make_level 2 "Two"
      "Use A and S to move clockwise, and counter clockwise around the \
       planet you're on"
      (-15.0) 0.0 bodies enemies
      {health= 6; time= 5.0; shots= 1}

  let () =
    (* Level 3 *)
    let bodies =
      [ { Body.surface= Body.Normal
        ; body= {Body.pos= vc 0.0 0.0; mass= 1000.0; radius= 15.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc (-30.0) (-30.0); mass= 1000.0; radius= 15.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc (-60.0) (-60.0); mass= 1000.0; radius= 15.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc (-90.0) (-90.0); mass= 100.0; radius= 6.0} } ]
    in
    let enemies = [make_slime (-85.0) (-85.0)] in
    make_level 3 "Three" "Press space to jump" (-15.0) 0.0 bodies enemies
      {health= 6; time= 10.0; shots= 1}

  let () =
    (* Level 4 *)
    let bodies =
      [ { Body.surface= Body.Normal
        ; body= {Body.pos= vc 0.0 0.0; mass= 400.0; radius= 10.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 35.0 10.0; mass= 600.0; radius= 10.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 65.0 30.0; mass= 1200.0; radius= 10.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 85.0 50.0; mass= 1600.0; radius= 10.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 100.0 75.0; mass= 2100.0; radius= 10.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 115.0 95.0; mass= 2600.0; radius= 10.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 135.0 115.0; mass= 400.0; radius= 5.0} } ]
    in
    let enemies = [make_slime 130.0 110.0] in
    make_level 4 "Four"
      "Different planets have different densities. More mass means more \
       gravity."
      (-15.0) 0.0 bodies enemies
      {health= 6; time= 20.0; shots= 1}

  let () =
    (* Level 5 *)
    let radii = 20.0 in
    let x theta = radii *. Float.cos theta in
    let y theta = radii *. Float.sin theta in
    let bodies =
      [ { Body.surface= Body.Normal
        ; body= {Body.pos= vc (x 0.0) (y 0.0); mass= 400.0; radius= 7.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc (x 2.1) (y 2.1); mass= 600.0; radius= 7.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc (x 4.2) (y 4.2); mass= 600.0; radius= 7.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc (-40.0) 0.0; mass= 800.0; radius= 8.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc (-60.0) 5.0; mass= 300.0; radius= 4.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc (-70.0) (-5.0); mass= 300.0; radius= 4.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc (-80.0) 5.0; mass= 300.0; radius= 4.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc (-100.0) 5.0; mass= 600.0; radius= 3.0} } ]
    in
    let enemies = [make_slime (-103.0) 2.0] in
    make_level 5 "Five"
      "Jump just as you land to double (or triple) jump and go even higher."
      15.0 0.0 bodies enemies
      {health= 6; time= 10.0; shots= 1}

  let () =
    (* Level 6 *)
    let radii = 20.0 in
    let x theta = radii *. Float.cos theta in
    let y theta = radii *. Float.sin theta in
    let bodies =
      [ { Body.surface= Body.Normal
        ; body= {Body.pos= vc (x 0.0) (y 0.0); mass= 400.0; radius= 7.0} }
      ; { Body.surface= Body.Painful
        ; body= {Body.pos= vc (x 2.1) (y 2.1); mass= 600.0; radius= 7.0} }
      ; { Body.surface= Body.Painful
        ; body= {Body.pos= vc (x 4.2) (y 4.2); mass= 600.0; radius= 7.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc (-40.0) 0.0; mass= 800.0; radius= 8.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc (-60.0) 5.0; mass= 300.0; radius= 4.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc (-70.0) (-5.0); mass= 300.0; radius= 4.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc (-80.0) 5.0; mass= 300.0; radius= 4.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc (-100.0) 5.0; mass= 600.0; radius= 3.0} } ]
    in
    let enemies = [make_slime (-103.0) 2.0] in
    make_level 6 "Six" "Some planets are painful to touch" 14.0 2.0 bodies
      enemies
      {health= 6; time= 11.0; shots= 1}

  let () =
    (* Level 7 *)
    let bodies =
      [ { Body.surface= Body.Normal
        ; body= {Body.pos= vc 0.0 0.0; mass= 600.0; radius= 6.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 18.0 0.0; mass= 600.0; radius= 6.0} } ]
    in
    let enemies = [make_slime 12.0 0.0] in
    make_level 7 "Seven"
      "Careful, if you are not, you may get caught by your own shot." 6.0 0.0
      bodies enemies
      {health= 6; time= 3.0; shots= 1}

  let () =
    (* Level 8 *)
    let bodies =
      [ { Body.surface= Body.Normal
        ; body= {Body.pos= vc 0.0 0.0; mass= 600.0; radius= 8.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 20.0 10.0; mass= 1200.0; radius= 2.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 50.0 0.0; mass= 100.0; radius= 2.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 20.0 (-10.0); mass= 100.0; radius= 10.0} } ]
    in
    let enemies = [make_slime 45.0 0.0] in
    make_level 8 "Eight"
      "Some celetial bodies are extremely dense. You can restart the level \
       through the pause menu (press p)"
      6.0 0.0 bodies enemies
      {health= 6; time= 8.0; shots= 1}

  let () =
    (* Level 9 *)
    let bodies =
      [ { Body.surface= Body.Normal
        ; body= {Body.pos= vc 0.0 0.0; mass= 900.0; radius= 10.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 40.0 0.0; mass= 1200.0; radius= 10.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 70.0 (-10.0); mass= 600.0; radius= 7.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 70.0 10.0; mass= 600.0; radius= 7.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 60.0 0.0; mass= 100.0; radius= 3.0} }
      ; { Body.surface= Body.Painful
        ; body= {Body.pos= vc 90.0 (-20.0); mass= 500.0; radius= 6.0} }
      ; { Body.surface= Body.Painful
        ; body= {Body.pos= vc 30.0 30.0; mass= 500.0; radius= 1.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 110.0 30.0; mass= 500.0; radius= 1.0} } ]
    in
    let enemies =
      [ make_slime 40.0 11.0
      ; make_slime 70.0 (-18.0)
      ; make_slime 110.0 32.0
      ; make_slime 110.0 28.0 ]
    in
    make_level 9 "Nine"
      "You've learned all there is. Good luck. See if you can 3 star all \
       the levels"
      (-10.0) 0.0 bodies enemies
      {health= 6; time= 12.0; shots= 3}

  let () =
    (* Level 10 *)
    let bodies =
      [ { Body.surface= Body.Normal
        ; body= {Body.pos= vc (-15.0) 0.0; mass= 500.0; radius= 7.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 15.0 15.0; mass= 500.0; radius= 7.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 15.0 (-15.0); mass= 500.0; radius= 7.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 30.0 0.0; mass= 500.0; radius= 7.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc (-55.0) 0.0; mass= 300.0; radius= 2.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 50.0 0.0; mass= 200.0; radius= 4.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 70.0 0.0; mass= 200.0; radius= 4.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 85.0 7.0; mass= 200.0; radius= 4.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 100.0 14.0; mass= 200.0; radius= 4.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 110.0 25.0; mass= 200.0; radius= 4.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 120.0 38.0; mass= 200.0; radius= 4.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 125.0 55.0; mass= 200.0; radius= 4.0} } ]
    in
    let enemies =
      [ make_slime (-58.0) 0.0
      ; make_slime 0.4 0.0
      ; make_slime 25.0 6.5
      ; make_slime 125.0 60.0
      ; make_slime 100.0 20.0 ]
    in
    make_level 10 "Ten" "Another level to test your skills" (-30.0) 0.0
      bodies enemies
      {health= 6; time= 25.0; shots= 4}

  let () =
    (* Level 11 *)
    let bodies =
      [ { Body.surface= Body.Painful
        ; body= {Body.pos= vc 0.0 0.0; mass= 900.0; radius= 10.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 30.0 0.0; mass= 300.0; radius= 5.0} } ]
    in
    let enemies = [make_slime 30.0 (-6.0)] in
    make_level 11 "Eleven" "Think fast!" (-50.0) 0.0 bodies enemies
      {health= 4; time= 5.0; shots= 1}

  let () =
    (* Level 12 *)
    let x r theta = r *. Float.cos theta in
    let y r theta = r *. Float.sin theta in
    let circle_angles count =
      List.init count (fun i ->
          float_of_int i *. Float.pi *. 2.0 /. float_of_int count)
    in
    let angle_to_enemy r theta = make_slime (x r theta) (y r theta) in
    let r1 = List.map (angle_to_enemy 5.0) (circle_angles 3) in
    let bodies = [] in
    let enemies = List.concat [r1] in
    make_level 12 "Twelve" "Careful not to drift off too far" 0.0 0.0 bodies
      enemies
      {health= 5; time= 10.0; shots= 3}
end

module World2 = struct
  let () =
    (* Level 1 *)
    let bodies =
      [ { Body.surface= Body.Sticky
        ; body= {Body.pos= vc 0.0 0.0; mass= 300.0; radius= 5.0} } ]
    in
    let enemies = [make_slime 5.0 0.0] in
    make_level 101 "One"
      "Ahh, this planet is covered in pink goo, we can't move!!" (-5.0) 0.0
      bodies enemies
      {health= 6; time= 5.0; shots= 1}

  let () =
    (* Level 2 *)
    let bodies =
      [ { Body.surface= Body.Sticky
        ; body= {Body.pos= vc 0.0 0.0; mass= 300.0; radius= 5.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 20.0 0.0; mass= 300.0; radius= 5.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc (-20.0) 0.0; mass= 300.0; radius= 5.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 0.0 20.0; mass= 1000.0; radius= 5.0} } ]
    in
    let enemies =
      [make_slime 5.0 0.0; make_slime 25.0 0.0; make_slime (-25.0) 0.0]
    in
    make_level 102 "Two"
      "Use the gravity of planets to shoot places you couldn't otherwise \
       reach"
      (-5.0) 0.0 bodies enemies
      {health= 6; time= 20.0; shots= 3}

  let () =
    (* Level 3 *)
    let bodies =
      [ { Body.surface= Body.Sticky
        ; body= {Body.pos= vc 0.0 0.0; mass= 1000.0; radius= 10.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 18.0 2.0; mass= 200.0; radius= 2.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 26.0 (-2.0); mass= 200.0; radius= 2.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 34.0 2.0; mass= 200.0; radius= 2.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 40.0 0.0; mass= 200.0; radius= 2.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 30.0 20.0; mass= 100.0; radius= 15.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 30.0 (-20.0); mass= 100.0; radius= 15.0} } ]
    in
    let enemies = [make_slime 42.0 0.0] in
    make_level 103 "Three"
      "Black holes ahead, good thing you're too stuck to get sucked in" 10.0
      0.0 bodies enemies
      {health= 6; time= 5.0; shots= 1}

  let () =
    (* Level 4 *)
    let bodies =
      [ { Body.surface= Body.Normal
        ; body= {Body.pos= vc 0.0 0.0; mass= 300.0; radius= 5.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 30.0 0.0; mass= 500.0; radius= 5.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 15.0 5.0; mass= -500.0; radius= 5.0} } ]
    in
    let enemies = [make_slime 25.0 0.0] in
    make_level 104 "Four"
      "That's a weird looking planet. I wonder whats up with that?" 5.0 0.0
      bodies enemies
      {health= 6; time= 3.0; shots= 1}

  let () =
    (* Level 5 *)
    let bodies =
      [ { Body.surface= Body.Sticky
        ; body= {Body.pos= vc 0.0 0.0; mass= 300.0; radius= 5.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 20.0 18.0; mass= -150.0; radius= 10.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 35.0 20.0; mass= 100.0; radius= 3.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 15.0 (-10.0); mass= -500.0; radius= 4.0} } ]
    in
    let enemies = [make_slime 38.0 20.0] in
    make_level 105 "Five"
      "These antigravity planets seem to have different masses too" 5.0 0.0
      bodies enemies
      {health= 6; time= 5.0; shots= 1}

  let () =
    (* Level 6 *)
    let bodies =
      [ { Body.surface= Body.Normal
        ; body= {Body.pos= vc 0.0 0.0; mass= 300.0; radius= 5.0} }
      ; { Body.surface= Body.Sticky
        ; body= {Body.pos= vc 0.0 30.0; mass= 600.0; radius= 7.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc (-25.0) 20.0; mass= -400.0; radius= 5.0} }
      ; { Body.surface= Body.Painful
        ; body= {Body.pos= vc 25.0 20.0; mass= 500.0; radius= 5.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 20.0 60.0; mass= 100.0; radius= 5.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 0.0 65.0; mass= 300.0; radius= 5.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc (-20.0) 60.0; mass= 100.0; radius= 5.0} } ]
    in
    let enemies =
      [ make_slime (-20.0) 55.0
      ; make_slime 0.0 60.0
      ; make_slime 20.0 55.0
      ; make_slime 0.0 (-5.0) ]
    in
    make_level 106 "Six"
      "Remember, if you find yourself in a sticky situation, you can hit \
       [p]ause and restart the level."
      0.0 5.0 bodies enemies
      {health= 6; time= 12.0; shots= 4}

  let () =
    (* Level 7 *)
    let bodies =
      [ { Body.surface= Body.Sticky
        ; body= {Body.pos= vc 0.0 0.0; mass= 300.0; radius= 5.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc (-10.0) 0.0; mass= 10.0; radius= 5.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 7.0 0.0; mass= 10.0; radius= 2.0} }
      ; { Body.surface= Body.Bouncy
        ; body= {Body.pos= vc 20.0 0.0; mass= 300.0; radius= 5.0} } ]
    in
    let enemies = [make_slime 0.0 (-5.0)] in
    make_level 107 "Seven"
      "The surface of that planet looks strangely... bouncy?" 0.0 5.0 bodies
      enemies
      {health= 6; time= 5.0; shots= 1}

  let () =
    (* Level 8 *)
    let tl = 5.0 in
    let toff = 8.0 in
    let tlen = 6 in
    let tunnel =
      List.init tlen (fun i ->
          let i = float_of_int i in
          [ { Body.surface= Body.Bouncy
            ; body=
                { Body.pos= vc ((i *. 6.0) +. toff) tl
                ; mass= 200.0
                ; radius= 3.0 } }
          ; { Body.surface= Body.Bouncy
            ; body=
                { Body.pos= vc ((i *. 6.0) +. toff) (-.tl)
                ; mass= 200.0
                ; radius= 3.0 } } ])
    in
    let bodies =
      List.concat
        [ List.flatten tunnel
        ; [ { Body.surface= Body.Sticky
            ; body= {Body.pos= vc 0.0 0.0; mass= 200.0; radius= 2.0} }
          ; { Body.surface= Body.Normal
            ; body= {Body.pos= vc 45.0 0.0; mass= 500.0; radius= 2.0} } ] ]
    in
    let enemies =
      [make_slime 45.0 (-2.0); make_slime 45.0 2.0; make_slime (-5.0) 0.0]
    in
    make_level 108 "Eight" "Boooiiing! Are you having fun yet?" 4.0 0.0
      bodies enemies
      {health= 6; time= 7.0; shots= 2}

  let () =
    (* Level 9 *)
    let bodies =
      [ { Body.surface= Body.Sticky
        ; body= {Body.pos= vc 0.0 0.0; mass= 100.0; radius= 10.0} }
      ; { Body.surface= Body.Bouncy
        ; body= {Body.pos= vc (-20.0) 10.0; mass= 50.0; radius= 6.0} }
      ; { Body.surface= Body.Bouncy
        ; body= {Body.pos= vc (-20.0) (-10.0); mass= 50.0; radius= 6.0} } ]
    in
    let enemies = [make_slime (-4.0) (-8.0)] in
    make_level 109 "Nine"
      "Caution low gravity zone. It'll be hard to curve shots here" 0.0 10.0
      bodies enemies
      {health= 6; time= 7.0; shots= 1}

  let () =
    (* Level 10 *)
    let bodies =
      [ { Body.surface= Body.Normal
        ; body= {Body.pos= vc 100.0 0.0; mass= 350.0; radius= 7.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 85.0 (-6.0); mass= -300.0; radius= 3.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 70.0 0.0; mass= 500.0; radius= 7.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 55.0 (-6.0); mass= -300.0; radius= 3.0} }
      ; { Body.surface= Body.Sticky
        ; body= {Body.pos= vc 40.0 0.0; mass= 800.0; radius= 8.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 45.0 (-9.5); mass= -100.0; radius= 2.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 40.0 (-10.0); mass= -100.0; radius= 2.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 35.0 (-9.5); mass= -100.0; radius= 2.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 0.0 20.0; mass= 10.0; radius= 7.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 0.0 (-20.0); mass= 100.0; radius= 7.0} }
      ; { Body.surface= Body.Bouncy
        ; body= {Body.pos= vc (-13.0) 8.0; mass= 10.0; radius= 5.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc (-10.0) (-10.0); mass= 200.0; radius= 1.0} } ]
    in
    let enemies =
      [ make_slime 100.0 (-8.0)
      ; make_slime (-7.0) 20.0
      ; make_slime (-7.0) (-20.0) ]
    in
    make_level 110 "Ten" "Why are we killing slimes again?" 100.0 10.0 bodies
      enemies
      {health= 6; time= 22.0; shots= 3}

  let () =
    (* Level 11 *)
    let curve theta_from theta_to radius planets size surface mass
        ?(mass_to = mass) cx cy =
      let fplanets = float_of_int planets in
      let dtheta = (theta_to -. theta_from) /. fplanets in
      let dmass = (mass_to -. mass) /. fplanets in
      List.init planets (fun i ->
          let fi = float_of_int i in
          let theta = theta_from +. (fi *. dtheta) in
          let x = cx +. (radius *. Float.cos theta) in
          let y = cy +. (radius *. Float.sin theta) in
          { Body.surface
          ; body=
              {Body.pos= vc x y; mass= mass +. (dmass *. fi); radius= size}
          })
    in
    let bodies = [] in
    let tc = 7 in
    let span = 1.0 in
    let delta = span /. float_of_int tc in
    let curve_bodies =
      curve 0.0
        ((span *. 2.0) -. delta)
        100.0 2 4.0 Body.Normal 200.0 ~mass_to:600.0 0.0 0.0
    in
    let curve_bodies_bounce =
      curve delta (span -. delta) 100.0 (tc - 2) 5.1 Body.Bouncy 200.0
        ~mass_to:450.0 0.0 0.0
    in
    let bodies = List.concat [bodies; curve_bodies; curve_bodies_bounce] in
    let enemies =
      [ make_slime 50.0 86.0
      ; make_slime 70.0 70.0
      ; make_slime 86.0 50.0
      ; make_slime 94.0 34.0 ]
    in
    make_level 111 "Eleven" "Do or do not, there is no try. --Yoda" 105.0 0.0
      bodies enemies
      {health= 6; time= 20.0; shots= 4}

  let () =
    (* Level 12 *)
    let bodies =
      [ { Body.surface= Body.Sticky
        ; body= {Body.pos= vc 0.0 0.0; mass= 150.0; radius= 1.0} } ]
    in
    let eloc r theta =
      let r = float_of_int r in
      let theta = float_of_int theta *. Float.pi /. 180.0 in
      let x = r *. Float.cos theta in
      let y = r *. Float.sin theta in
      make_slime x y
    in
    let enemies =
      [ (* radius angle[degrees] *)
        eloc 20 30
      ; eloc 30 150
      ; eloc 40 90
      ; eloc 50 45
      ; eloc 60 135
      ; eloc 65 180
      ; eloc 65 0 ]
    in
    make_level 112 "Twelve"
      "Ahh, they're attacking back. Careful not to blow yourself up" 0.0 1.0
      bodies enemies
      {health= 6; time= 15.0; shots= 7}
end

module World3 = struct
  let () =
    (* Level 1 *)
    let bodies =
      [ { Body.surface= Body.Slippery
        ; body= {Body.pos= vc 0.0 0.0; mass= 7000.0; radius= 25.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 60.0 0.0; mass= 300.0; radius= 5.0} } ]
    in
    let enemies = [make_slime 55.0 0.0] in
    make_level 201 "One" "Brrrr, its cold and slippery here" (-25.0) 0.0
      bodies enemies
      {health= 6; time= 6.0; shots= 1}

  let () =
    (* Level 2 *)
    let bodies =
      [ { Body.surface= Body.Slippery
        ; body= {Body.pos= vc 90.0 30.0; mass= 1200.0; radius= 10.0} }
      ; { Body.surface= Body.Sticky
        ; body= {Body.pos= vc 70.0 20.0; mass= 100.0; radius= 3.0} }
      ; { Body.surface= Body.Sticky
        ; body= {Body.pos= vc 60.0 40.0; mass= 100.0; radius= 3.0} }
      ; { Body.surface= Body.Sticky
        ; body= {Body.pos= vc 50.0 20.0; mass= 100.0; radius= 3.0} }
      ; { Body.surface= Body.Slippery
        ; body= {Body.pos= vc 30.0 30.0; mass= 1800.0; radius= 14.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc (-30.0) 30.0; mass= 200.0; radius= 3.0} } ]
    in
    let enemies = [make_slime 80.0 30.0; make_slime (-27.0) 30.0] in
    make_level 202 "Two" "Careful you don't slip into a sticky situation"
      100.0 30.0 bodies enemies
      {health= 6; time= 10.0; shots= 2}

  let () =
    (* Level 3 *)
    let bodies =
      [ { Body.surface= Body.Slippery
        ; body= {Body.pos= vc 90.0 30.0; mass= 1200.0; radius= 10.0} }
      ; { Body.surface= Body.Sticky
        ; body= {Body.pos= vc 60.0 20.0; mass= 800.0; radius= 7.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 10.0 (-15.0); mass= 700.0; radius= 4.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 20.0 10.0; mass= -200.0; radius= 3.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 10.0 35.0; mass= 700.0; radius= 4.0} } ]
    in
    let enemies =
      [make_slime 80.0 30.0; make_slime 5.0 (-15.0); make_slime 5.0 35.0]
    in
    make_level 203 "Three" "Brrrr, its cold and slippery here" 100.0 30.0
      bodies enemies
      {health= 6; time= 20.0; shots= 3}

  let () =
    (* Level 4 *)
    let bodies =
      [ { Body.surface= Body.Slippery
        ; body= {Body.pos= vc 90.0 30.0; mass= 550.0; radius= 5.0} }
      ; { Body.surface= Body.Bouncy
        ; body= {Body.pos= vc 60.0 30.0; mass= 850.0; radius= 15.0} } ]
    in
    let enemies = [make_slime 45.0 30.0] in
    let powerups = [{Powerup.power= Powerup.Fireblast; pos= vc 90.0 37.0}] in
    make_level 204 "Four" ~powerups
      "Ahhh! I'm out of control! If only there was some powerup that could \
       cause the next shot to melt an icy planet"
      93.0 30.0 bodies enemies
      {health= 6; time= 10.0; shots= 2}

  let () =
    (* Level 5 *)
    let bodies =
      [ { Body.surface= Body.Slippery
        ; body= {Body.pos= vc 90.0 30.0; mass= 550.0; radius= 5.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 75.0 30.0; mass= 150.0; radius= 3.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 90.0 10.0; mass= 150.0; radius= 3.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 105.0 30.0; mass= 150.0; radius= 3.0} }
      ; { Body.surface= Body.Slippery
        ; body= {Body.pos= vc 90.0 60.0; mass= 450.0; radius= 5.0} }
      ; { Body.surface= Body.Sticky
        ; body= {Body.pos= vc 75.0 60.0; mass= 250.0; radius= 3.0} }
      ; { Body.surface= Body.Sticky
        ; body= {Body.pos= vc 105.0 60.0; mass= 250.0; radius= 3.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 90.0 80.0; mass= 550.0; radius= 3.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 90.0 105.0; mass= 250.0; radius= 1.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 98.0 100.0; mass= -10.0; radius= 3.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 82.0 100.0; mass= -10.0; radius= 3.0} } ]
    in
    let enemies = [make_slime 90.0 108.0] in
    let powerups = [{Powerup.power= Powerup.Fireblast; pos= vc 90.0 55.0}] in
    make_level 205 "Five" ~powerups
      "Ice is slippery, if you go to fast, you may lose your footing" 93.0
      30.0 bodies enemies
      {health= 6; time= 10.0; shots= 2}

  let () =
    (* Level 6 *)
    let bodies =
      [ { Body.surface= Body.Normal
        ; body= {Body.pos= vc 300.0 300.0; mass= 550.0; radius= 8.0} }
      ; { Body.surface= Body.Slippery
        ; body= {Body.pos= vc 330.0 300.0; mass= 550.0; radius= 8.0} }
      ; { Body.surface= Body.Sticky
        ; body= {Body.pos= vc 360.0 300.0; mass= 550.0; radius= 6.5} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 430.0 305.0; mass= -750.0; radius= 6.5} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 400.0 310.0; mass= 550.0; radius= 2.5} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 415.0 300.0; mass= 550.0; radius= 4.5} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 400.0 295.0; mass= 50.0; radius= 10.0} } ]
    in
    let enemies = [make_slime 419.5 300.0] in
    let powerups = [{Powerup.power= Powerup.Fireblast; pos= vc 90.0 55.0}] in
    make_level 206 "Six" ~powerups "TODO: Level text" 300.0 308.0 bodies
      enemies
      {health= 6; time= 7.0; shots= 1}

  let () =
    (* Level 7 *)
    let bodies =
      [ { Body.surface= Body.Normal
        ; body= {Body.pos= vc 300.0 300.0; mass= 550.0; radius= 8.0} } ]
    in
    let enemies = [make_ice_slime 308.0 300.0; make_slime 307.0 298.0] in
    let powerups =
      [{Powerup.power= Powerup.Fireblast; pos= vc 300.0 291.0}]
    in
    make_level 207 "Seven" ~powerups
      "Icy enemies have hard armor that needs to be melted. Fire shots will \
       not be enough to kill a slime though"
      300.0 308.0 bodies enemies
      {health= 6; time= 5.0; shots= 2}

  let () =
    (* Level 8 *)
    let bodies =
      [ { Body.surface= Body.Slippery
        ; body= {Body.pos= vc 300.0 300.0; mass= 550.0; radius= 8.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 340.0 300.0; mass= 550.0; radius= 8.0} } ]
    in
    let enemies = [make_ice_slime 250.0 300.0; make_slime 348.0 300.0] in
    let powerups =
      [{Powerup.power= Powerup.Fireblast; pos= vc 300.0 291.0}]
    in
    make_level 208 "Eight" ~powerups "" 300.0 308.0 bodies enemies
      {health= 6; time= 12.0; shots= 3}

  let () =
    (* Level 9 *)
    let px = 300.0 in
    let py = 300.0 in
    let pr = 4.0 in
    let ploc theta =
      let theta = float_of_int theta *. Float.pi /. 180.0 in
      let r = pr +. 1.5 in
      let x = px +. (r *. Float.cos theta) in
      let y = py +. (r *. Float.sin theta) in
      {Powerup.power= Powerup.Fireblast; pos= vc x y}
    in
    let eloc r theta t =
      let r = float_of_int r in
      let theta = float_of_int theta *. Float.pi /. 180.0 in
      let x = px +. (r *. Float.cos theta) in
      let y = py +. (r *. Float.sin theta) in
      (if t then make_ice_slime else make_slime) x y
    in
    let bodies =
      [ { Body.surface= Body.Slippery
        ; body= {Body.pos= vc px py; mass= 350.0; radius= pr} } ]
    in
    let powerups = [ploc 0; ploc 180; ploc 270] in
    let enemies =
      [ (* radius angle[degrees] *)
        eloc 40 90 true
      ; eloc 45 45 true
      ; eloc 65 135 false
      ; eloc 85 180 false
      ; eloc 85 0 false ]
    in
    make_level 209 "Nine" ~powerups
      "Make sure not to waste powerups by missing" 300.0 308.0 bodies enemies
      {health= 6; time= 15.0; shots= 8}

  let () =
    (* Level 10 *)
    let orbital_velocity planet_mass at_distance =
      Float.sqrt
        (Moonshot.gravitational_constant *. planet_mass /. at_distance)
    in
    let heavy = 100.0 in
    let ob = 3.0 in
    let bodies =
      [ { Body.surface= Body.Sticky
        ; body= {Body.pos= vc 300.0 300.0; mass= 10.0; radius= 1.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 340.0 300.0; mass= heavy; radius= 1.0} } ]
    in
    let enemies =
      [ make_moving_slime 340.0 (300.0 +. ob) (orbital_velocity heavy ob) 0.0
      ; make_moving_slime 340.0 (300.0 -. ob)
          (-.orbital_velocity heavy ob)
          0.0
      ; make_moving_slime (340.0 +. ob) 300.0 0.0 (orbital_velocity heavy ob)
      ; make_moving_slime (340.0 -. ob) 300.0 0.0
          (-.orbital_velocity heavy ob) ]
    in
    let powerups = [] in
    make_level 210 "Ten" ~powerups "TODO: Level text" 300.0 303.0 bodies
      enemies
      {health= 6; time= 3.0; shots= 1}

  let () =
    (* Level 11 *)
    let bodies =
      [ { Body.surface= Body.Slippery
        ; body= {Body.pos= vc 300.0 300.0; mass= 300.0; radius= 5.0} }
      ; { Body.surface= Body.Slippery
        ; body= {Body.pos= vc 300.0 355.0; mass= 300.0; radius= 5.0} }
      ; { Body.surface= Body.Slippery
        ; body= {Body.pos= vc 250.0 355.0; mass= 300.0; radius= 5.0} }
      ; { Body.surface= Body.Slippery
        ; body= {Body.pos= vc 200.0 355.0; mass= 300.0; radius= 5.0} }
      ; { Body.surface= Body.Slippery
        ; body= {Body.pos= vc 200.0 300.0; mass= 300.0; radius= 5.0} } ]
    in
    let enemies = [make_slime 200.0 300.0] in
    let powerups = [] in
    make_level 211 "Eleven" ~powerups
      "Is that some planets way out in the distance, hidden out of sight?"
      300.0 303.0 bodies enemies
      {health= 6; time= 5.0; shots= 1}

  let () =
    (* Level 12 *)
    let bodies =
      [ { Body.surface= Body.Normal
        ; body= {Body.pos= vc (-15.0) 0.0; mass= 500.0; radius= 7.0} }
      ; { Body.surface= Body.Sticky
        ; body= {Body.pos= vc 15.0 15.0; mass= 500.0; radius= 7.0} }
      ; { Body.surface= Body.Sticky
        ; body= {Body.pos= vc 15.0 (-15.0); mass= 500.0; radius= 7.0} }
      ; { Body.surface= Body.Slippery
        ; body= {Body.pos= vc 30.0 0.0; mass= 500.0; radius= 7.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc (-55.0) 0.0; mass= 300.0; radius= 2.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 50.0 0.0; mass= 200.0; radius= 4.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 70.0 0.0; mass= 200.0; radius= 4.0} }
      ; { Body.surface= Body.Bouncy
        ; body= {Body.pos= vc 85.0 7.0; mass= 200.0; radius= 4.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 100.0 14.0; mass= 200.0; radius= 4.0} }
      ; { Body.surface= Body.Bouncy
        ; body= {Body.pos= vc 110.0 25.0; mass= 200.0; radius= 4.0} }
      ; { Body.surface= Body.Normal
        ; body= {Body.pos= vc 120.0 38.0; mass= 200.0; radius= 4.0} }
      ; { Body.surface= Body.Bouncy
        ; body= {Body.pos= vc 125.0 55.0; mass= 200.0; radius= 4.0} } ]
    in
    let enemies =
      [ make_ice_slime (-58.0) 0.0
      ; make_slime 0.4 0.0
      ; make_slime 25.0 6.5
      ; make_slime 125.0 60.0
      ; make_slime 100.0 20.0 ]
    in
    let powerups =
      [{Powerup.power= Powerup.Fireblast; pos= vc 120.0 42.0}]
    in
    make_level 212 "Twelve" ~powerups
      "The final level! Did you get 3 stars on everything yet?" (-30.0) 0.0
      bodies enemies
      {health= 6; time= 75.0; shots= 5}
end

let load i =
  match Hashtbl.find_opt level_map i with Some l -> l | None -> blank_level
