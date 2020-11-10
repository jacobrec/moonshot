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
 * World 1: The enemy strikes back
 * Introduce sticky planets
 * 2-1) Stuck
 * 2-2) Moving
 * 2-3) Jumping
 * 2-4) Planet gravities
 * 2-5) Double Jumping
 * 2-6) Painful Planets
 * 2-7) Friendly Fire
 * 2-8) Stuck on black holes
 * 2-9) Challenge 1
 * 2-10) Challenge 2
 * 2-11) Hurty planet special
 * 2-12) Zero G shooting Challenge
*)

let vc = Raylib.Vector2.create

let level_map = Hashtbl.create 24
let avaliable_worlds = 2
let world_names = [
    "Getting Started";
    "Strange new worlds"
  ]

let make_level id name start_text px py bodies enemies star_reqs =
  let movables = [] in
  let cam = Raylib.Camera2D.create (vc (float_of_int (screen_width / 2)) (float_of_int (screen_height / 2)))
              (vc 0.0 0.0) 0.0 1.0 in (* offset target rotation zoom *)
  let make_player x y =
    {
      Player.feet={
        Body.body={pos=vc x y; mass=  10.0; radius=0.5;}; vel=vc 0.0 0.0};
      Player.head={
          Body.body={pos=vc x (y +. 1.0); mass= -3.5; radius=0.5;}; vel=vc 0.0 0.0};
      input=Player.None;
      Player.health=6;
      last_damaged_at=0.0;
    } in
  let stars = Starfield.create 8 400 in
  let level = { Model.static=bodies;
                stars;
                name;
                id;
                start_text;
                fading=[];
                enemies;
                cam;
                player=make_player px py;
                runtime=0.0;
                shots_taken=0;
                longest_bullet=0.0;
                star_reqs;
                bullets=movables } in
  if Hashtbl.mem level_map id then begin
      print_endline "ERROR: Level id is already taken. Please choose a unique ID";
      invalid_arg "make_level"
    end;
  Hashtbl.add level_map id level


let () =
  let bodies = [] in
  let enemies = [] in
  make_level 0 "404" "Level not found" 0.0 0.0 bodies enemies {health=8; time=0.0; shots=0}
let blank_level = Hashtbl.find level_map 0


module World1 = struct
  let () = (* Level 1 *)
    let bodies = [
        {Body.surface=Body.Normal;
         body={Body.pos=vc (-30.0) 0.0; mass=900.0; radius=15.0;}};
        {Body.surface=Body.Normal;
         body={Body.pos=vc 30.0 0.0; mass=1500.0; radius=15.0;}};
      ] in
    let enemies = [
        {Enemy.loc={Body.body={Body.pos=vc 15.0 0.0; mass=1.0; radius=1.0;};
                    vel=vc 0.0 0.0}; action=Standing}
      ] in
    make_level 1 "One"
      "Click and drag away from your player to aim like a slingshot. Release to fire."
      (-15.0) 0.0 bodies enemies {health=6; time=5.0; shots=1}

  let () = (* Level 2 *)
    let bodies = [
        {Body.surface=Body.Normal;
         body={Body.pos=vc 0.0 0.0; mass=1800.0; radius=15.0;}};
      ] in
    let enemies = [
        {Enemy.loc={Body.body={Body.pos=vc 15.0 0.0; mass=1.0; radius=1.0;};
                    vel=vc 0.0 0.0}; action=Standing}
      ] in
    make_level 2 "Two"
      "Use A and S to move clockwise, and counter clockwise around the planet you're on"
      (-15.0) 0.0 bodies enemies {health=6; time=5.0; shots=1}

  let () = (* Level 3 *)
    let bodies = [
        {Body.surface=Body.Normal; body={Body.pos=vc    0.0     0.0; mass=1000.0; radius=15.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc (-30.0) (-30.0); mass=1000.0; radius=15.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc (-60.0) (-60.0); mass=1000.0; radius=15.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc (-90.0) (-90.0); mass=100.0; radius=6.0;}};
      ] in
    let enemies = [
        {Enemy.loc={Body.body={Body.pos=vc (-85.0) (-85.0); mass=1.0; radius=1.0;};
                    vel=vc 0.0 0.0}; action=Standing}
      ] in
    make_level 3 "Three"
      "Press space to jump"
      (-15.0) 0.0 bodies enemies {health=6; time=10.0; shots=1}

  let () = (* Level 4 *)
    let bodies = [
        {Body.surface=Body.Normal; body={Body.pos=vc    0.0    0.0; mass=400.0;  radius=10.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc   35.0   10.0; mass=600.0;  radius=10.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc   65.0   30.0; mass=1200.0; radius=10.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc   85.0   50.0; mass=1600.0; radius=10.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc  100.0   75.0; mass=2100.0; radius=10.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc  115.0   95.0; mass=2600.0; radius=10.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc  135.0  115.0; mass=400.0;  radius=5.0;}};
      ] in
    let enemies = [
        {Enemy.loc={Body.body={Body.pos=vc (130.0) (110.0); mass=1.0; radius=1.0;};
                    vel=vc 0.0 0.0}; action=Standing}
      ] in
    make_level 4 "Four"
      "Different planets have different densities. More mass means more gravity."
      (-15.0) 0.0 bodies enemies {health=6; time=15.0; shots=1}

  let () = (* Level 5 *)
    let radii = 20.0 in
    let x theta = radii *. Float.cos theta in
    let y theta = radii *. Float.sin theta in
    let bodies = [
        {Body.surface=Body.Normal; body={Body.pos=vc  (x 0.0) (y 0.0); mass=600.0;  radius=7.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc  (x 2.1) (y 2.1); mass=600.0;  radius=7.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc  (x 4.2) (y 4.2); mass=600.0;  radius=7.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc  (-40.0) 0.0;     mass=600.0; radius=8.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc  (-60.0) 5.0;    mass=300.0; radius=4.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc  (-70.0) (-5.0); mass=300.0; radius=4.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc  (-80.0) 5.0;  mass=300.0; radius=4.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc  (-100.0) 5.0; mass=600.0; radius=3.0;}};
      ] in
    let enemies = [
        {Enemy.loc={Body.body={Body.pos=vc (-103.0) (2.0); mass=1.0; radius=1.0;};
                    vel=vc 0.0 0.0}; action=Standing}
      ] in
    make_level 5 "Five"
      "Jump just as you land to double (or triple) jump and go even higher."
      15.0 0.0 bodies enemies {health=6; time=10.0; shots=1}

  let () = (* Level 6 *)
    let radii = 20.0 in
    let x theta = radii *. Float.cos theta in
    let y theta = radii *. Float.sin theta in
    let bodies = [
        {Body.surface=Body.Normal; body={Body.pos=vc  (x 0.0) (y 0.0); mass=600.0;  radius=7.0;}};
        {Body.surface=Body.Painful; body={Body.pos=vc  (x 2.1) (y 2.1); mass=600.0;  radius=7.0;}};
        {Body.surface=Body.Painful; body={Body.pos=vc  (x 4.2) (y 4.2); mass=600.0;  radius=7.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc  (-40.0) 0.0; mass=600.0;  radius=8.0;}};

        {Body.surface=Body.Normal; body={Body.pos=vc  (-60.0) 5.0; mass=300.0;  radius=4.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc  (-70.0) (-5.0); mass=300.0;  radius=4.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc  (-80.0) 5.0; mass=300.0;  radius=4.0;}};

        {Body.surface=Body.Normal; body={Body.pos=vc  (-100.0) 5.0; mass=600.0;  radius=3.0;}};
      ] in
    let enemies = [
        {Enemy.loc={Body.body={Body.pos=vc (-103.0) (2.0); mass=1.0; radius=1.0;};
                    vel=vc 0.0 0.0}; action=Standing}
      ] in
    make_level 6 "Six"
      "Some planets are painful to touch"
      14.0 2.0 bodies enemies {health=6; time=13.0; shots=1}

  let () = (* Level 7 *)
    let bodies = [
        {Body.surface=Body.Normal; body={Body.pos=vc 0.0 0.0; mass=600.0;  radius=6.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc 18.0 0.0; mass=600.0;  radius=6.0;}};
      ] in
    let enemies = [
        {Enemy.loc={Body.body={Body.pos=vc (12.0) (0.0); mass=1.0; radius=1.0;};
                    vel=vc 0.0 0.0}; action=Standing}
      ] in
    make_level 7 "Seven"
      "Careful, if you are not, you may get caught by your own shot."
      6.0 0.0 bodies enemies {health=6; time=3.0; shots=1}

  let () = (* Level 8 *)
    let bodies = [
        {Body.surface=Body.Normal; body={Body.pos=vc 0.0 0.0; mass=600.0;  radius=8.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc 20.0 10.0; mass=1200.0;  radius=2.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc 50.0 0.0; mass=100.0;  radius=2.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc 20.0 (-10.0); mass=100.0;  radius=10.0;}};
      ] in
    let enemies = [
        {Enemy.loc={Body.body={Body.pos=vc (50.0) (-2.0); mass=1.0; radius=1.0;};
                    vel=vc 0.0 0.0}; action=Standing}
      ] in
    make_level 8 "Eight"
      "Some celetial bodies are extremely dense. You can restart the level through the pause menu (press p)"
      6.0 0.0 bodies enemies {health=6; time=8.0; shots=1}

  let () = (* Level 9 *)
    let bodies = [
        {Body.surface=Body.Normal; body={Body.pos=vc 0.0 0.0; mass=900.0; radius=10.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc 40.0 0.0; mass=1200.0; radius=10.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc 70.0 (-10.0); mass=600.0; radius=7.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc 70.0 10.0; mass=600.0; radius=7.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc 60.0 0.0; mass=100.0; radius=3.0;}};
        {Body.surface=Body.Painful;  body={Body.pos=vc 90.0 (-20.0); mass=500.0; radius=6.0;}};
        {Body.surface=Body.Painful;  body={Body.pos=vc 30.0 30.0; mass=500.0; radius=1.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc 110.0 30.0; mass=500.0; radius=1.0;}};
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
    make_level 9 "Nine"
      "You've learned all there is. Good luck. See if you can 3 star all the levels"
      (-10.0) 0.0 bodies enemies {health=6; time=12.0; shots=3}

  let () = (* Level 10 *)
    let bodies = [
        {Body.surface=Body.Normal; body={Body.pos=vc (-15.0) (  0.0); mass=500.0; radius=7.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc ( 15.0) ( 15.0); mass=500.0; radius=7.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc ( 15.0) (-15.0); mass=500.0; radius=7.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc ( 30.0) (  0.0); mass=500.0; radius=7.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc (-55.0) (  0.0); mass=300.0; radius=2.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc ( 50.0) (  0.0); mass=200.0; radius=4.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc ( 70.0) (  0.0); mass=200.0; radius=4.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc ( 85.0) (  7.0); mass=200.0; radius=4.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc (100.0) ( 14.0); mass=200.0; radius=4.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc (110.0) ( 25.0); mass=200.0; radius=4.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc (120.0) ( 38.0); mass=200.0; radius=4.0;}};
        {Body.surface=Body.Normal; body={Body.pos=vc (125.0) ( 55.0); mass=200.0; radius=4.0;}};
      ] in
    let enemies = [
        {Enemy.loc={Body.body={Body.pos=vc (-58.0) 0.0; mass=1.0; radius=1.0;};
                    vel=vc 0.0 0.0}; action=Standing};
        {Enemy.loc={Body.body={Body.pos=vc (0.4) 0.0; mass=1.0; radius=1.0;};
                    vel=vc 0.0 0.0}; action=Standing};
        {Enemy.loc={Body.body={Body.pos=vc 25.0 6.5; mass=1.0; radius=1.0;};
                    vel=vc 0.0 0.0}; action=Standing};
        {Enemy.loc={Body.body={Body.pos=vc 125.0 60.0; mass=1.0; radius=1.0;};
                    vel=vc 0.0 0.0}; action=Standing};
        {Enemy.loc={Body.body={Body.pos=vc 100.0 20.0; mass=1.0; radius=1.0;};
                    vel=vc 0.0 0.0}; action=Standing};
      ] in
    make_level 10 "Ten"
      "Another level to test your skills"
      (-30.0) 0.0 bodies enemies {health=6; time=25.0; shots=4}

  let () = (* Level 11 *)
    let bodies = [
        {Body.surface=Body.Painful;
         body={Body.pos=vc 0.0 0.0; mass=900.0; radius=10.0;}};
        {Body.surface=Body.Normal;
         body={Body.pos=vc 30.0 0.0; mass=300.0; radius=5.0;}};
      ] in
    let enemies = [
        {Enemy.loc={Body.body={Body.pos=vc 30.0 (-6.0); mass=1.0; radius=1.0;};
                    vel=vc 0.0 0.0}; action=Standing}
      ] in
    make_level 11 "Eleven"
      "Think fast!"
      (-30.0) 0.0 bodies enemies {health=5; time=3.0; shots=1}

  let () = (* Level 12 *)
    let x r theta = r *. Float.cos theta in
    let y r theta = r *. Float.sin theta in
    let circle_angles count = List.init count (fun i -> (float_of_int i) *. Float.pi *. 2.0 /. (float_of_int count)) in
    let angle_to_enemy r theta =
      {Enemy.loc={Body.body={Body.pos=vc (x r theta) (y r theta); mass=1.0; radius=1.0;};
                  vel=vc 0.0 0.0}; action=Standing} in

    let r1 = List.map (angle_to_enemy 5.0) (circle_angles 3) in
    let bodies = [] in
    let enemies = List.concat [r1] in
    make_level 12 "Twelve"
      "Careful not to drift off too far"
      (0.0) 0.0 bodies enemies {health=5; time=10.0; shots=3}

end

module World2 = struct
  let make_slime x y =
    {Enemy.loc={Body.body={Body.pos=vc x y; mass=1.0; radius=1.0;};
                vel=vc 0.0 0.0}; action=Standing}
let () = (* Level 1 *)
  let bodies = [
      {Body.surface=Body.Sticky; body={Body.pos=vc 0.0 0.0; mass=300.0; radius=5.0;}};
    ] in
  let enemies = [
      {Enemy.loc={Body.body={Body.pos=vc 5.0 0.0; mass=1.0; radius=1.0;};
                  vel=vc 0.0 0.0}; action=Standing}
    ] in
  make_level 101 "One"
    "Ahh, this planet is covered in pink goo, we can't move!!"
    (-5.0) 0.0 bodies enemies {health=6; time=5.0; shots=1}

let () = (* Level 2 *)
  let bodies = [
      {Body.surface=Body.Sticky; body={Body.pos=vc 0.0 0.0; mass=300.0; radius=5.0;}};
      {Body.surface=Body.Normal; body={Body.pos=vc 20.0 0.0; mass=300.0; radius=5.0;}};
      {Body.surface=Body.Normal; body={Body.pos=vc (-20.0) 0.0; mass=300.0; radius=5.0;}};
      {Body.surface=Body.Normal; body={Body.pos=vc 0.0 20.0; mass=1000.0; radius=5.0;}};
    ] in
  let enemies = [
      make_slime 5.0 0.0;
      make_slime 25.0 0.0;
      make_slime (-25.0) 0.0;
    ] in
  make_level 102 "Two"
    "Use the gravity of planets to shoot places you couldn't otherwise reach"
    (-5.0) 0.0 bodies enemies {health=6; time=20.0; shots=3}

let () = (* Level 3 *)
  let bodies = [
      {Body.surface=Body.Sticky; body={Body.pos=vc 0.0 0.0; mass=1000.0; radius=10.0;}};
      {Body.surface=Body.Normal; body={Body.pos=vc 18.0 2.0; mass=200.0; radius=2.0;}};
      {Body.surface=Body.Normal; body={Body.pos=vc 26.0 (-2.0); mass=200.0; radius=2.0;}};
      {Body.surface=Body.Normal; body={Body.pos=vc 34.0 (2.0); mass=200.0; radius=2.0;}};
      {Body.surface=Body.Normal; body={Body.pos=vc 40.0 0.0; mass=200.0; radius=2.0;}};
      {Body.surface=Body.Normal; body={Body.pos=vc 30.0 20.0; mass=100.0; radius=15.0;}};
      {Body.surface=Body.Normal; body={Body.pos=vc 30.0 (-20.0); mass=100.0; radius=15.0;}};
    ] in
  let enemies = [
      make_slime 42.0 0.0;
    ] in
  make_level 103 "Three"
    "Black holes ahead, good thing you're too stuck to get sucked in"
    (10.0) 0.0 bodies enemies {health=6; time=5.0; shots=1}


let () = (* Level rest *)
  let bodies = [
      {Body.surface=Body.Normal; body={Body.pos=vc 0.0 0.0; mass=300.0; radius=5.0;}};
    ] in
  let enemies = [
      {Enemy.loc={Body.body={Body.pos=vc 5.0 0.0; mass=1.0; radius=1.0;};
                  vel=vc 0.0 0.0}; action=Standing}
    ] in
  make_level 104 "Four"
    "TODO: make a level"
    (-5.0) 0.0 bodies enemies {health=6; time=10.0; shots=1};
  make_level 105 "Five"
    "TODO: make a level"
    (-5.0) 0.0 bodies enemies {health=6; time=10.0; shots=1};
  make_level 106 "Six"
    "TODO: make a level"
    (-5.0) 0.0 bodies enemies {health=6; time=10.0; shots=1};
  make_level 107 "Seven"
    "TODO: make a level"
    (-5.0) 0.0 bodies enemies {health=6; time=10.0; shots=1};
  make_level 108 "Eight"
    "TODO: make a level"
    (-5.0) 0.0 bodies enemies {health=6; time=10.0; shots=1};
  make_level 109 "Nine"
    "TODO: make a level"
    (-5.0) 0.0 bodies enemies {health=6; time=10.0; shots=1};
  make_level 110 "Ten"
    "TODO: make a level"
    (-5.0) 0.0 bodies enemies {health=6; time=10.0; shots=1};
  make_level 111 "Eleven"
    "TODO: make a level"
    (-5.0) 0.0 bodies enemies {health=6; time=10.0; shots=1};
  make_level 112 "Twelve"
    "TODO: make a level"
    (-5.0) 0.0 bodies enemies {health=6; time=10.0; shots=1}

end


let load i =
  match Hashtbl.find_opt level_map i with
  | Some l -> l
  | None -> blank_level

