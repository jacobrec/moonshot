open Raylib
open Moonshot

let gravity' m1 m2 r =
  let _g = 6.6743e-11 in
  let g = 10.0 in
  g *. (m1 *. m2) /. (r *. r)


(* calculates the force on b1 from b2 *)
let gravity b1 b2 =
  let open Moonshot.Body in
  let m1 = b1.mass in
  let m2 = b2.mass in
  let x1 = Vector2.x b1.pos in
  let y1 = Vector2.y b1.pos in
  let x2 = Vector2.x b2.pos in
  let y2 = Vector2.y b2.pos in
  let dx = (x2 -. x1) in
  let dy = (y2 -. y1) in
  let f = gravity' m1 m2 @@ Float.sqrt (dx *. dx +. dy *. dy) in
  let theta = Float.atan2 dy dx in
  let fx = f *. Float.cos theta in
  let fy = f *. Float.sin theta in
  (fx, fy)


let gravity_from_many body many =
  let (fx, fy) = List.fold_left (fun (x, y) b2 ->
                     let (fx, fy) = gravity body b2 in
                     (x +. fx, y +. fy)
                   ) (0.0, 0.0) many in
  (fx, fy)

let accelerate_moving_body delta b (acc_x, acc_y) =
  let open Moonshot.Body in
  let (vel_x, vel_y) = (Vector2.x b.vel +. acc_x *. delta, Vector2.y b.vel +. acc_y *. delta) in
  let (pos_x, pos_y) = (Vector2.x b.body.pos +. vel_x *. delta,
                        Vector2.y b.body.pos +. vel_y *. delta) in
  let pos = Vector2.create pos_x pos_y in
  let vel = Vector2.create vel_x vel_y in
  {Moonshot.Body.vel=vel; body={b.body with pos=pos}}

let update_body delta bodies b1 =
  let open Moonshot.Body in
  let (fx, fy) = gravity_from_many b1.body bodies in
  let (acc_x, acc_y) = (fx /. b1.body.mass, fy /. b1.body.mass) in
  accelerate_moving_body delta b1 (acc_x, acc_y)

let vector_distance' v1 v2 =
  let vx = Vector2.x in
  let vy = Vector2.y in
  let dx = vx v1 -. vx v2 in
  let dy = vy v1 -. vy v2 in
  (dx *. dx +. dy *. dy)

let vector_distance v1 v2 =
  Float.sqrt @@ vector_distance' v1 v2

let angle_from_vectors' (x1, y1) (x2, y2) =
  Float.atan2 (y2 -. y1) (x2 -. x1)

let angle_from_vectors v1 v2 =
  let v1 = vector v1 in
  let v2 = vector v2 in
  angle_from_vectors' v1 v2

let bodies_touch b1 b2 =
  let open Moonshot.Body in
  check_collision_circles b1.pos b1.radius b2.pos b2.radius

let rotate theta (x, y) =
  let x' =   (x *. Float.cos theta) +. (y *. Float.sin theta) in
  let y' = -.(x *. Float.sin theta) +. (y *. Float.cos theta) in
  (x', y')

let update_planet_collidable ?(inp=None) delta bodies base =
  let open Moonshot.Body in

  let b = base in
  let b' = (match List.find_opt (fun b2 -> bodies_touch b.body b2) bodies with
            | None -> b
            | Some planet ->
               let e = 0.5 in
               let (vx, vy) = vector b.vel in

               let normal_dir = angle_from_vectors b.body.pos planet.pos in
               let rotate_forward = rotate normal_dir in
               let rotate_back = rotate @@ -. normal_dir in

               let (vpar, vper) = rotate_forward (vx, vy) in
               let vpar = -.e *. vpar in

               (* TODO: player controls *)
               let input_force = 2.0 in
               let jump_force = 18.0 in
               let (vpar, vper) = match inp with
                 | None -> (vpar, vper)
                 | Some inp ->
                    match inp with
                    | Moonshot.Player.CW -> (vpar, vper +. input_force)
                    | Moonshot.Player.CCW -> (vpar, vper -. input_force)
                    | Moonshot.Player.Jump -> (vpar -. jump_force, vper)
                    | _ -> (vpar, vper)
               in

               (* Friction *)
               let friction = 0.90 in
               let vper = friction *. vper in
               let vper = if (Float.abs vper) < 1.5 then 0.0 else vper in

               let (new_vx, new_vy) = rotate_back (vpar, vper) in


               let (px, py) = vector planet.pos in
               let (x, y) = vector b.body.pos in
               let theta = Float.atan2 (y -. py) (x -. px) in
               let radius = b.body.radius +. planet.radius in
               let new_x = px +. radius *. Float.cos theta in
               let new_y = py +. radius *. Float.sin theta in

               let pos = Vector2.create new_x new_y in
               let vel = Vector2.create new_vx new_vy in
               {vel; body={b.body with pos}}) in

  (* Calculate forces *)
  let (b_fx, b_fy) = gravity_from_many b'.body bodies in
  let (b_accx, b_accy) = (b_fx /. b'.body.mass, b_fy /. b'.body.mass) in
  let b'' = accelerate_moving_body delta b' (b_accx, b_accy) in


  (b'', (b_fx, b_fy))

let update_player delta bodies fading player =
  let {Moonshot.Player.head=float; feet=base; input; health} = player in
  (* player is like a balloon attached to a rock at a fixed distance *)
  let open Moonshot.Body in

  let in_any_explosion = List.exists (fun b2 -> ignore(b2.remaining);
                                                   bodies_touch base.body b2.body) fading in
  let health = if in_any_explosion then health - 1 else health in

  (* Calculate new base *)
  let (new_b, (b_fx, b_fy)) = update_planet_collidable ~inp:(Some input) delta bodies base in
  let new_f = float in

  (* Calculate new float *)
  let gravity_dir = -.Float.pi /.2.0  -. Float.atan2 b_fx b_fy in
  let seperation = 1.0 in
  Vector2.set_x new_f.body.pos ((Vector2.x new_b.body.pos) +. seperation *. Float.cos gravity_dir);
  Vector2.set_y new_f.body.pos ((Vector2.y new_b.body.pos) +. seperation *. Float.sin gravity_dir);

  {player with Moonshot.Player.head=new_f; feet=new_b; health}

let truncate_aim ax ay px py =
  let aim_length = 10.0 in
  let dx = ax -. px in
  let dy = ay -. py in
  let theta = Float.atan2 dy dx in
  let len = Float.sqrt (dx *. dx +. dy *. dy) in
  let truncate = Float.min len aim_length in
  let ax = px +. truncate *. Float.cos theta in
  let ay = py +. truncate *. Float.sin theta in
  (ax, ay)

let create_bullet_from_aim ax ay head =
  let open Body in
  let (px, py) = vector head.body.pos in
  let (ax, ay) = truncate_aim ax ay px py in
  let dx = px -. ax in
  let dy = py -. ay in

  let scale = 5.0 in
  let vel = Vector2.create (scale *. dx) (scale *. dy) in
  {Body.body={Body.pos=Vector2.create px py; mass=1.0; radius=0.5;};
   vel=vel}


let create_player_bullets player =
  let open Moonshot.Player in
  match player.input with
  | Moonshot.Player.Fire (ax, ay) ->
     [create_bullet_from_aim ax ay player.head]
  | _ -> []

let update_enemies delta bodies fading enemies =
  let open Moonshot.Enemy in
  let open Moonshot.Body in
  let in_any_explosion b1 = List.exists (fun b2 -> ignore(b2.remaining);
                                                   bodies_touch b1.loc.body b2.body) fading in
  let enemies = List.map (fun x -> if in_any_explosion x then (* check if dead *)
                                     {x with action=Dead 2.0} else x) enemies in
  let enemies = List.map (fun x -> {x with action=(match x.action with (* update death time *)
                                                   | Dead y -> Dead (y -. delta)
                                                   | _ -> x.action)}) enemies in
  let enemies = List.filter (fun x -> match x.action with (* remove long dead bodies *)
                                      | Dead y -> y > 0.0
                                      | _ -> true) enemies in
  let update_enemy e = let (loc, _) = update_planet_collidable delta bodies e.loc in {e with loc} in
  List.map update_enemy enemies

let update_camera cam player =
  let open Moonshot.Player in
  let zoom_speed_per_frame = 0.02 in
  let (x, y) = sofwv player.head.body.pos in
  let (vx, vy) = vector player.feet.vel in
  let speed = Float.sqrt (vx *. vx +. vy *. vy) in
  let clamped = Float.max 0.0 (Float.min 30.0 speed) in
  let desired_zoom = 1.0 +. (30.0 -. clamped) /. 30.0 in
  let diff_zoom = desired_zoom -. (Camera2D.zoom cam) in
  let abs_diff_zoom = Float.abs diff_zoom in
  let clamped_abs_diff_zoom = Float.min zoom_speed_per_frame abs_diff_zoom in
  let new_zoom = (Camera2D.zoom cam) +. Float.copy_sign clamped_abs_diff_zoom diff_zoom  in
  Camera2D.set_target cam (Vector2.create (float_of_int x) (float_of_int y));
  Camera2D.set_zoom cam new_zoom;
  cam

let update_playing_check_for_level_end model =
  let open Moonshot.Model in
  if model.player.health <= 0 then Model.LevelEnd {Model.reason=Model.Died;
                                                   runtime=model.runtime;
                                                   level=0;
                                                   shots_taken=model.shots_taken;
                                                   health=model.player.health}
  else if 0 = List.length model.enemies then Model.LevelEnd {Model.reason=Model.Victory;
                                                   runtime=model.runtime;
                                                   level=0;
                                                   shots_taken=model.shots_taken;
                                                   health=model.player.health}
  else Model.Playing model

let update_playing delta model =
  let open Moonshot.Body in
  let { Moonshot.Model.bullets=movables; static=bodies; fading=fading; player=player;
        enemies; cam; runtime; shots_taken } = model in
  let bodies = List.concat [List.map (fun x -> ignore (x.remaining); x.body) fading; bodies] in
  let fading = List.map (fun x -> { x with remaining =  x.remaining -. delta}) fading in
  let fading = List.filter (fun x -> x.remaining > 0.0) fading in
  let movables = List.map (update_body delta bodies) movables in
  let in_any_static b1 = List.exists (fun b2 -> bodies_touch b1.body b2) bodies in
  let (dead, alive) = List.partition in_any_static movables in
  let create_explosions = List.map explosion_from_body dead in
  let fading = List.concat [fading; create_explosions] in
  let player = update_player delta bodies fading player in
  let player_bullets = create_player_bullets player in
  let new_bullets = List.concat [alive; player_bullets] in
  let shots_taken = shots_taken + List.length player_bullets in
  let enemies = update_enemies delta bodies fading enemies in
  let living_enemies = List.filter (fun x -> Moonshot.Enemy.is_alive x) enemies in
  let cam = update_camera cam player in
  let runtime = if 0 = List.length living_enemies then runtime else runtime +. delta in
  update_playing_check_for_level_end
    { model with bullets=new_bullets; fading; player; enemies; cam; runtime; shots_taken}

let update delta model =
  match model with
  | Model.Paused p -> Model.Paused p
  | Model.Playing p -> update_playing delta p
  | Model.MenuScreen -> Model.MenuScreen
  | Model.LevelEnd p -> Model.LevelEnd p

