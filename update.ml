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

let update_planet_collidable ?(inp=None) ?(no_forces=false) delta bodies base =
  let open Moonshot.Body in

  let b = base in
  let b' = (match List.find_opt (fun b2 -> bodies_touch b.body b2) bodies with
            | None -> b
            | Some planet ->
               let (vx, vy) = vector b.vel in

               let normal_dir = angle_from_vectors b.body.pos planet.pos in
               let rotate_forward = rotate normal_dir in
               let rotate_back = rotate @@ -. normal_dir in

               let (vpar, vper) = rotate_forward (vx, vy) in
               let vpar_jump = -0.5 *. vpar in
               let vpar = 0.0 in

               (* TODO: player controls *)
               let input_force = 20.0 in
               let jump_force = 18.0 in
               let (vpar, vper) = match inp with
                 | None -> (vpar, vper)
                 | Some inp ->
                    match inp with
                    | Moonshot.Player.CW -> (vpar, vper +. input_force)
                    | Moonshot.Player.CCW -> (vpar, vper -. input_force)
                    | Moonshot.Player.Jump -> (vpar_jump -. jump_force, vper)
                    | _ -> (vpar, vper)
               in

               (* Friction *)
               let friction = 0.50 in
               let vper = friction *. vper in
               let vper = if (Float.abs vper) < 2.0 then 0.0 else vper in

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


  let b''' = if no_forces then b' else b'' in
  (b''', (b_fx, b_fy))

let update_player delta bodies static fading player =
  let {Moonshot.Player.head=float; feet=base; input; health; last_damaged_at} = player in
  (* player is like a balloon attached to a rock at a fixed distance *)
  let open Moonshot.Body in

  let ouchies = List.filter (fun x -> x.surface = Painful) static in
  let touching_ouchy_planets = List.exists (fun b2 -> ignore (b2.surface); bodies_touch base.body b2.body) ouchies in
  let stickies = List.filter (fun x -> x.surface = Sticky) static in
  let touching_sticky_planets = List.exists (fun b2 -> ignore (b2.surface); bodies_touch base.body b2.body) stickies in
  let bouncies = List.filter (fun x -> x.surface = Bouncy) static in
  let touching_bouncy_planet = List.exists (fun b2 -> ignore (b2.surface); bodies_touch base.body b2.body) bouncies in
  let in_any_explosion = List.exists (fun b2 -> ignore(b2.remaining);
                                                   bodies_touch base.body b2.body) fading in
  let damaged = (in_any_explosion || touching_ouchy_planets) &&
                  (last_damaged_at +. Moonshot.damage_cooldown < get_time ()) in
  let health = if damaged then health - 1 else health in
  let input = if damaged || touching_bouncy_planet then Player.Jump else input in
  let last_damaged_at = if damaged then get_time () else last_damaged_at in

  (* Calculate new base *)
  let (new_b, (b_fx, b_fy)) = update_planet_collidable ~inp:(Some input) delta bodies base in
  let new_f = float in
  let new_b = if not touching_sticky_planets then new_b else base in

  (* Calculate new float *)
  let gravity_dir = -.Float.pi /.2.0  -. Float.atan2 b_fx b_fy in
  let seperation = 1.0 in
  Vector2.set_x new_f.body.pos ((Vector2.x new_b.body.pos) +. seperation *. Float.cos gravity_dir);
  Vector2.set_y new_f.body.pos ((Vector2.y new_b.body.pos) +. seperation *. Float.sin gravity_dir);

  {player with Moonshot.Player.head=new_f; feet=new_b; health; last_damaged_at}

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


let create_player_bullets time player =
  let open Moonshot.Player in
  match player.input with
  | Moonshot.Player.Fire (ax, ay) ->
     [{Body.created_at=time; moving=create_bullet_from_aim ax ay player.head}]
  | _ -> []

let explosion_from_body time x =
  let open Body in
  let t = time -. x.created_at in
  let x = x.moving in
  (t, {remaining=Moonshot.explosion_time;
       body={x.body with radius=Moonshot.explosion_radius; mass= Moonshot.explosion_mass}})

let elastic_collision body planet =
  let open Body in
  ignore (planet.surface);
  let normal_dir = angle_from_vectors body.body.pos planet.body.pos in
  let rotate_forward = rotate normal_dir in
  let rotate_back = rotate @@ -. normal_dir in
  let vx, vy = vector body.vel in
  let (vpar, vper) = rotate_forward (vx, vy) in
  let vx, vy = rotate_back (-.Float.abs(vpar), vper) in
  let vel = Vector2.create vx vy in
  { body with vel }

let update_bullet delta static bodies bullet =
  let open Body in
  let movable = bullet.moving in
  let moving = update_body delta bodies movable in
  let bouncies = List.filter (fun x -> x.surface = Bouncy) static in
  let touching_bouncy_planet = List.find_opt (fun b2 -> ignore (b2.surface);
                                                      bodies_touch bullet.moving.body b2.body)
                                 bouncies in
  match touching_bouncy_planet with
  | None -> { bullet with moving }
  | Some p -> { bullet with moving=(elastic_collision moving p) }

let update_enemies delta bodies fading enemies =
  let open Moonshot.Enemy in
  let open Moonshot.Body in
  let in_any_explosion b1 = List.exists (fun b2 -> ignore(b2.remaining);
                                                   bodies_touch b1.loc.body b2.body) fading in
  let on_a_planet b1 = List.exists (fun b2 -> bodies_touch b1.loc.body b2) bodies in
  let enemies = List.map (fun x -> if in_any_explosion x then (* check if dead *)
                                     {x with action=Dead 2.0} else x) enemies in
  let enemies = List.map (fun x -> {x with action=(match x.action with (* update death time *)
                                                   | Dead y -> Dead (y -. delta)
                                                   | _ -> x.action)}) enemies in
  let enemies = List.filter (fun x -> match x.action with (* remove long dead bodies *)
                                      | Dead y -> y > 0.0
                                      | _ -> true) enemies in

  let update_enemy e =
    let (loc, (fx, fy)) = if not (on_a_planet e) then
                            update_planet_collidable delta bodies e.loc
                          else update_planet_collidable ~no_forces:true delta bodies e.loc in
    {e with loc; angle=(Float.atan2 fy fx)} in
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
  let bullets = model.bullets in
  let longest_bullet = List.fold_left
                         (fun a x -> let open Body in Float.max a (model.runtime -. x.created_at))
                         model.longest_bullet bullets in

  let end_conditions = [
      ((fun _ -> model.player.health <= 0), Model.Died);
      ((fun _ -> let (x, y) = vector model.player.feet.body.pos in
                 (x *. x +. y *. y) > 100.0 *. 100.0 &&
                   not (List.exists (fun p -> let open Body in
                                              ignore (p.surface);
                                              let (px, py) = vector p.body.pos in
                                              let dx, dy = (px -. x), (py -. y) in
                                              100.0 *. 100.0 > (dx *. dx +. dy *. dy)
                          ) model.static)), Model.DriftedAway);
      ((fun _ -> 0 = List.length model.enemies), Model.Victory);
    ] in
  match List.find_opt (fun (a, _) -> a ()) end_conditions with
  | Some (_, reason) ->
     let le = {Model.reason=reason;
               id=model.id;
               name=model.name;
               runtime=model.runtime;
               longest_bullet;
               shots_taken=model.shots_taken;
               star_reqs=model.star_reqs;
               health=model.player.health} in
     if reason = Victory then Savedata.save le;
     Model.LevelEnd le
  | None -> Model.Playing model

let update_playing delta model =
  let open Moonshot.Body in
  let { Moonshot.Model.bullets=bullets; static; fading; player;
        enemies; cam; runtime; shots_taken; longest_bullet; _ } = model in
  let enemy_body x = let open Enemy in x.loc.body in
  let explosion_body x = ignore (x.remaining); x.body in
  let planet_body x = ignore (x.surface); x.body in
  let bullet_body x = ignore (x.created_at); x.moving.body in
  let body_set ?(bullets=[]) ?(static=[]) ?(fading=[]) ?(enemies=[]) _ =
    List.concat [List.map explosion_body fading;
                 List.map enemy_body enemies;
                 List.map bullet_body bullets;
                 List.map planet_body static]
  in
  let fading = List.map (fun x -> let percent = x.remaining /. Moonshot.explosion_time in
                                  let r = Moonshot.explosion_radius /. 2.0 in
                                  {remaining=x.remaining -. delta;
                                   body={x.body with mass= percent *. Moonshot.explosion_mass;
                                                     radius= r +. r *. percent}}) fading in
  let fading = List.filter (fun x -> x.remaining > 0.0) fading in
  let movables = List.map (update_bullet delta static (body_set ~fading ~static ())) bullets in
  let in_any_static b1 = List.exists (fun b2 -> bodies_touch b1.moving.body b2)
                           (body_set ~fading ~static:(List.filter (fun x -> x.surface <> Body.Bouncy) static) ~enemies ()) in
  let (dead, alive) = List.partition in_any_static movables in
  let create_explosions = List.map (explosion_from_body runtime) dead in
  let bullet_time = List.map (fun (a, _) -> a) create_explosions in
  let longest_bullet = List.fold_left (fun a x -> Float.max a x) longest_bullet bullet_time in
  let create_explosions = List.map (fun (_, a) -> a) create_explosions in
  let fading = List.concat [fading; create_explosions] in
  let player = update_player delta (body_set ~fading ~static ()) static fading player in
  let player_bullets = create_player_bullets runtime player in
  let shots_taken = shots_taken + List.length player_bullets in
  let new_bullets = List.concat [alive; player_bullets] in
  let enemies = update_enemies delta (body_set ~fading ~static ()) fading enemies in
  let living_enemies = List.filter (fun x -> Moonshot.Enemy.is_alive x) enemies in
  let cam = update_camera cam player in
  let runtime = if 0 = List.length living_enemies then runtime else runtime +. delta in
  update_playing_check_for_level_end
    { model with bullets=new_bullets; fading; player; enemies; cam; runtime; shots_taken; longest_bullet }

let update delta model =
  match model with
  | Model.Paused p -> Model.Paused p
  | Model.Playing p -> update_playing delta p
  | Model.WorldSelect -> Model.WorldSelect
  | Model.LevelSelect p -> Model.LevelSelect p
  | Model.MenuScreen -> Model.MenuScreen
  | Model.StatsScreen p -> Model.StatsScreen p
  | Model.LevelEnd p -> Model.LevelEnd p

