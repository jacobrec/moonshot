open Raylib

let ssize = 65
let screen_width = 16 * ssize
let screen_height = 9 * ssize

let pixels_per_meter = float_of_int ssize /. 10.0
let meters_per_pixel = 1.0 /. pixels_per_meter

let vector v =
  (Vector2.x v, Vector2.y v)

let screen_of_world w =
  int_of_float (w *. pixels_per_meter)

let world_of_screen p =
   (float_of_int p) *. meters_per_pixel

let screen_of_world_vector v =
  let x = (screen_of_world @@ Vector2.x v) + screen_width / 2 in
  let y = (screen_height - (screen_of_world @@ Vector2.y v) - 1) - screen_height / 2 in
  (x, y)

let world_of_screen_vector v =
  let x = int_of_float @@ Vector2.x v in
  let x' = x - screen_width / 2 in
  let x'' = world_of_screen x' in
  let y = int_of_float @@ Vector2.y v in
  let y' = (screen_height - y - 1) - screen_height / 2 in
  let y'' = world_of_screen y' in
  (x'', y'')

let sofw = screen_of_world
let wofs = world_of_screen
let sofwv = screen_of_world_vector
let wofsv = world_of_screen_vector


module Body = struct
  type t = {
      pos: Vector2.t; (* in meters *)
      mass: float; (* in kg *)
      radius: float; (* in meters *)
    }
  type fading = {
      remaining: float; (* in seconds *)
      body: t;
    }
  type moving = {
      vel: Vector2.t; (* in m/s *)
      body: t;
    }
end

module Player = struct
  type input_type =
    | CW
    | CCW
    | Jump
    | Aiming of float * float
    | Fire of float * float
    | None

  type t = {
      head : Body.moving;
      feet : Body.moving;
      input: input_type;
  }
end

let explosion_time = 1.0
let explosion_from_body x =
  let open Body in
  {remaining=explosion_time; body={x.body with radius=3.0; mass= -100.0}}

module Model = struct
  type t = {
      static : Body.t list;
      bullets : Body.moving list;
      fading : Body.fading list;
      player : Player.t;
    }
end

let setup () =
  init_window screen_width screen_height "test1";
  set_target_fps 60;

  let bodies = [
      {Body.pos=Vector2.create 0.0 0.0; mass=900.0; radius=10.0;};
      {Body.pos=Vector2.create 30.0 0.0; mass=300.0; radius=5.0;};
      (* {Body.pos=Vector2.create 40.0 0.0; mass= -200.0; radius=1.0;}; *)
    ] in

  let movables = [
      {Body.body={Body.pos=Vector2.create (-30.0) 0.0; mass=1.0; radius=0.5;};
       vel=Vector2.create 0.0 15.0}
        (* {Body.pos=Vector2.create 30.0 0.0; mass=100.0; radius=5.0;}; *)
    ] in
  let vc = Vector2.create in
  { Model.static=bodies;
    fading=[];
    player={
      Player.feet={Body.body={pos=vc (-30.0) 0.0; mass=  10.0; radius=0.5;}; vel=vc 1.0 0.0};
      Player.head={Body.body={pos=vc (-30.0) 1.0; mass= -3.5; radius=0.5;}; vel=vc (-1.0) 0.0};
      input=Player.None
    };
    bullets=movables }


let gravity' m1 m2 r =
  let _g = 6.6743e-11 in
  let g = 10.0 in
  g *. (m1 *. m2) /. (r *. r)


(* calculates the force on b1 from b2 *)
let gravity b1 b2 =
  let open Body in
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
  let open Body in
  let (vel_x, vel_y) = (Vector2.x b.vel +. acc_x *. delta, Vector2.y b.vel +. acc_y *. delta) in
  let (pos_x, pos_y) = (Vector2.x b.body.pos +. vel_x *. delta,
                        Vector2.y b.body.pos +. vel_y *. delta) in
  let pos = Vector2.create pos_x pos_y in
  let vel = Vector2.create vel_x vel_y in
  {Body.vel=vel; body={b.body with pos=pos}}

let update_body delta bodies b1 =
  let open Body in
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
  let open Body in
  check_collision_circles b1.pos b1.radius b2.pos b2.radius

let rotate theta (x, y) =
  let x' =   (x *. Float.cos theta) +. (y *. Float.sin theta) in
  let y' = -.(x *. Float.sin theta) +. (y *. Float.cos theta) in
  (x', y')


let update_player delta bodies player =
  let {Player.head=float; feet=base; input=inp} = player in
  (* player is like a balloon attached to a rock at a fixed distance *)
  let open Body in

  (* Calculate forces *)
  let (b_fx, b_fy) = gravity_from_many base.body bodies in
  let (b_accx, b_accy) = (b_fx /. base.body.mass, b_fy /. base.body.mass) in
  let new_b = accelerate_moving_body delta base (b_accx, b_accy) in
  let new_f = float in

  let gravity_dir = -.Float.pi /.2.0  -. Float.atan2 b_fx b_fy in
  let seperation = 1.0 in
  Vector2.set_x new_f.body.pos ((Vector2.x new_b.body.pos) +. seperation *. Float.cos gravity_dir);
  Vector2.set_y new_f.body.pos ((Vector2.y new_b.body.pos) +. seperation *. Float.sin gravity_dir);

  (* Planet Collision *)
  let b = new_b in
  (match List.find_opt (fun b2 -> bodies_touch b.body b2) bodies with
  | None -> ()
  | Some planet ->
     let e = 0.5 in
     let (vx, vy) = vector b.vel in

     let normal_dir = angle_from_vectors b.body.pos planet.pos in
     let rotate_forward = rotate normal_dir in
     let rotate_back = rotate @@ -. normal_dir in

     let (vpar, vper) = rotate_forward (vx, vy) in
     let vpar = -.e *. vpar in
     (* player controls *)
     let input_force = 2.0 in
     let jump_force = 20.0 in
     let (vpar, vper) = match inp with
       | Player.CW -> (vpar, vper +. input_force)
       | Player.CCW -> (vpar, vper -. input_force)
       | Player.Jump -> (vpar -. jump_force, vper)
       | _ -> (vpar, vper) in

     let (new_vx, new_vy) = rotate_back (vpar, vper) in

     (* Friction *)
     let friction = 0.90 in
     let (new_vx, new_vy) = (new_vx *. friction, new_vy *. friction) in

     let (px, py) = vector planet.pos in
     let (x, y) = vector b.body.pos in
     let theta = Float.atan2 (y -. py) (x -. px) in
     let radius = b.body.radius +. planet.radius in
     let new_x = px +. radius *. Float.cos theta in
     let new_y = py +. radius *. Float.sin theta in

     Vector2.set_x b.body.pos new_x;
     Vector2.set_y b.body.pos new_y;
     Vector2.set_x b.vel new_vx;
     Vector2.set_y b.vel new_vy);

  {player with Player.head=new_f; feet=new_b}
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
  let open Player in
  match player.input with
    | Player.Fire (ax, ay) ->
       [create_bullet_from_aim ax ay player.head]
    | _ -> []


let update delta model =
  let open Body in
  let { Model.bullets=movables; static=bodies; fading=fading; player=player } = model in
  let fading = List.map (fun x -> { x with remaining =  x.remaining -. delta}) fading in
  let fading = List.filter (fun x -> x.remaining > 0.0) fading in
  let movables = List.map (update_body delta bodies) movables in
  let in_any_static b1 = List.exists (fun b2 -> bodies_touch b1.body b2) bodies in
  let (dead, alive) = List.partition in_any_static movables in
  let create_explosions = List.map explosion_from_body dead in
  let fading = List.concat [fading; create_explosions] in
  let player = update_player delta bodies player in
  let player_bullets = create_player_bullets player in
  let new_bullets = List.concat [alive; player_bullets] in
  { model with bullets=new_bullets; fading=fading; player=player}

let draw_body color b =
  let open Body in
  let r = b.radius in
  let (px, py) = sofwv b.pos in
  let pr = float_of_int @@ sofw r in
  draw_circle px py pr color

let draw_explosion x =
  let open Body in
  let opacity = x.remaining /. explosion_time in
  let c = Color.create 255 0 0 @@ int_of_float (255.0 *. opacity) in
  draw_body c x.body

let draw_dotted_line color max_length x1 y1 x2 y2 =
  let map4 f (a, b, c, d) = (f a, f b, f c, f d) in
  let (x1, y1, x2, y2) = map4 float_of_int (x1, y1, x2, y2) in
  let dx = x2 -. x1 in
  let dy = y2 -. y1 in
  let len = Float.sqrt (dx *. dx +. dy *. dy) in
  let rec calculate_segments segs =
    let mini_length = len /. float_of_int (2 * segs) in
    if mini_length > max_length then
      calculate_segments (segs + 1)
    else (segs, mini_length) in
  let (solid_segments, mini_length) = calculate_segments 1 in
  let theta = Float.atan2 dy dx in
  let extend x y =
    (x +. mini_length *. Float.cos theta,
     y +. mini_length *. Float.sin theta) in
  let rec inner times x y =
    if times = 0 then ()
    else begin
        let (x', y') = extend x y in
        let (x'', y'') = extend x' y' in
        let (a, b, c, d) = map4 int_of_float (x, y, x', y') in
        draw_line a b c d color;
        inner (times - 1) x'' y''
      end in
  inner solid_segments x1 y1

let draw_aim_assist bodies dots freq ax ay player =
  let open Player in
  let timing = 0.0167 in
  let total = int_of_float ((float_of_int dots) /. 0.0167 *. freq) + 1 in
  let bullet = create_bullet_from_aim ax ay player.head in
  let (all, _) = List.fold_left (fun (acc, b) x ->
                     let b' = (update_body x bodies b) in
                     ((b' :: acc), b'))
                   ([], bullet) (List.init total (fun _ -> timing)) in
  let filteri f l =
    let rec inner f l i =
      match l with
      | h :: t -> if f i h then h :: (inner f t (i + 1)) else (inner f t (i + 1))
      | [] -> [] in
    inner f l 0 in
  let every_few = filteri (fun i _ -> i mod (int_of_float (freq /. timing) + 1) = 0) all in
  List.iter (fun x -> let open Body in draw_body Color.gray x.body) every_few

let draw model =
  let { Model.bullets=movables; static=bodies; fading=fading;
        player={feet=pfeet; head=phead; input=inp} } = model in
  let player = model.player in
  begin_drawing ();
  clear_background Color.raywhite;
  List.iter (draw_body Color.beige) bodies;
  List.iter (fun x -> let open Body in draw_body Color.black x.body) movables;
  List.iter draw_explosion fading;
  List.iter (draw_body Color.lime) @@
    List.map (fun x -> let open Body in x.body) [phead; pfeet];
  (match inp with
   | Player.Aiming (ax, ay) ->
      draw_aim_assist bodies 5 0.15 ax ay player;
      let (px, py) = vector phead.body.pos in
      let (ax, ay) = truncate_aim ax ay px py in
      let (ax, ay) = sofwv (Vector2.create ax ay) in
      let (px, py) = sofwv phead.body.pos in
      draw_dotted_line Color.gray 10.0 ax ay px py;
   | _ -> ());
  end_drawing ();
  model

let input_mouse_or_touch _ =
  if true then
    ((is_mouse_button_down MouseButton.Left), get_mouse_position ())
  (* TODO: detect if mobile, and probably will need a better way for this*)
  else ((0 < Raylib.get_touch_points_count ()), get_touch_position 0)

let input model =
  let open Model in
  let ccw = is_key_down Key.A in
  let cw = is_key_down Key.S in
  let jump = is_key_down Key.Space in
  let (touched, tv) = input_mouse_or_touch () in
  let (tx, ty) = wofsv tv in
  let (px, py) = vector model.player.head.body.pos in
  let touch_catch_size = 10.0 in
  let inp =
    if (match model.player.input with | Player.Aiming _ -> true | _ -> false) &&
         not touched then
      match model.player.input with
      | Player.Aiming (x, y) -> Player.Fire (x, y)
      | _ -> raise Not_found (*this is checked in the condition*)
    else if (match model.player.input with | Player.Aiming _ -> false | _ -> true)
            && touched && (((Float.pow (tx -. px) 2.0) +.
                              (Float.pow (ty -. py) 2.0)) <
                 touch_catch_size *. touch_catch_size) then
      Player.Aiming (tx, ty)
    else if (match model.player.input with | Player.Aiming _ -> true | _ -> false)
            && touched then
      Player.Aiming (tx, ty)
    else if jump then
      Player.Jump
    else if not ccw && cw then
      Player.CW
    else if ccw && not cw then
      Player.CCW
    else Player.None in

  {model with player={model.player with input=inp}}

let rec loop model =
  if window_should_close () then close_window () else
    model
    |> input
    |> update (get_frame_time ())
    |> draw
    |> loop


let () = setup () |> loop
