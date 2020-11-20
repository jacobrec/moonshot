open Raylib
open Moonshot

let draw_body color b =
  let open Moonshot.Body in
  let r = b.radius in
  let (px, py) = sofwv b.pos in
  let pr = float_of_int @@ sofw r in
  draw_circle px py pr color

let draw_explosion x =
  let open Moonshot.Body in
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

let draw_aim_assist static dots freq ax ay player =
  let open Moonshot.Player in
  let open Moonshot.Body in
  let (bouncy, not_bouncy) = List.partition (fun x -> x.surface = Body.Bouncy) static in
  let bodies = List.map (fun x -> ignore (x.surface); x.body) static in
  let is_in_bouncy b =
    List.find_opt (fun x -> ignore (x.surface); Update.bodies_touch x.body b) bouncy in
  let should_dead b =
    List.exists (fun x -> ignore (x.surface); Update.bodies_touch x.body b) not_bouncy in
  let timing = 0.0167 in
  let total = int_of_float ((float_of_int dots) /. 0.0167 *. freq) + 1 in
  let bullet = Update.create_bullet_from_aim ax ay player.head in
  let (all, _, _) = List.fold_left (fun (acc, b, stop) x ->
                        if stop then (acc, b, stop) else
                          let b' = (Update.update_body x bodies b) in
                          let b'' = match is_in_bouncy b'.body with
                            | Some p -> Update.elastic_collision b' p
                            | None -> b' in
                          ((b'' :: acc), b'', should_dead b''.body))
                   ([], bullet, false) (List.init total (fun _ -> timing)) in
  let filteri f l =
    let rec inner f l i =
      match l with
      | h :: t -> if f i h then h :: (inner f t (i + 1)) else (inner f t (i + 1))
      | [] -> [] in
    inner f l 0 in
  let every_few = filteri (fun i _ -> i mod (int_of_float (freq /. timing) + 1) = 0) all in
  List.iter (fun x -> let open Moonshot.Body in draw_body Color.darkgray x.body) every_few

let draw_texture text wloc ang size =
  let psize = sofw size in
  let tsize = Texture2D.width text in
  let sf = (float_of_int psize) /. (float_of_int tsize) in
  let p = sofwv_v wloc in
  let p_ang = 270.0 -. ang *. 180.0 /. Float.pi in
  let px, py = vector p in
  let p_off = (float_of_int tsize *. sf) /. 2.0 in
  let p = Vector2.create (px -. (p_off *. Float.cos ang) +. p_off *. Float.sin ang)
            (py +. (p_off *. Float.sin ang) +. p_off *. Float.cos ang) in

  if Moonshot.true_draw then draw_texture_ex text p p_ang sf Color.white

let draw_texture_hud text wloc size =
  let psize = size in
  let tsize = Texture2D.width text in
  let sf = (float_of_int psize) /. (float_of_int tsize) in
  let p = wloc in
  let px, py = vector p in
  let p_off = (float_of_int tsize *. sf) /. 2.0 in
  let p = Vector2.create (px -. p_off) (py -. p_off) in

  if Moonshot.true_draw then draw_texture_ex text p 0.0 sf Color.white

let draw_enemy e =
  let open Moonshot.Enemy in
  let c, t = match e.action with
    | Dead _ -> Color.darkblue, Images.get Images.EnemyDead
    | Shielded -> Color.skyblue, Images.get_animation Images.EnemyStanding
    | _ -> Color.blue, Images.get_animation Images.EnemyStanding in

  let size = 2.0 in
  draw_texture t e.loc.body.pos e.angle size;
  if Moonshot.debug_draw then
    draw_body c e.loc.body

let draw_player ani pfeet phead =
  let open Moonshot.Body in
  let (pxh, pyh) = vector phead.body.pos in
  let (pxf, pyf) = vector pfeet.body.pos in
  let p_angr = Float.atan2 (pyf -. pyh) (pxf -. pxh) in
  let size = 3.5 in
  let tex = match ani with
    | Player.Standing -> (Images.get Images.PlayerStanding)
    | Player.Walking -> Images.get_animation Images.PlayerWalking
    | Player.WalkingReverse -> Images.get_animation Images.PlayerWalkingReverse
    | Player.Falling -> Images.get_animation Images.PlayerFalling in

  draw_texture tex phead.body.pos p_angr size;
  if Moonshot.debug_draw then
    List.iter (draw_body Color.lime) @@ List.map (fun x -> x.body) [phead; pfeet]

let draw_blocky_circle x y r ci co =
  let rf = r in
  let r = int_of_float r in
  let size = sofw 0.5 in
  let square x = x * x in

  let lerp a b x =
    a + (int_of_float (float_of_int (b - a) *. x)) in
  let lerp_color c1 c2 x =
    let steps = 100 in
    let x = float_of_int ((int_of_float (x *. (float_of_int steps)))) /. (float_of_int steps) in
    let (r1, g1, b1, a1) = c1 in
    let (r2, g2, b2, a2) = c2 in
    lerp r1 r2 x, lerp g1 g2 x, lerp b1 b2 x, lerp a1 a2 x in

  let tuple_color (r, g, b, a) = Color.create r g b a in

  let variance = 10 in
  let calc_for_stripes x =
        (x/10) mod 10 in
  let calc_for_specals x =
    let xf = float_of_int x in
    int_of_float ((float_of_int variance) *. Float.sin xf) in
  let calc_for_slights x =
    let xf = float_of_int x in
    let ans = (Float.sin (xf *. 2.0) +. Float.sin (xf *. Float.pi)) in
    int_of_float ((float_of_int variance) *. ans) in
  let calc_for_noise x =
        let fx = float_of_int x in
        let rand = Noise.get fx in
        int_of_float @@ (rand *. (float_of_int variance)) in
  let shake_color seed (r, g, b, a) =
    let seed = seed + r + g + b in
    let calc_random x =
      if r mod 2 = 0 then
        calc_for_noise x
      else if r mod 3 = 0 then
        calc_for_specals x
      else if r mod 5 = 0 then
        calc_for_slights x
      else
        calc_for_stripes x in
    let bound_rand upper seed value = min (max (value + (calc_random seed) - variance / 2) 0) upper in
    let h, s, v = Colorutils.hsv_of_rgb (r, g, b) in
    let r', g', b' = Colorutils.rgb_of_hsv (h, bound_rand 99 seed s, v) in
    (r', g', b', a) in

  if Moonshot.true_draw then
    List.init (2 * r) (fun i -> x - r + i * size)
    |> List.map (fun x -> List.init (2 * r) (fun i -> x, (y - r + i * size)))
    |> List.flatten
    |> List.filter (fun (px, py) -> square (py - y + size/2) + square (px - x + size/2) <
                                      int_of_float (rf *. rf))
    |> List.map (fun (px, py) -> let d2 = square (py - y + size/2) + square (px - x + size/2) in
                                 let d = Float.sqrt (float_of_int d2) in
                                 let p = d /. rf in
                                 let shake_seed = ((px + py * 2 * r + (x + y * 10000) * 1000000)* 10) in
                                 px, py, shake_color shake_seed @@ lerp_color ci co p)
    |> List.iter (fun (x, y, c) -> draw_rectangle x y size size (tuple_color c)) ;

  let ci = tuple_color ci in
  let co = tuple_color co in
  if Moonshot.debug_draw then draw_circle_gradient x y rf ci co

let draw_planet p =
  let open Body in
  let surface = p.surface in
  let p = p.body in
  let density = p.mass /. (p.radius *. p.radius) in
  let adensity = Float.abs (p.mass /. (p.radius *. p.radius)) in
  let color_create r g b a = (r, g, b, a) in
  let color =
    if adensity > 30.0 then color_create 40 10 75 255 (* dark purple *)
    else if adensity > 25.0 then color_create 80 80 80 255 (* darkgray *)
    else if adensity > 20.0 then color_create 127 106 79 255 (* brown *)
    else if adensity > 15.0 then color_create 130 130 130 255 (* gray *)
    else if adensity > 10.0 then color_create 255 203 0 255 (* gold *)
    else if adensity > 5.0 then color_create 211 176 131 255 (* beige *)
    else color_create 200 200 200 255 (* lightgray *) in
  let r = p.radius in
  let (px, py) = sofwv p.pos in
  let pr = float_of_int @@ sofw r in
  let color_inner, color_outer =
    match surface with
    | Body.Bouncy -> color, color_create 255 255 255 255 (* white *)
    | Body.Painful -> color, color_create 230 41 55 255 (* red *)
    | Body.Sticky  -> color, color_create 255 109 194 255 (* pink *)
    | Body.Slippery  -> color, color_create 102 191 255 255 (* lightblue *)
    | Body.Normal when density < 0.0 -> color_create 0 0 0 0 (* blank *), color
    | Body.Normal  -> color, color in
  draw_blocky_circle px py pr color_inner color_outer

let draw_powerup powerup =
  let { Powerup.power=power; pos } = powerup in
  let x, y = sofwv pos in
  let size = float_of_int @@ sofw @@ Powerup.draw_size power in
  let gfx = match power with
    | Powerup.Fireblast -> Color.red in

  draw_circle x y size gfx

let measure_text_wh text size =
  let f = get_font_default () in
  let base_size = Font.base_size f in
  let spacing = ((float_of_int size) /. (float_of_int base_size)) in
  let v = measure_text_ex f text (float_of_int size) spacing in
  let (fx, fy) = vector v in
  (int_of_float fx, int_of_float fy)

let draw_centered_text text cx cy font color =
  let (width, height) = measure_text_wh text font in
  draw_text text (cx - width / 2) (cy - height / 2) font color

let line_wrapped_text text font width =
  let words = String.split_on_char ' ' text in
  let lines = List.fold_left (fun lines x ->
                  let lline = List.hd lines in
                  let rest = List.tl lines in
                  let lline_x = lline ^ x in
                  let twidth = measure_text lline_x font in
                  if twidth < width then (lline_x ^ " ") :: rest
                  else (x ^ " ") :: lline :: rest
                  ) [""] words in
  let text = String.concat "\n" @@ List.rev lines in
  let (_, y) = measure_text_wh text font in
  (text, font/2 + y)


let draw_text_box ?(centered=false) font x y width fg_color bg_color text =
  let (text, height) = line_wrapped_text text font (width - font) in
  let x = if centered then x - width/2 else x in
  let y = if centered then y - height/2 else y in
  draw_rectangle x y (width+font) (height+font) bg_color;
  draw_centered_text text (x + width/2) (height/2 + y) font fg_color


let box_width = 0.8
let box_width = int_of_float (box_width *. (float_of_int Moonshot.screen_width))
let level_textbox = draw_text_box (Moonshot.font_size * 3) ((Moonshot.screen_width-box_width) / 2) 0
                      box_width Color.raywhite (Color.create 49 49 49 100)

let draw_playing_starfield stars px py =
  let star_backoff = 5.0 in
  let (px, py) = (px /. star_backoff, py /. star_backoff) in
  Starfield.draw stars px py (Moonshot.sofw (1.0 /. star_backoff)) Color.raywhite

let draw_playing model =
  let { Moonshot.Model.bullets=movables; static; fading;
        player={animation_state=ani; feet=pfeet; head=phead; input=inp; health; _}; powerups;
        enemies; cam; runtime; _} = model in
  let player = model.player in
  begin_drawing ();
  clear_background Color.black;
  (* IDK if the starfield effect actually looks good*)
  let (px, py) = vector phead.body.pos in
  draw_playing_starfield model.stars px py;
  begin_mode_2d cam;
  List.iter draw_planet static;

  (* Draw bullets *)
  List.iter (fun x -> let open Moonshot.Body in
                      draw_texture (Images.get_animation Images.BulletFlying)
                        x.moving.body.pos 0.0 (2.0 *. x.moving.body.radius);
                      if Moonshot.debug_draw then
                        draw_body Color.gray x.moving.body
    ) movables;

  (* Draw powerups *)
  List.iter draw_powerup powerups;

  List.iter draw_explosion fading;
  (* Draw Player*)
  draw_player ani pfeet phead;

  (* Draw Enemies*)
  List.iter draw_enemy enemies;

  (* Draw Aiming lines*)
  (match inp with
   | Moonshot.Player.Aiming (ax, ay) ->
      draw_aim_assist static Moonshot.aim_assist_dots 0.15 ax ay player;
      let (px, py) = vector phead.body.pos in
      let (ax, ay) = Update.truncate_aim ax ay px py in
      let (ax, ay) = sofwv (Vector2.create ax ay) in
      let (px, py) = sofwv phead.body.pos in
      draw_dotted_line Color.gray 10.0 ax ay px py;
   | _ -> ());
  end_mode_2d ();

  (* Draw HUD *)
  let heart_radius = (float_of_int Moonshot.ssize) /. 6.0 in
  let heart_space = 3 * (int_of_float heart_radius) in
  let heart_offset = (int_of_float (1.5 *. heart_radius)) in
  let draw_hearts i =
    let xs = List.init i (fun i -> heart_offset + heart_space * i) in
    List.iter (fun x ->
        let fx, fy = float_of_int x, float_of_int heart_offset in
        draw_texture_hud (Images.get Images.HeartFull) (Vector2.create fx fy) (3*(int_of_float heart_radius));
        if Moonshot.debug_draw then draw_circle x heart_offset heart_radius Color.red
      ) xs in
  let draw_half_heart x =
    let fx, fy = float_of_int x, float_of_int heart_offset in
    draw_texture_hud (Images.get Images.HeartHalf) (Vector2.create fx fy) (3*(int_of_float heart_radius));
    if Moonshot.debug_draw then draw_circle x heart_offset (heart_radius/.2.0) Color.red
  in
  draw_hearts (health / 2);
  if health mod 2 == 1 then
    draw_half_heart (heart_offset + heart_space * (health / 2));

  let fsize = Moonshot.font_size in
  let trunc_time = (float_of_int (int_of_float (runtime *. 100.0))) /. 100.0 in
  draw_text (string_of_float trunc_time) (Moonshot.screen_width - (fsize * 5)) fsize fsize Color.gold;

  if runtime < 5.0 then
    level_textbox model.start_text;

  end_drawing ();
  Model.Playing model

let line_drawer font cx sy color =
  let offset = ref 0 in
  let draw_line ?(size=font) ?(centered=true) text =
    (if centered then draw_centered_text else draw_text) text cx (sy + !offset) size color;
    offset  := !offset + size in
  draw_line

let draw_paused model =
  let open Model in
  begin_drawing ();
  clear_background Color.black;
  let (px, py) = vector model.player.head.body.pos in
  draw_playing_starfield model.stars px py;
  let ld = line_drawer (Moonshot.font_size*3)
             (Moonshot.screen_width/2) (Moonshot.screen_height/4) Color.raywhite in
  ld ~size:(Moonshot.font_size*6) "Paused";
  ld "(R)estart";
  ld "Re(s)ume";
  ld "(Q)uit";
  end_drawing ();
  Model.Paused model


let menu_starfield = Starfield.create 3 800
let draw_menu_starfield _ =
  Starfield.draw menu_starfield (10.0 *. get_time ()) (get_time ())
    (Moonshot.sofw 1.0) Color.raywhite
let draw_menuscreen _ =
  let f_color = Color.raywhite in
  let b_color = Color.black in
  let big_size = 10 in
  let little_size = 3 in

  let little_font_size = little_size * Moonshot.font_size in
  let big_font_size = big_size * Moonshot.font_size in
  begin_drawing ();
  clear_background b_color;
  draw_menu_starfield ();
  draw_centered_text "Starshot" (Moonshot.screen_width / 2) (Moonshot.screen_height / 3)
    big_font_size f_color;
  draw_centered_text "Press [space] to play"
    (Moonshot.screen_width / 2) (3 * Moonshot.screen_height / 4)
    little_font_size f_color;
  draw_centered_text "Press [s] to view stats"
    (Moonshot.screen_width / 2) (3 * Moonshot.screen_height / 4 + little_font_size)
    little_font_size f_color;
  end_drawing ();
  Model.MenuScreen

let draw_stars fcolor scolor has_health_star has_shot_star has_time_star =
  let ty = (6 * Moonshot.screen_height / 8) in
  let cx = Moonshot.screen_width / 2 in
  let x1 = (cx-cx/2) in
  let x2 = cx in
  let x3 = (cx+cx/2) in
  draw_centered_text "Health" x1 ty (Moonshot.font_size*3) fcolor;
  draw_centered_text "Shots" x2 ty (Moonshot.font_size*3) fcolor;
  draw_centered_text "Time" x3 ty (Moonshot.font_size*3) fcolor;
  let draw_star x =
    let r = Moonshot.ssize / 2 in
    let y = ty - (2*r) in
    let xf = float_of_int x in
    let yf = float_of_int y in
    if Moonshot.debug_draw then draw_circle x y (float_of_int r) scolor;
    draw_texture_hud (Images.get_animation Images.StarShine) (Vector2.create xf yf) (2*r)
    in

  if has_health_star then draw_star x1;
  if has_shot_star then draw_star x2;
  if has_time_star then draw_star x3

let draw_levelend model =
  let open Moonshot.Model in
  let fcolor = Color.raywhite in
  let bcolor = Color.create 49 49 49 200 in
  begin_drawing ();
  clear_background Color.black;
  draw_menu_starfield ();

  let msg = match model.reason with
    | Victory -> "Victory!!"
    | DriftedAway -> "Lost in space :("
    | Died -> "You Died :(" in
  let has_time_star = (model.reason=Victory) && model.star_reqs.time >= model.runtime in
  let has_shot_star = (model.reason=Victory) && model.star_reqs.shots >= model.shots_taken in
  let has_health_star = (model.reason=Victory) && model.star_reqs.health <= model.health in

  let damage_taken =(float_of_int (6-model.health) /. 2.0) in

  let width = 3 * Moonshot.screen_width / 4 in
  let height = 3 * Moonshot.screen_height / 4 in
  let cx = Moonshot.screen_width / 2 in
  let cy = Moonshot.screen_height / 2 in
  let ld = line_drawer (Moonshot.font_size*2)
             cx (Moonshot.screen_height/4) fcolor in
  draw_rectangle (cx - width / 2) (cy - height / 2) width height bcolor;
  ld ~size:(Moonshot.font_size*4) ("Level " ^ model.name ^ ": " ^ msg);
  ld (Printf.sprintf "You took %d shots" model.shots_taken);
  ld (Printf.sprintf "It took you %.2f seconds" model.runtime);
  ld (Printf.sprintf "You took %.1f damage" damage_taken);
  ld (Printf.sprintf "Your longest shot stayed in orbit for %.2f seconds" model.longest_bullet);
  ld "Press [space] to continue or [r] to retry level";

  draw_stars fcolor Color.gold has_health_star has_shot_star has_time_star;

  end_drawing ();
  Model.LevelEnd model

let draw_stats p =
  let f_color = Color.raywhite in
  let b_color = Color.black in
  begin_drawing ();
  clear_background b_color;
  draw_menu_starfield ();

  let bcolor = Color.create 49 49 49 200 in
  let width = 3 * Moonshot.screen_width / 4 in
  let height = 3 * Moonshot.screen_height / 4 in
  let cx = Moonshot.screen_width / 2 in
  let cy = Moonshot.screen_height / 2 in
  draw_rectangle (cx - width / 2) (cy - height / 2) width height bcolor;

  let ifmul a b = int_of_float ((float_of_int a) *. b) in
  let ld = line_drawer (ifmul Moonshot.font_size 1.8)
             (Moonshot.screen_width/2) (Moonshot.screen_height/4) f_color in
  ld ~size:(Moonshot.font_size*4) "Stats";
  if Option.is_none p then
    ld "Press [space] to return to main menu"
  else
    ld "Press [space] to go up a level";
  let (a, b, c, d) = Savedata.totals () in
  ld (Printf.sprintf "Total shots taken: %d" a);
  ld (Printf.sprintf "Total time in levels: %.2fs" b);
  ld (Printf.sprintf "Total damage recieved: %.1f" c);
  ld (Printf.sprintf "Total stars aquired: %d" d);
  (match p with
   | None -> ld "Press [1-9,0] to select a world for details"
   | Some (p, None) -> ld ("Press [1-9,0,-,=] to select a level in world "
                           ^ (string_of_int (p / 100)) ^ " details")
   | Some (w, Some p) ->
      let font = Moonshot.font_size * 2 in
      let y = (cy + height/2) - font / 2 - 5 in
      draw_centered_text (Printf.sprintf "Selected level %d of world %d" p (w/100)) cx y font f_color;
      let (h, s, t) = Savedata.level (w - 100 + p) in
      draw_stars f_color Color.gold h s t;
  );
  end_drawing ();
  Model.StatsScreen p

let draw_worldselect _ =
  let f_color = Color.raywhite in
  let b_color = Color.black in
  begin_drawing ();
  clear_background b_color;
  draw_menu_starfield ();

  let bcolor = Color.create 49 49 49 200 in
  let width = 3 * Moonshot.screen_width / 4 in
  let height = 3 * Moonshot.screen_height / 4 in
  let cx = Moonshot.screen_width / 2 in
  let cy = Moonshot.screen_height / 2 in
  draw_rectangle (cx - width / 2) (cy - height / 2) width height bcolor;

  let ifmul a b = int_of_float ((float_of_int a) *. b) in
  let ld = line_drawer (ifmul Moonshot.font_size 1.8)
             (Moonshot.screen_width/2) (Moonshot.screen_height/4) f_color in
  ld ~size:(Moonshot.font_size*4) "World Select";
  ld "Press [space] to return to main menu";
  ld "Press [1-9,0] to select a world";
  let lld_y = (Moonshot.screen_height/2) in
  let ldl = line_drawer (ifmul Moonshot.font_size 2.0)
             (5*Moonshot.screen_width/16) lld_y f_color in
  let keys = ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "0"; "-"; "="] in
  List.iter (fun level ->
      ldl ~centered:false (Printf.sprintf "[%s] World %d: %s" (List.nth keys (level-1)) level
                         (List.nth Level.world_names (level - 1))
        );
    ) (List.init (List.length Level.world_names) (fun i -> i+1));
  end_drawing ();
  Model.WorldSelect

let draw_levelselect w =
  let f_color = Color.raywhite in
  let b_color = Color.black in
  begin_drawing ();
  clear_background b_color;
  draw_menu_starfield ();

  let bcolor = Color.create 49 49 49 200 in
  let width = 3 * Moonshot.screen_width / 4 in
  let height = 3 * Moonshot.screen_height / 4 in
  let cx = Moonshot.screen_width / 2 in
  let cy = Moonshot.screen_height / 2 in
  draw_rectangle (cx - width / 2) (cy - height / 2) width height bcolor;

  let ifmul a b = int_of_float ((float_of_int a) *. b) in
  let ld = line_drawer (ifmul Moonshot.font_size 2.0)
             (Moonshot.screen_width/2) (Moonshot.screen_height/4) f_color in
  ld ~size:(Moonshot.font_size*4) ("World "^(string_of_int (w/100))^" Level Select");
  ld "Press [space] to return to world select";
  ld "Press [1-9,0,-,=] to select a level";
  let lld_y = (Moonshot.screen_height/2) in
  let ldl = line_drawer (ifmul Moonshot.font_size 2.0)
             (5*Moonshot.screen_width/16) lld_y f_color in
  let ldr = line_drawer (ifmul Moonshot.font_size 2.0)
             (9*Moonshot.screen_width/16) lld_y f_color in
  let keys = ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "0"; "-"; "="] in
  List.iter (fun level ->
      let (s1, s2, s3) = Savedata.level (w - 100 + level) in
      let star b = if b then "*" else "-" in
      (if level mod 2 = 1 then ldl else ldr)
        ~centered:false (Printf.sprintf "[%s] Level %d (%s%s%s)" (List.nth keys (level-1)) level
                         (star s1) (star s2) (star s3)
        );
    ) (List.init 12 (fun i -> i+1));
  end_drawing ();

  Model.LevelSelect w


let draw model =
  match model with
  | Model.Paused p -> draw_paused p
  | Model.Playing p -> draw_playing p
  | Model.MenuScreen -> draw_menuscreen ()
  | Model.StatsScreen p -> draw_stats p
  | Model.LevelEnd p -> draw_levelend p
  | Model.WorldSelect -> draw_worldselect ()
  | Model.LevelSelect w -> draw_levelselect w
