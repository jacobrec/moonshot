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

let draw_aim_assist bodies dots freq ax ay player =
  let open Moonshot.Player in
  let timing = 0.0167 in
  let total = int_of_float ((float_of_int dots) /. 0.0167 *. freq) + 1 in
  let bullet = Update.create_bullet_from_aim ax ay player.head in
  let (all, _) = List.fold_left (fun (acc, b) x ->
                     let b' = (Update.update_body x bodies b) in
                     ((b' :: acc), b'))
                   ([], bullet) (List.init total (fun _ -> timing)) in
  let filteri f l =
    let rec inner f l i =
      match l with
      | h :: t -> if f i h then h :: (inner f t (i + 1)) else (inner f t (i + 1))
      | [] -> [] in
    inner f l 0 in
  let every_few = filteri (fun i _ -> i mod (int_of_float (freq /. timing) + 1) = 0) all in
  List.iter (fun x -> let open Moonshot.Body in draw_body Color.gray x.body) every_few

let draw_enemy e =
  let open Moonshot.Enemy in
  let c = match e.action with
    | Dead _ -> Color.darkblue
    | _ -> Color.blue in
  draw_body c e.loc.body

let draw_planet p =
  let open Body in
  let pain = p.is_painful in
  let p = p.body in
  let density = p.mass /. (p.radius *. p.radius) in
  let color =
    if density > 30.0 then Color.black
    else if density > 25.0 then Color.black
    else if density > 20.0 then Color.brown
    else if density > 15.0 then Color.gray
    else if density > 10.0 then Color.gold
    else if density > 5.0 then Color.beige
    else Color.lightgray in
  let r = p.radius in
  let (px, py) = sofwv p.pos in
  let pr = float_of_int @@ sofw r in
  if pain then draw_circle_gradient px py pr color Color.red
  else draw_circle px py pr color

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


let draw_text_box font x y width fg_color bg_color text =
  let (text, height) = line_wrapped_text text font (width - font) in
  draw_rectangle x y (width+font) (height+font) bg_color;
  draw_centered_text text (x + width/2) (height/2 + y) font fg_color

let box_width = 0.2
let box_width = int_of_float (box_width *. (float_of_int Moonshot.screen_width))
let level_textbox = draw_text_box Moonshot.font_size ((Moonshot.screen_width-box_width) / 2) 0
                      box_width Color.raywhite Color.darkgray

let draw_playing model =
  let { Moonshot.Model.bullets=movables; static; fading;
        player={feet=pfeet; head=phead; input=inp; health}; enemies; cam; runtime; _} = model in
  let player = model.player in
  begin_drawing ();
  begin_mode_2d cam;
  clear_background Color.raywhite;
  List.iter draw_planet static;
  List.iter (fun x -> let open Moonshot.Body in draw_body Color.black x.moving.body) movables;
  List.iter draw_explosion fading;
  List.iter (draw_body Color.lime) @@
    List.map (fun x -> let open Moonshot.Body in x.body) [phead; pfeet];
  List.iter draw_enemy enemies;
  (match inp with
   | Moonshot.Player.Aiming (ax, ay) ->
      let bodies = List.map (fun x -> let open Body in ignore (x.is_painful); x.body) static in
      draw_aim_assist bodies 5 0.15 ax ay player;
      let (px, py) = vector phead.body.pos in
      let (ax, ay) = Update.truncate_aim ax ay px py in
      let (ax, ay) = sofwv (Vector2.create ax ay) in
      let (px, py) = sofwv phead.body.pos in
      draw_dotted_line Color.gray 10.0 ax ay px py;
   | _ -> ());
  end_mode_2d ();

  (* Draw HUD *)
  let heart_space = 30 in
  let heart_radius = 10.0 in
  let heart_offset = 15 in
  let draw_hearts i =
    let xs = List.init i (fun i -> heart_offset + heart_space * i) in
    List.iter (fun x -> draw_circle x heart_offset heart_radius Color.red) xs in
  let draw_half_heart x =
    draw_circle x heart_offset (heart_radius/.2.0) Color.red in
  draw_hearts (health / 2);
  if health mod 2 == 1 then
    draw_half_heart (heart_offset + heart_space * (health / 2));

  let trunc_time = (float_of_int (int_of_float (runtime *. 100.0))) /. 100.0 in
  draw_text (string_of_float trunc_time) (Moonshot.screen_width - 50) 10 14 Color.gold;

  if runtime < 8.0 then
    level_textbox model.start_text;

  end_drawing ();
  Model.Playing model

let draw_paused model =
  begin_drawing ();
  clear_background Color.raywhite;
  draw_text "Paused\n(R)estart\nRe(s)ume\n(Q)uit" 10 10 14 Color.gray;
  end_drawing ();
  Model.Paused model


let menu_starfield = Starfield.create 3 100
let draw_menuscreen _ =
  let f_color = Color.raywhite in
  let b_color = Color.black in
  let big_size = 10 in
  let little_size = 3 in

  let little_font_size = little_size * Moonshot.font_size in
  let big_font_size = big_size * Moonshot.font_size in
  begin_drawing ();
  clear_background b_color;
  Starfield.draw menu_starfield (10.0 *. get_time ()) (get_time ()) (Moonshot.sofw 1.0) Color.raywhite;
  draw_centered_text "Starshot" (Moonshot.screen_width / 2) (Moonshot.screen_height / 3)
    big_font_size f_color;
  draw_centered_text "Press [0-9,-,=] to select a level"
    (Moonshot.screen_width / 2) (3 * Moonshot.screen_height / 4)
    little_font_size f_color;
  draw_centered_text "Press [space] to view stats"
    (Moonshot.screen_width / 2) (3 * Moonshot.screen_height / 4 + little_font_size)
    little_font_size f_color;
  end_drawing ();
  Model.MenuScreen

let draw_levelend model =
  let open Moonshot.Model in
  begin_drawing ();
  clear_background Color.raywhite;
  let msg = match model.reason with
    | Victory -> "Victory!!"
    | DriftedAway -> "Lost in space :("
    | Died -> "You Died :(" in
  let has_time_star = model.star_reqs.time >= model.runtime in
  let has_shot_star = model.star_reqs.shots >= model.shots_taken in
  let has_health_star = model.star_reqs.health <= model.health in
  let make_star h b = h ^ ": " ^ (if b then "*" else "-") in
  let time_star = make_star "Time" has_time_star in
  let shot_star = make_star "Shots" has_shot_star in
  let health_star = make_star "Health" has_health_star in
  let msg = Printf.sprintf "Level %s\n%s\nYou took %d shots\nIt took you %.2f seconds\nYou took %.1f damage\nYour longest shot stayed in orbit for %.2f seconds\n***Stars***\n%s\n%s\n%s\n\nPress space to continue"
              model.name msg model.shots_taken model.runtime
              (float_of_int (6-model.health) /. 2.0) model.longest_bullet
              health_star time_star shot_star in
  draw_text msg 10 10 14 Color.gray;
  end_drawing ();
  Model.LevelEnd model

let draw model =
  match model with
  | Model.Paused p -> draw_paused p
  | Model.Playing p -> draw_playing p
  | Model.MenuScreen -> draw_menuscreen ()
  | Model.LevelEnd p -> draw_levelend p
