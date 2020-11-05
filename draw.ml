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

let draw_playing model =
  let { Moonshot.Model.bullets=movables; static=bodies; fading=fading;
        player={feet=pfeet; head=phead; input=inp}; enemies; cam } = model in
  let player = model.player in
  begin_mode_2d cam;
  clear_background Color.raywhite;
  List.iter (draw_body Color.beige) bodies;
  List.iter (fun x -> let open Moonshot.Body in draw_body Color.black x.body) movables;
  List.iter draw_explosion fading;
  List.iter (draw_body Color.lime) @@
    List.map (fun x -> let open Moonshot.Body in x.body) [phead; pfeet];
  List.iter draw_enemy enemies;
  (match inp with
   | Moonshot.Player.Aiming (ax, ay) ->
      draw_aim_assist bodies 5 0.15 ax ay player;
      let (px, py) = vector phead.body.pos in
      let (ax, ay) = Update.truncate_aim ax ay px py in
      let (ax, ay) = sofwv (Vector2.create ax ay) in
      let (px, py) = sofwv phead.body.pos in
      draw_dotted_line Color.gray 10.0 ax ay px py;
   | _ -> ());
  end_drawing ();
  Model.Playing model

let draw_paused model =
  begin_drawing ();
  clear_background Color.raywhite;
  draw_text "Paused" 10 10 14 Color.gray;
  end_drawing ();
  Model.Paused model

let draw_menuscreen _ =
  begin_drawing ();
  clear_background Color.raywhite;
  draw_text "Main Menu" 10 10 14 Color.gray;
  draw_text "Press 0-9 to select a level" 10 20 14 Color.gray;
  end_drawing ();
  Model.MenuScreen

let draw model =
  match model with
  | Model.Paused p -> draw_paused p
  | Model.Playing p -> draw_playing p
  | Model.MenuScreen -> draw_menuscreen ()
