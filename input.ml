open Raylib
open Moonshot


let input_mouse_or_touch _ =
  if true then
    ((is_mouse_button_down MouseButton.Left), get_mouse_position ())
      (* TODO: detect if mobile, and probably will need a better way for this*)
  else ((0 < Raylib.get_touch_points_count ()), get_touch_position 0)

let input_playing model =
  let open Moonshot.Model in
  let ccw = is_key_down Key.A || is_key_down Key.W in
  let cw = is_key_down Key.D || is_key_down Key.S in
  let jump = is_key_down Key.Space in
  let paused = is_key_pressed Key.P in
  let (touched, tv) = input_mouse_or_touch () in
  let tv' = get_screen_to_world_2d tv model.cam in
  let (tx, ty) = wofsv tv' in
  let (px, py) = vector model.player.head.body.pos in
  let touch_catch_size = 10.0 in
  let open Moonshot.Player in
  let touch_x = int_of_float @@ Vector2.x tv in
  let inp =
    if (match model.player.input with | Aiming _ -> true | _ -> false) &&
         not touched then
      match model.player.input with
      | Moonshot.Player.Aiming (x, y) -> Fire (x, y)
      | _ -> raise Not_found (*this is checked in the condition*)
    else if (match model.player.input with | Aiming _ -> false | _ -> true)
            && touched && (((Float.pow (tx -. px) 2.0) +.
                              (Float.pow (ty -. py) 2.0)) <
                             touch_catch_size *. touch_catch_size) then
      Aiming (tx, ty)
    else if (match model.player.input with | Aiming _ -> true | _ -> false)
            && touched then
      Aiming (tx, ty)
    else if jump then
      Jump
    else if not ccw && cw then
      CW
    else if ccw && not cw then
      CCW
    else if touched && touch_x < (2 * ssize) then
      CCW
    else if touched && touch_x > screen_width - (2 * ssize) then
      CW
    else None in

  let new_t = {model with player={model.player with input=inp}} in
  if paused then Model.Paused new_t
  else Model.Playing new_t

let input_paused model =
  let unpaused = is_key_pressed Key.Space ||
                 is_key_pressed Key.P ||
                 is_key_pressed Key.A ||
                 is_key_pressed Key.S in
  let quit = is_key_pressed Key.Q in
  let restart = is_key_pressed Key.R in
  let open Model in
  if quit then Model.LevelSelect ((model.id / 100 + 1) * 100)
  else if restart then Playing (Level.load model.id)
  else if unpaused then Playing model
  else Paused model

let in_bounds x y w h v =
  let px, py = vector v in
  let px, py = int_of_float px, int_of_float py in
  px > x && px < x + w && py > y && py < y + h

let touch_area x y w h =
  if Moonshot.touch_draw then begin
      begin_drawing ();
      draw_rectangle_lines x y w h Color.pink;
      end_drawing ()
    end;
  let (touched, tv) = input_mouse_or_touch () in
  touched && in_bounds x y w h tv


let input_menu _ =
  let sf x = int_of_float ((float_of_int ssize) *. x) in
  (* TODO: Graphical selector with buttons *)
  match true with
  | _ when is_key_pressed Key.Space -> Model.WorldSelect
  | _ when is_key_pressed Key.S -> Model.StatsScreen None
  | _ when touch_area (sf 5.0) (sf 6.5) (sf 6.0) (sf 0.5) -> Model.WorldSelect
  | _ when touch_area (sf 5.0) (sf 7.0) (sf 6.0) (sf 0.5) -> Model.StatsScreen None
  | _ -> Model.MenuScreen

let input_stats p =
  let keymap = [
      (Key.One,   1);
      (Key.Two,   2);
      (Key.Three, 3);
      (Key.Four,  4);
      (Key.Five,  5);
      (Key.Six,   6);
      (Key.Seven, 7);
      (Key.Eight, 8);
      (Key.Nine,  9);
      (Key.Zero,  10); (* Level 10 *)
      (Key.Minus, 11);(* Level 11 *)
      (Key.Equal, 12);(* Level 12 *)
    ] in
  match List.find_opt (fun (a, _) -> is_key_pressed a) keymap with
  | None when is_key_pressed Key.Space ->
     (match p with
      | None -> Model.MenuScreen
      | Some (_, None) -> Model.StatsScreen None
      | Some (w, Some _) -> Model.StatsScreen (Some (w, None)))
  | None -> Model.StatsScreen p
  | Some (_, b) ->
     (match p with
      | None when b <= (List.length Level.world_names) -> Model.StatsScreen (Some (b * 100, None))
      | None -> Model.StatsScreen p
      | Some (w, None) -> Model.StatsScreen (Some (w, Some b))
      | Some (w, Some l) -> Model.StatsScreen (Some (w, Some l)))

let input_levelend p =
  let open Model in
  ignore p.reason;
  if is_key_pressed Key.Space then Model.LevelSelect ((p.id / 100 + 1) * 100)
  else if is_key_pressed Key.R then Model.Playing (Level.load p.id)
  else Model.LevelEnd p

let input_level p =
  let keymap = [
      (Key.One,   1);
      (Key.Two,   2);
      (Key.Three, 3);
      (Key.Four,  4);
      (Key.Five,  5);
      (Key.Six,   6);
      (Key.Seven, 7);
      (Key.Eight, 8);
      (Key.Nine,  9);
      (Key.Zero,  10); (* Level 10 *)
      (Key.Minus, 11);(* Level 11 *)
      (Key.Equal, 12);(* Level 12 *)
    ] in
  let sf x = int_of_float ((float_of_int ssize) *. x) in
  let touchmap = List.init 12 (fun i ->
                     let x = if i mod 2 = 0 then (sf 5.0) else (sf 9.0) in
                     let y = sf (4.5 +. 0.35 *. float_of_int (i / 2)) in
                     ((x, y, (sf 3.0), (sf 0.35)), i + 1)
                   ) in

  let with_touchmap _=
    List.find_map (fun ((x, y, w, h), value) ->
        if touch_area x y w h then Some value else None
      ) touchmap in

  match List.find_opt (fun (a, _) -> is_key_pressed a) keymap with
  | None when is_key_pressed Key.Space -> Model.WorldSelect
  | None -> (match with_touchmap () with
                          | Some b -> Model.Playing (Level.load ((p-100) + b))
                          | None -> Model.LevelSelect p)
  | Some (_, b) -> Model.Playing (Level.load ((p-100) + b))

let input_world _ =
  let keymap = [
      (Key.One,   1);
      (Key.Two,   2);
      (Key.Three, 3);
      (Key.Four,  4);
      (Key.Five,  5);
      (Key.Six,   6);
      (Key.Seven, 7);
      (Key.Eight, 8);
      (Key.Nine,  9);
      (Key.Zero,  10); (* Level 10 *)
      (Key.Minus, 11);(* Level 11 *)
      (Key.Equal, 12);(* Level 12 *)
    ] in
  let sf x = int_of_float ((float_of_int ssize) *. x) in
  let touchmap = List.init (List.length Level.world_names) (fun i ->
                     let fi = float_of_int i in
                     (((sf 5.0), (sf (4.5 +. (fi *. 0.35))), (sf 6.0), (sf 0.35)), i + 1)
                   ) in

  let with_touchmap _=
    List.find_map (fun ((x, y, w, h), value) ->
        if touch_area x y w h then Some value else None
      ) touchmap in

  match List.find_opt (fun (a, _) -> is_key_pressed a) keymap with
  | None when is_key_pressed Key.Space -> Model.MenuScreen
  | None -> (match with_touchmap () with
                          | Some b -> Model.LevelSelect (b * 100)
                          | None -> Model.WorldSelect)
  | Some (_, b) when b <= (List.length Level.world_names) -> Model.LevelSelect (b * 100)
  | Some (_, _) -> Model.WorldSelect

let input model =
  match model with
  | Model.Paused p -> input_paused p
  | Model.Playing p -> input_playing p
  | Model.MenuScreen -> input_menu ()
  | Model.StatsScreen p -> input_stats p
  | Model.LevelEnd p -> input_levelend p
  | Model.WorldSelect -> input_world ()
  | Model.LevelSelect world -> input_level world
