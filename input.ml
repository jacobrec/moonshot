open Raylib
open Moonshot

let input_mouse_or_touch _ =
  if true then
    ((is_mouse_button_down MouseButton.Left), get_mouse_position ())
      (* TODO: detect if mobile, and probably will need a better way for this*)
  else ((0 < Raylib.get_touch_points_count ()), get_touch_position 0)

let input_playing model =
  let open Moonshot.Model in
  let ccw = is_key_down Key.A in
  let cw = is_key_down Key.S in
  let jump = is_key_down Key.Space in
  let paused = is_key_pressed Key.P in
  let (touched, tv) = input_mouse_or_touch () in
  let tv' = get_screen_to_world_2d tv model.cam in
  let (tx, ty) = wofsv tv' in
  let (px, py) = vector model.player.head.body.pos in
  let touch_catch_size = 10.0 in
  let open Moonshot.Player in
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
    else None in

  let new_t = {model with player={model.player with input=inp}} in
  if paused then Model.Paused new_t
  else Model.Playing new_t

let input_paused model =
  let unpaused = is_key_pressed Key.Space ||
                 is_key_pressed Key.P ||
                 is_key_pressed Key.A ||
                 is_key_pressed Key.S in
  if unpaused then Model.Playing model
  else Model.Paused model

let input_menu _ =
  (* TODO: Graphical selector with buttons *)
       if is_key_pressed Key.Zero  then Model.Playing (Level.load 0)
  else if is_key_pressed Key.One   then Model.Playing (Level.load 1)
  else if is_key_pressed Key.Two   then Model.Playing (Level.load 2)
  else if is_key_pressed Key.Three then Model.Playing (Level.load 3)
  else if is_key_pressed Key.Four  then Model.Playing (Level.load 4)
  else if is_key_pressed Key.Five  then Model.Playing (Level.load 5)
  else if is_key_pressed Key.Six   then Model.Playing (Level.load 6)
  else if is_key_pressed Key.Seven then Model.Playing (Level.load 7)
  else if is_key_pressed Key.Eight then Model.Playing (Level.load 8)
  else if is_key_pressed Key.Nine  then Model.Playing (Level.load 9)
  else Model.MenuScreen


let input model =
  match model with
  | Model.Paused p -> input_paused p
  | Model.Playing p -> input_playing p
  | Model.MenuScreen -> input_menu ()
