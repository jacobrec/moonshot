open Raylib
open Moonshot

let input_mouse_or_touch _ =
  if true then
    ((is_mouse_button_down MouseButton.Left), get_mouse_position ())
      (* TODO: detect if mobile, and probably will need a better way for this*)
  else ((0 < Raylib.get_touch_points_count ()), get_touch_position 0)

let input model =
  let open Moonshot.Model in
  let ccw = is_key_down Key.A in
  let cw = is_key_down Key.S in
  let jump = is_key_down Key.Space in
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

  {model with player={model.player with input=inp}}
