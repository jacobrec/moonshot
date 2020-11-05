open Raylib
open Moonshot



let setup () =
  init_window screen_width screen_height "test1";
  set_target_fps 60;
  Model.MenuScreen


let rec loop model =
  if window_should_close () then close_window () else
    model
    |> Input.input
    |> Update.update (get_frame_time ())
    |> Draw.draw
    |> loop


let () = setup () |> loop
