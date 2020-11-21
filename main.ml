open Raylib
open Moonshot


let music = load_music_stream "sound/background.mp3"
let setup () =
  init_window screen_width screen_height "Starshot";
  init_audio_device ();
  set_target_fps 60;
  play_music_stream music;
  Model.MenuScreen

let cleanup () =
  close_audio_device ();
  close_window ()

let rec loop model =
  update_music_stream music;
  if window_should_close () then cleanup () else
    model
    |> Input.input
    |> Update.update (get_frame_time ())
    |> Draw.draw
    |> loop


let () = setup () |> loop
