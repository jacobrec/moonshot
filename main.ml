open Raylib
open Moonshot


let music_game = load_music_stream "sound/background.mp3"
let music_menu = load_music_stream "sound/menu.mp3"
let music_credits = load_music_stream "sound/credits.mp3"
let music = ref music_menu

let setup () =
  init_window screen_width screen_height "Starshot";
  init_audio_device ();
  set_target_fps 60;
  play_music_stream music_game;
  play_music_stream music_credits;
  play_music_stream music_menu;
  Model.MenuScreen

let cleanup () =
  close_audio_device ();
  close_window ()

let manage_audio model =
  match model with
  | Model.Paused _ -> update_music_stream music_game
  | Model.Playing _ -> update_music_stream music_game
  | Model.MenuScreen -> update_music_stream music_menu
  | Model.StatsScreen _ -> update_music_stream music_menu
  | Model.LevelEnd _ -> update_music_stream music_menu
  | Model.WorldSelect -> update_music_stream music_menu
  | Model.LevelSelect _ -> update_music_stream music_menu

let rec loop model =
  manage_audio model;
  if window_should_close () then cleanup () else
    model
    |> Input.input
    |> Update.update (get_frame_time ())
    |> Draw.draw
    |> loop


let () = setup () |> loop
