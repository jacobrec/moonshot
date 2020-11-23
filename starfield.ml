(* Parallex star field *)

(* x, y, z basically *)
type t = (float * float * float) list

let width = 4000

let height = 4000

let create layer_count total_count : t =
  let flayers = float_of_int layer_count in
  let fcount = float_of_int total_count in
  let groups = Float.pow 2.0 flayers -. 1.0 in
  let stars_in_group = fcount /. groups in
  let layers =
    List.init layer_count (fun i ->
        stars_in_group *. Float.pow 2.0 (float_of_int i))
  in
  let stars =
    List.mapi
      (fun i x ->
        List.init (int_of_float x) (fun _ ->
            let z = float_of_int (layer_count - i) in
            ( Random.float (float_of_int width)
            , Random.float (float_of_int height)
            , z )))
      layers
  in
  List.flatten stars

let nmod a b = ((a mod b) + b) mod b

let draw field vx vy pixel_size color =
  List.iter
    (fun (x, y, z) ->
      let tx = x +. (vx *. z) in
      let ty = y +. (vy *. z) in
      let ix =
        nmod (pixel_size * int_of_float tx) width - (5 * pixel_size)
      in
      let iy =
        nmod (pixel_size * int_of_float ty) height - (5 * pixel_size)
      in
      let iz = pixel_size * int_of_float z in
      Raylib.draw_rectangle ix iy iz iz color)
    field
