let fmodi f i =
  let ff, fi = Float.modf f in
  let fi = int_of_float fi in
  let f = ff +. float_of_int (fi mod i) in
  if f < 0.0 then f +. float_of_int i else f

(** Expects values to be from [0,255]
* returns values in range [0-360), [0-100), [0-100) *)
let hsv_of_rgb (r, g, b) =
  let r' = (float_of_int r) /. 255.0 in
  let g' = (float_of_int g) /. 255.0 in
  let b' = (float_of_int b) /. 255.0 in
  let cmax = Float.max (Float.max r' g') b' in
  let cmin = Float.min (Float.min r' g') b' in
  let delta = cmax -. cmin in

  let h = int_of_float @@ 60.0 *. (
            match true with
            | _ when delta = 0.0 -> 0.0
            | _ when cmax = r' -> fmodi ((g' -. b') /. delta) 6
            | _ when cmax = g' -> (b' -. r') /. delta +. 2.0
            | _ when cmax = b' -> (r' -. g') /. delta +. 4.0
            | _ -> 0.0) in

  let s = int_of_float (100.0 *. (if cmax = 0.0 then 0.0 else delta /. cmax)) in
  let v = int_of_float (100.0 *. cmax) in
  h, s, v

(** Expects values to be in range [0-360), [0-100), [0-100)
 * returns values from [0,255] *)
let rgb_of_hsv (h, s, v) =
  let h = (float_of_int h) in
  let s = (float_of_int s) /. 100.0 in
  let v = (float_of_int v) /. 100.0 in
  let c = v *. s in
  let x = c *. (1.0 -. Float.abs ((fmodi (h /. 60.0) 2) -. 1.0)) in
  let m = v -. c in
  let z = 0.0 in
  let r', g', b' = match 0 with
    | _ when h <=  60.0 -> (c, x, z)
    | _ when h <= 120.0 -> (x, c, z)
    | _ when h <= 180.0 -> (z, c, x)
    | _ when h <= 240.0 -> (z, x, c)
    | _ when h <= 300.0 -> (x, z, c)
    | _ when h <= 360.0 -> (c, z, x)
    | _ -> (z, z, z) in
  let r = int_of_float ((r' +. m) *. 255.0) in
  let g = int_of_float ((g' +. m) *. 255.0) in
  let b = int_of_float ((b' +. m) *. 255.0) in
  r, g, b


let hsva_of_rgba (r, g, b, a) =
  let h, s, v = hsv_of_rgb r, g, b in
  h, s, v, a

let rgba_of_hsva (h, s, v, a) =
  let r, g, b = rgb_of_hsv h, s, v in
  r, g, b, a

let () =
  let r, g, b = (235, 100, 183) in
  let h, s, v = hsv_of_rgb (r, g, b) in
  let r', g', b' = rgb_of_hsv (h, s, v) in
  Printf.printf "rgb[%d, %d, %d] -> hsv[%d, %d, %d] -> rgb[%d, %d, %d]\n%!" r g b h s v r' g' b'
