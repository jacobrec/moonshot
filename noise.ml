let cerp a b x =
  let f = (1.0 -. Float.cos (x *. Float.pi)) *. 0.5 in
  a *. (1.0 -. f) +. b *. f

let raw_noise x =
  let n = (x lsl 13) lxor x in
  let ipart = ((n * (n * n * 15731 + 789211) + 1376312589) land 0x7fffffff) in
  1.0 -. (float_of_int ipart) /. 1073741824.0

let smooth x =
  let xi = int_of_float x in
  let a = raw_noise @@ xi in
  let b = raw_noise @@ xi + 1 in
  cerp a b (x -. (float_of_int xi))

let powint base exponent =
  Float.pow base (float_of_int exponent)

let get ?(freq=1.0) ?(persistance=0.65) ?(amplitude=1.0) ?(octaves=4) x =
  List.init octaves (fun i ->
      ((powint persistance i) *. amplitude) *. smooth (x *. ((powint 2.0 i) *. freq)))
  |> List.fold_left (+.) 0.0

