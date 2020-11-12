type images =
  | PlayerStanding
  | EnemyStanding1
  | EnemyStanding2
  | EnemyStanding3
  | EnemyStanding4

let texture_map = Hashtbl.create 10

let get_path_from_image img =
  match img with
  | PlayerStanding -> "img/player/standing.png"
  | EnemyStanding1 -> "img/enemy/standing/1.png"
  | EnemyStanding2 -> "img/enemy/standing/2.png"
  | EnemyStanding3 -> "img/enemy/standing/3.png"
  | EnemyStanding4 -> "img/enemy/standing/0.png"

let load_image img =
  let img_path = get_path_from_image img in
  let t = Raylib.load_texture img_path in
  Hashtbl.add texture_map img t;
  t

let get image =
  match Hashtbl.find_opt texture_map image with
  | None -> load_image image
  | Some t -> t

type animation =
  | EnemyStanding

let get_animation ani =
  let t = Raylib.get_time () in
  match ani with
  | EnemyStanding ->
     let frame = int_of_float (t *. 10.0) in
     let frame = frame mod 4 in
     match frame with
     | 1 -> get EnemyStanding1
     | 2 -> get EnemyStanding2
     | 3 -> get EnemyStanding3
     | _ -> get EnemyStanding4
