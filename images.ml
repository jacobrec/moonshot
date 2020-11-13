type images =
  | PlayerStanding
  | PlayerWalking1
  | PlayerWalking2
  | PlayerWalking3
  | PlayerWalking4
  | PlayerFalling1
  | PlayerFalling2
  | EnemyStanding1
  | EnemyStanding2
  | EnemyStanding3
  | EnemyStanding4

let texture_map = Hashtbl.create 10

let get_path_from_image img =
  match img with
  | PlayerStanding -> "img/player/standing.png"
  | PlayerFalling1 -> "img/player/falling/1.png"
  | PlayerFalling2 -> "img/player/falling/0.png"
  | PlayerWalking1 -> "img/player/walking/1.png"
  | PlayerWalking2 -> "img/player/walking/2.png"
  | PlayerWalking3 -> "img/player/walking/3.png"
  | PlayerWalking4 -> "img/player/walking/0.png"
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
  | PlayerWalking
  | PlayerFalling

let get_animation_frame frame_time frames =
  let t = Raylib.get_time () in
  let frame = int_of_float (t /. frame_time) in
  let frame = frame mod List.length frames in
  get (List.nth frames frame)

let get_animation ani =
  match ani with
  | PlayerWalking
  | PlayerFalling
  | EnemyStanding ->
     get_animation_frame 0.1 [
         EnemyStanding1;
         EnemyStanding2;
         EnemyStanding3;
         EnemyStanding4;
       ]
