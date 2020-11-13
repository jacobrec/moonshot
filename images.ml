type images =
  | PlayerStanding
  | PlayerWalking1
  | PlayerWalking2
  | PlayerWalking3
  | PlayerWalking4
  | PlayerWalkingReverse1
  | PlayerWalkingReverse2
  | PlayerWalkingReverse3
  | PlayerWalkingReverse4
  | PlayerFalling1
  | PlayerFalling2
  | EnemyStanding1
  | EnemyStanding2
  | EnemyStanding3
  | EnemyStanding4
  | EnemyDead

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
  | PlayerWalkingReverse1 -> "img/player/walking_rev/1.png"
  | PlayerWalkingReverse2 -> "img/player/walking_rev/2.png"
  | PlayerWalkingReverse3 -> "img/player/walking_rev/3.png"
  | PlayerWalkingReverse4 -> "img/player/walking_rev/0.png"
  | EnemyStanding1 -> "img/enemy/standing/1.png"
  | EnemyStanding2 -> "img/enemy/standing/2.png"
  | EnemyStanding3 -> "img/enemy/standing/3.png"
  | EnemyStanding4 -> "img/enemy/standing/0.png"
  | EnemyDead -> "img/enemy/dead.png"

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
  | PlayerWalkingReverse
  | PlayerFalling

let get_animation_frame frame_time frames =
  let t = Raylib.get_time () in
  let frame = int_of_float (t /. frame_time) in
  let frame = frame mod List.length frames in
  get (List.nth frames frame)

let get_animation ani =
  match ani with
  | PlayerWalking ->
     get_animation_frame 0.1 [
         PlayerWalking1;
         PlayerWalking2;
         PlayerWalking3;
         PlayerWalking4;
       ]
  | PlayerWalkingReverse ->
     get_animation_frame 0.1 [
         PlayerWalkingReverse1;
         PlayerWalkingReverse2;
         PlayerWalkingReverse3;
         PlayerWalkingReverse4;
       ]
  | PlayerFalling ->
     get_animation_frame 0.3 [
         PlayerFalling1;
         PlayerFalling2;
       ]
  | EnemyStanding ->
     get_animation_frame 0.1 [
         EnemyStanding1;
         EnemyStanding2;
         EnemyStanding3;
         EnemyStanding4;
       ]
