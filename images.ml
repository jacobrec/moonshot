type animation =
  | EnemyStanding
  | PlayerWalking
  | PlayerWalkingReverse
  | PlayerFalling
  | BulletFlying
  | BulletFire
  | StarShine
  | Explosion
  | PowerupFire

type images =
  | PlayerStanding
  | EnemyDead
  | EnemyIce
  | HeartFull
  | HeartHalf
  | HeartNone

type t =
  | Animation of (animation * int)
  | Image of images

let texture_map = Hashtbl.create 10

let get_path_from_image img =
  match img with
  | PlayerStanding -> "img/player/standing.png"
  | EnemyDead -> "img/enemy/dead.png"
  | EnemyIce  -> "img/enemy/ice.png"
  | HeartFull -> "img/heart/full.png"
  | HeartHalf -> "img/heart/half.png"
  | HeartNone -> "img/heart/empty.png"

(* Important animation constants *)
let get_animation_frame time ani =
  let f_time, frames =
    match ani with
    | PlayerWalking | PlayerWalkingReverse -> 0.1, 4
    | StarShine -> 0.05, 16
    | EnemyStanding -> 0.1, 4
    | BulletFlying -> 0.08, 8
    | BulletFire -> 0.08, 9
    | Explosion -> 0.06, 7
    | PowerupFire -> 0.1, 6
    | PlayerFalling -> 0.2, 2 in
  let t = if time < 0.0 then Raylib.get_time () else time in
  let frame = int_of_float (t /. f_time) in
  if ani = StarShine then
    let f = frame mod (3 * frames) in
    if f >= frames then 0 else f
  else frame mod frames

let get_path_from_animation img frame =
  let f = string_of_int frame in
  let pref = match img with
  | EnemyStanding -> "img/enemy/standing/"
  | BulletFlying -> "img/bullet/flying/"
  | BulletFire -> "img/bullet/fire/"
  | Explosion -> "img/bullet/explosion/"
  | StarShine -> "img/star/"
  | PowerupFire -> "img/powerups/fire/"
  | PlayerFalling -> "img/player/falling/"
  | PlayerWalking -> "img/player/walking/"
  | PlayerWalkingReverse -> "img/player/walking_rev/" in
  pref ^ f ^ ".png"

let load_image img =
  let img_path = match img with
  | Animation (a, i) -> get_path_from_animation a i
  | Image i -> get_path_from_image i in
  let t = Raylib.load_texture img_path in
  Hashtbl.add texture_map img t;
  t


let get image =
  let i = (Image image) in
  match Hashtbl.find_opt texture_map i with
  | None -> load_image i
  | Some t -> t

let get_animation ?(time= -1.0) ani =
  let f = get_animation_frame time ani in
  let i = (Animation (ani, f)) in
  match Hashtbl.find_opt texture_map i with
  | None -> load_image i
  | Some t -> t


