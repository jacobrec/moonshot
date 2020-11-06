open Raylib

module Body = struct
  type t = {
      pos: Vector2.t; (* in meters *)
      mass: float; (* in kg *)
      radius: float; (* in meters *)
    }
  type fading = {
      remaining: float; (* in seconds *)
      body: t;
    }
  type planet = {
      body: t;
      is_painful: bool;
    }
  type moving = {
      vel: Vector2.t; (* in m/s *)
      body: t;
    }
  type bullet = {
      created_at : float;
      moving : moving;
    }
end

module Player = struct
  type input_type =
    | CW
    | CCW
    | Jump
    | Aiming of float * float
    | Fire of float * float
    | None

  type t = {
      head : Body.moving;
      feet : Body.moving;
      input: input_type;
      health : int;
    }
end

module Enemy = struct
  type action =
    | Standing
    | Dead of float

  type t = {
      loc : Body.moving;
      action : action;
    }
  let is_alive x =
    match x.action with
    | Dead _ -> false
    | _ -> true
end

module Model = struct
  type star_requirements = {
      time : float;
      shots : int;
      health : int;
    }

  type playing = {
      name : string;
      static : Body.planet list;
      bullets : Body.bullet list;
      fading : Body.fading list;
      player : Player.t;
      enemies : Enemy.t list;
      cam : Camera2D.t;
      runtime : float;
      shots_taken : int;
      longest_bullet : float;
      star_reqs : star_requirements;
    }

  type endreason =
    | Victory
    | Died
    | DriftedAway

  type endstats = {
      health  : int;
      runtime : float;
      shots_taken : int;
      longest_bullet : float;
      reason  : endreason;
      name : string;
      star_reqs : star_requirements;
    }

  type t =
    | Playing of playing
    | Paused of playing
    | LevelEnd of endstats
    | MenuScreen
end

let ssize = 65
let screen_width = 16 * ssize
let screen_height = 9 * ssize

let pixels_per_meter = float_of_int ssize /. 10.0
let meters_per_pixel = 1.0 /. pixels_per_meter

let vector v =
  (Vector2.x v, Vector2.y v)

let screen_of_world w =
  int_of_float (w *. pixels_per_meter)

let world_of_screen p =
  (float_of_int p) *. meters_per_pixel

let screen_of_world_vector v =
  let x = (screen_of_world @@ Vector2.x v) + screen_width / 2 in
  let y = (screen_height - (screen_of_world @@ Vector2.y v) - 1) - screen_height / 2 in
  (x, y)

let world_of_screen_vector v =
  let x = int_of_float @@ Vector2.x v in
  let x' = x - screen_width / 2 in
  let x'' = world_of_screen x' in
  let y = int_of_float @@ Vector2.y v in
  let y' = (screen_height - y - 1) - screen_height / 2 in
  let y'' = world_of_screen y' in
  (x'', y'')

let sofw = screen_of_world
let wofs = world_of_screen
let sofwv = screen_of_world_vector
let wofsv = world_of_screen_vector

let explosion_time = 1.0
let explosion_mass = -100.0
let explosion_radius = 3.0

