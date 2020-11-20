open Raylib

let debug_draw = true
let true_draw = true

let damage_cooldown = 0.31
let explosion_time = 0.3
let explosion_mass = -100.0
let explosion_radius = 3.0

let aim_assist_dots = 50

let ssize = 75
let screen_width = 16 * ssize
let screen_height = 9 * ssize
let font_size = ssize * 12 / 65

let pixels_per_meter = float_of_int ssize /. 10.0
let meters_per_pixel = 1.0 /. pixels_per_meter

module Powerup = struct
  type kind =
    | Fireblast
  type t = {
      power : kind;
      pos : Vector2.t;
    }

  let draw_size = function
    | Fireblast -> 1.0

end

module Body = struct
  type surface_type =
    | Normal
    | Painful
    | Sticky
    | Bouncy
    | Slippery

  type bulletkind =
    | Fireblast
    | Normalblast

  type t = {
      pos: Vector2.t; (* in meters *)
      mass: float; (* in kg *)
      radius: float; (* in meters *)
    }
  type fading = {
      remaining: float; (* in seconds *)
      body: t;
      explosion_kind : bulletkind;
    }
  type planet = {
      body: t;
      surface: surface_type;
    }
  type moving = {
      vel: Vector2.t; (* in m/s *)
      body: t;
    }
  type bullet = {
      kind : bulletkind;
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

  type animation_action =
    | Standing
    | Falling
    | Walking
    | WalkingReverse

  type t = {
      head : Body.moving;
      feet : Body.moving;
      input: input_type;
      health : int;
      last_damaged_at : float;
      animation_state : animation_action;
    }
end

module Enemy = struct
  type action =
    | Standing
    | Shielded
    | Dead of float

  type t = {
      loc : Body.moving;
      action : action;
      angle : float;
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


  type endreason =
    | Victory
    | Died
    | DriftedAway

  type endstats = {
      id : int;
      health  : int;
      runtime : float;
      shots_taken : int;
      longest_bullet : float;
      reason  : endreason;
      name : string;
      star_reqs : star_requirements;
    }
  type playing = {
      start_text : string;
      name : string;
      id : int;
      stars : Starfield.t;
      static : Body.planet list;
      bullets : Body.bullet list;
      powerups : Powerup.t list;
      special_shots : Powerup.kind list;
      fading : Body.fading list;
      player : Player.t;
      enemies : Enemy.t list;
      cam : Camera2D.t;
      runtime : float;
      shots_taken : int;
      longest_bullet : float;
      star_reqs : star_requirements;
    }

  type t =
    | Playing of playing
    | Paused of playing
    | LevelEnd of endstats
    | WorldSelect
    | LevelSelect of int
    | MenuScreen
    | StatsScreen of (int * int Option.t) Option.t
end


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
let sofwv_v v = let (x, y) = screen_of_world_vector v in
              Vector2.create (float_of_int x) (float_of_int y)

