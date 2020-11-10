let offset_total_time = 0
let offset_total_shots = 1
let offset_total_damage = 2
let offset_total_stars = 3
let offset_time = 0
let offset_health = 1
let offset_shots = 2
let offset_time_star = 3
let offset_health_star = 4
let offset_shots_star = 5
let offset_level_completed = 6
let max_properties = 20
let static_properties = 200
let get_level_offset i =
  static_properties + i * max_properties

let get_best_time_offset i =
  (get_level_offset i) + offset_time
let get_best_health_offset i =
  (get_level_offset i) + offset_time
let get_best_shots_offset i =
  (get_level_offset i) + offset_time
let has_star_time_offset i =
  (get_level_offset i) + offset_time_star
let has_star_health_offset i =
  (get_level_offset i) + offset_health_star
let has_star_shots_offset i =
  (get_level_offset i) + offset_shots_star
let has_level_completed_offset i =
  (get_level_offset i) + offset_level_completed


let write_totals a b c d =
  let b = int_of_float (b *. 100.0) in (* time is stored to 2 decimals *)
  let c = int_of_float (c *. 10.0) in (* health is stored to 1 decimal *)
  Raylib.save_storage_value offset_total_shots a;
  Raylib.save_storage_value offset_total_time b;
  Raylib.save_storage_value offset_total_damage c;
  Raylib.save_storage_value offset_total_stars d

let write_level_stats i has_time_star has_health_star has_shot_star
      damage_taken runtime shots_taken =
  let load = Raylib.load_storage_value in
  let store = Raylib.save_storage_value in
  let toff = get_best_time_offset i in
  let hoff = get_best_health_offset i in
  let soff = get_best_shots_offset i in
  let new_time = int_of_float (runtime *. 100.0) in
  let new_health = int_of_float (damage_taken *. 10.0) in
  let new_shots = shots_taken in
  let old_time = load toff in
  let old_health = load hoff in
  let old_shots = load soff in
  let best_time = min old_time new_time in
  let best_health = min old_health new_health in
  let best_shots = min old_shots new_shots in
  store toff best_time;
  store hoff best_health;
  store soff best_shots;
  if has_time_star then store (has_star_time_offset i) 1;
  if has_health_star then store (has_star_health_offset i) 1;
  if has_shot_star then store (has_star_shots_offset i) 1

let level i =
  let a = (1 = Raylib.load_storage_value (has_star_health_offset i)) in
  let b = (1 = Raylib.load_storage_value (has_star_shots_offset i)) in
  let c = (1 = Raylib.load_storage_value (has_star_time_offset i)) in
  (a, b, c)

let totals _ =
  (* shots, time, damage, stars *)
  let a = Raylib.load_storage_value offset_total_shots in
  let b = Raylib.load_storage_value offset_total_time in
  let c = Raylib.load_storage_value offset_total_damage in
  let d = Raylib.load_storage_value offset_total_stars in
  let b = (float_of_int b) /. 100.0 in
  let c = (float_of_int c) /. 10.0 in
  (a, b, c, d)


let save levelend =
  let open Moonshot.Model in
  let (a, b, c, d) = totals () in
  let damage_taken = (float_of_int (6 - levelend.health) /. 2.0) in
  let a = a +  levelend.shots_taken in
  let b = b +. levelend.runtime in
  let c = c +. damage_taken in

  let id = levelend.id in
  let has_time_star = levelend.star_reqs.time >= levelend.runtime in
  let has_shot_star = levelend.star_reqs.shots >= levelend.shots_taken in
  let has_health_star = levelend.star_reqs.health <= levelend.health in

  let load = Raylib.load_storage_value in
  let new_stars =
    (if has_time_star && (0 = load (has_star_time_offset id)) then 1 else 0) +
    (if has_shot_star && (0 = load (has_star_shots_offset id)) then 1 else 0) +
    (if has_health_star && (0 = load (has_star_health_offset id)) then 1 else 0) in
  let d = d + new_stars in

  write_level_stats id has_time_star has_health_star has_shot_star
    damage_taken levelend.runtime levelend.shots_taken;
  write_totals a b c d


