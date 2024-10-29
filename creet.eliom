[%%client
open Js_of_ocaml
open Eliom_content.Html.D
open Eliom_content.Html

type creet_condition = Healthy | Sick | Berserk | Mean

type creet_status = {
  mutable condition: creet_condition;
  mutable max_size: float;
}

let base_creet_size: float = 50.

let _max_size_for_condition = function
  | Healthy -> 1.
  | Sick -> 1.
  | Berserk -> 4.0
  | Mean -> 0.85

type coordinates = {
  mutable x : float;
  mutable x_min : int;
  mutable x_max : int;
  mutable x_step : float;
  mutable y : float;
  mutable y_min : int;
  mutable y_max : int;
  mutable y_step : float;
}

type creet = {
  dom_elt : Dom_html.divElement Js.t;
  status: creet_status;
  coordinates: coordinates;
  max_counter: int;
  mutable counter: int;
  mutable speed: float;
  mutable size: float
}

let get_bg_color condition =
  Js.string (match condition with
    | Healthy -> "dodgerblue"
    | Sick -> "darkblue"
    | Berserk -> "darkcyan"
    | Mean -> "tomato"
  )

let _get_px number = Js.string (Printf.sprintf "%dpx" number)

let _get_step position step speed = position +. (step *. speed)

let _get_random_steps () =
  let step = max 0.25 (Random.float 0.75) in
  let top_step = step in
  let left_step = 1. -. step in
  ( (if Random.bool () = true then top_step else Float.neg top_step),
    if Random.bool () = true then left_step else Float.neg left_step )

let _move creet =
  creet.coordinates.x <- _get_step creet.coordinates.x  creet.coordinates.x_step creet.speed;
  creet.coordinates.y <- _get_step creet.coordinates.y  creet.coordinates.y_step creet.speed;

  creet.dom_elt##.style##.left := _get_px (int_of_float creet.coordinates.x);
  creet.dom_elt##.style##.top := _get_px (int_of_float creet.coordinates.y)

let _change_size creet =
  let target_size = creet.status.max_size *. base_creet_size in
  if creet.size <> target_size then
    let adjustment = (target_size -. creet.size) /. abs_float (target_size -. creet.size) in
    creet.size <- creet.size +. adjustment;
  creet.dom_elt##.style##.height := _get_px (int_of_float creet.size);
  creet.dom_elt##.style##.width := _get_px (int_of_float creet.size)

let _change_direction creet =
  if creet.status.condition = Mean then
    creet.counter <- 0
  else if creet.counter = creet.max_counter then (
    creet.counter <- 0;
    let x_step, y_step = _get_random_steps () in
    creet.coordinates.x_step <- x_step;
    creet.coordinates.y_step <- y_step )
  else creet.counter <- creet.counter + 1

let change_condition creet =
  let n = Random.int 100 in
  let new_condition =
    if n < 10 then  Berserk
    else if n >= 10 && n < 20 then Mean
    else Sick
  in

  creet.status.condition <- new_condition;
  creet.status.max_size <- _max_size_for_condition new_condition;
  creet.speed <- creet.speed *. 0.85;

  creet.dom_elt##.style##.backgroundColor := get_bg_color creet.status.condition;
  Firebug.console##log (Js.string ("Setting background color to: " ^
  (Js.to_string (get_bg_color creet.status.condition))));;


let create () =
  let x = (max 10 (Random.int 590)) in
  let y = (max 50 (Random.int 650 - 50)) in
  let elt = div ~a:[
      a_class [ "creet" ];
      a_style ("position: absolute; left: " ^ string_of_int x ^ "px; top: " ^ string_of_int y ^ "px;")
  ] [] in
  let x_step, y_step = _get_random_steps () in
  let creet = {
    dom_elt = To_dom.of_div elt;
    speed = 1.;
    size = 50.;
    coordinates = {
      x = (float_of_int x);
      x_min = 0;
      x_max = 1000;
      x_step;
      y = (float_of_int y);
      y_min = -15;
      y_max = 651;
      y_step;
    };
    status = {condition = Healthy ; max_size = _max_size_for_condition Healthy};
    counter = 0;
    max_counter = 2500 + Random.int 1000;
  } in
  creet.dom_elt##.style##.backgroundColor := get_bg_color creet.status.condition;
  creet


let move creet =
  (* Firebug.console##log (Js.string (Printf.sprintf "y: %d, y_min: %d" creet.coordinates.y creet.coordinates.y_min)); *)
  if creet.coordinates.x <= (float_of_int creet.coordinates.x_min)
    || creet.coordinates.x >= (float_of_int creet.coordinates.x_max -. creet.size) then (
    creet.coordinates.x_step <- Float.neg creet.coordinates.x_step;
    _move creet
  )
  else if creet.coordinates.y <= (float_of_int creet.coordinates.y_min)
    ||
    creet.coordinates.y >= (float_of_int creet.coordinates.y_max -. creet.size) then (
    if creet.coordinates.y <= 0. && creet.status.condition = Healthy then change_condition creet;
    creet.coordinates.y_step <- Float.neg creet.coordinates.y_step;
    _move creet;
  );

  (match creet.status.condition with
  | Berserk | Mean -> _change_size creet
  | _ -> () );

  _change_direction creet;
  _move creet
]
