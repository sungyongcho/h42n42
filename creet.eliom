[%%client
open Js_of_ocaml
open Js_of_ocaml_lwt
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
  mutable size: float;
  mutable speed : float;
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

let _move creet =
  creet.coordinates.x <- _get_step creet.coordinates.x  creet.coordinates.x_step creet.coordinates.speed;
  creet.coordinates.y <- _get_step creet.coordinates.y  creet.coordinates.y_step creet.coordinates.speed;

  creet.dom_elt##.style##.left := _get_px (int_of_float creet.coordinates.x);
  creet.dom_elt##.style##.top := _get_px (int_of_float creet.coordinates.y)

let _change_size creet =
  let target_size = creet.status.max_size *. base_creet_size in
  if creet.coordinates.size <> target_size then
    let adjustment = (target_size -. creet.coordinates.size) /. abs_float (target_size -. creet.coordinates.size) in
    creet.coordinates.size <- creet.coordinates.size +. adjustment;
  (* fix the code here *)
  creet.dom_elt##.style##.height := _get_px (int_of_float creet.coordinates.size);
  creet.dom_elt##.style##.width := _get_px (int_of_float creet.coordinates.size)

(* let _increase_berserk_size creet =
  if creet.coordinates.size < creet.status.max_size then
    creet.coordinates.size <- creet.coordinates.size +. 1.0;
  creet.dom_elt##.style##.height := _get_px (int_of_float creet.coordinates.size);
  creet.dom_elt##.style##.width := _get_px (int_of_float creet.coordinates.size) *)

let _change_direction creet =
  if creet.counter = creet.max_counter then (
    creet.counter <- 0;
    let step = Random.float 1. in
    creet.coordinates.x_step <- max 0.25 step;
    creet.coordinates.y_step <- max 0.25 (1. -. step))
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
  creet.coordinates.speed <- creet.coordinates.speed *. 0.85;

  creet.dom_elt##.style##.backgroundColor := get_bg_color creet.status.condition;
  Firebug.console##log (Js.string ("Setting background color to: " ^
  (Js.to_string (get_bg_color creet.status.condition))));;


let create ~x ~y () =
  let elt ~x ~y = div ~a:[
      a_class [ "creet" ];
      a_style ("position: absolute; left: " ^ string_of_int x ^ "px; top: " ^ string_of_int y ^ "px;")
  ] [] in
  let step = Random.float 1. in
  let creet = {
    dom_elt = To_dom.of_div (elt ~x ~y);
    coordinates = {
      speed = 1.;
      size = 50.;
      x = (float_of_int x);
      x_min = 0;
      x_max = 1000;
      x_step = max 0.25 step;
      y = (float_of_int y);
      y_min = -15;
      y_max = 651;
      y_step = max 0.25 (1. -. step);
    };
    status = {condition = Healthy ; max_size = _max_size_for_condition Healthy};
    counter = 0;
    max_counter = 2500 + Random.int 1000;
  } in
  creet.dom_elt##.style##.backgroundColor := get_bg_color creet.status.condition;
  creet


let rec move creet =
  let%lwt () = Lwt_js.sleep 0.001 in
  (* Firebug.console##log (Js.string (Printf.sprintf "y: %d, y_min: %d" creet.coordinates.y creet.coordinates.y_min)); *)
  if creet.coordinates.x <= (float_of_int creet.coordinates.x_min)
    || creet.coordinates.x >= (float_of_int creet.coordinates.x_max -. creet.coordinates.size) then (
    creet.coordinates.x_step <- Float.neg creet.coordinates.x_step;
    _move creet
  )
  else if creet.coordinates.y <= (float_of_int creet.coordinates.y_min)
    ||
    creet.coordinates.y >= (float_of_int creet.coordinates.y_max -. creet.coordinates.size) then (
    if creet.coordinates.y <= 0. && creet.status.condition = Healthy then change_condition creet;
    creet.coordinates.y_step <- Float.neg creet.coordinates.y_step;
    _move creet;
  );
  (match creet.status.condition with
  | Berserk | Mean -> _change_size creet
  | _ -> () );

  (* if creet.status.condition = Berserk then _increase_berserk_size creet; *)

  _change_direction creet;

  _move creet;
  move creet
]
