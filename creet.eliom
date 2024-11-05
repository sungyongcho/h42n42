[%%client
open Js_of_ocaml
open Js_of_ocaml_lwt
open Eliom_content.Html.D
open Eliom_content.Html
open Params

type creet_condition = Healthy | Sick | Berserk | Mean

type creet_status = {
  mutable condition: creet_condition;
  mutable max_size: float;
}

let _max_size_for_condition = function
  | Healthy -> 1.
  | Sick -> 1.
  | Berserk -> 4.0
  | Mean -> 0.85

type coordinates = {
  mutable x : float;
  mutable x_min : float;
  mutable x_max : float;
  mutable x_step : float;
  mutable y : float;
  mutable y_min : float;
  mutable y_max : float;
  mutable y_step : float;
}

type creet = {
  elt : Html_types.div elt;
  dom_elt : Dom_html.divElement Js.t;
  status: creet_status;
  coordinates: coordinates;
  max_counter: int;
  mutable counter: int;
  mutable speed: float;
  mutable global_speed: float ref;
  mutable size: float;
  mutable available: bool
}

let get_bg_color condition =
  Js.string (match condition with
    | Healthy -> "dodgerblue"
    | Sick -> "darkblue"
    | Berserk -> "darkcyan"
    | Mean -> "tomato"
  )

let _get_px number = Js.string (Printf.sprintf "%dpx" number)

let _get_step position step speed global_speed = position +. (step *. (speed *. !global_speed) )

let _get_random_steps () =
  let step = max 0.25 (Random.float 0.75) in
  let top_step = step in
  let left_step = 1. -. step in
  ( (if Random.bool () = true then top_step else Float.neg top_step),
    if Random.bool () = true then left_step else Float.neg left_step )

let _move creet =
  creet.coordinates.x <- _get_step creet.coordinates.x  creet.coordinates.x_step creet.speed creet.global_speed;
  creet.coordinates.y <- _get_step creet.coordinates.y  creet.coordinates.y_step creet.speed creet.global_speed;

  creet.dom_elt##.style##.left := _get_px (int_of_float creet.coordinates.x);
  creet.dom_elt##.style##.top := _get_px (int_of_float creet.coordinates.y)

  let _change_size creet =
    let target_size = creet.status.max_size *. creet_base_size in
    if creet.size <> target_size then (
      (* Update the size *)
      creet.size <- creet.size +. 1.;

      (* Update minimum coordinates to center based on the new size *)
      creet.coordinates.x_min <- creet.size /. 2.;
      creet.coordinates.y_min <- creet.size /. 2.;

      (* Center `x` and `y` based on new `x_min` and `y_min` *)
      creet.coordinates.x <- max creet.coordinates.x_min (min (creet.coordinates.x_max -. creet.size) (creet.coordinates.x));
      creet.coordinates.y <- max creet.coordinates.y_min (min (creet.coordinates.y_max -. creet.size) (creet.coordinates.y))
    );

    (* Apply the updated size to height, width, top, and left *)
    creet.dom_elt##.style##.height := _get_px (int_of_float creet.size);
    creet.dom_elt##.style##.width := _get_px (int_of_float creet.size);
    creet.dom_elt##.style##.top := _get_px (int_of_float creet.coordinates.y);
    creet.dom_elt##.style##.left := _get_px (int_of_float creet.coordinates.x)


(* let _change_direction creet =
  if creet.status.condition = Mean then
    creet.counter <- 0
  else if creet.counter >= creet.max_counter then (
    creet.counter <- 0;
    let x_step, y_step = _get_random_steps () in
    creet.coordinates.x_step <- x_step;
    creet.coordinates.y_step <- y_step;
    (* Ensure the new steps do not push the creet out of bounds *)
    if creet.coordinates.x +. creet.size +. (x_step *. creet.speed *. !(creet.global_speed)) >= creet.coordinates.x_max ||
        creet.coordinates.x -. creet.size +. (x_step *. creet.speed *. !(creet.global_speed)) <= creet.coordinates.x_min then
      creet.coordinates.x_step <- Float.neg creet.coordinates.x_step;
    if creet.coordinates.y +. creet.size +. (y_step *. creet.speed *. !(creet.global_speed)) >= creet.coordinates.y_max ||
        creet.coordinates.y -. creet.size  +. (y_step *. creet.speed *. !(creet.global_speed)) <= creet.coordinates.y_min then
      creet.coordinates.y_step <- Float.neg creet.coordinates.y_step;
  )
  else
    creet.counter <- creet.counter + 1 *)

let _change_condition creet =
  (* let n = Random.int 100 in *)
  (* let new_condition =
    if n < 10 then  Berserk
    else if n >= 10 && n < 20 then Mean
    else Sick *)
  let new_condition = Berserk
  in

  creet.status.condition <- new_condition;
  creet.status.max_size <- _max_size_for_condition new_condition;
  creet.speed <- creet.speed *. 0.85;

  creet.dom_elt##.style##.backgroundColor := get_bg_color creet.status.condition;
  Firebug.console##log (Js.string ("Setting background color to: " ^
  (Js.to_string (get_bg_color creet.status.condition))));;

  let _event_handler creet event =
    let container = Dom_html.document##.body in
    let container_rect = container##getBoundingClientRect in
    let container_left = container_rect##.left in
    let container_top = container_rect##.top in

    let radius = creet.size /. 2. in
    let left = float_of_int event##.clientX -. (Js.to_float container_left) in
    let top = float_of_int event##.clientY -. (Js.to_float container_top) -. (float_of_int river_height) in

    (* Adjust X coordinate *)
    if left < creet.coordinates.x_min then
      creet.coordinates.x <- creet.coordinates.x_min
    else if left > creet.coordinates.x_max -. radius then
      creet.coordinates.x <- creet.coordinates.x_max -. radius
    else
      creet.coordinates.x <- left;

    (* Adjust Y coordinate *)
    if top < creet.coordinates.y_min then
      creet.coordinates.y <- creet.coordinates.y_min
    else if top > creet.coordinates.y_max -. radius then
      creet.coordinates.y <- creet.coordinates.y_max -. radius
    else
      creet.coordinates.y <- top;

    (* Update the DOM element's position *)
    creet.dom_elt##.style##.left := _get_px (int_of_float creet.coordinates.x);
    creet.dom_elt##.style##.top := _get_px (int_of_float creet.coordinates.y)


let _handle_events creet mouse_down _ =
  creet.available <- false;
  _event_handler creet mouse_down;
  Lwt.pick
    [
      Lwt_js_events.mousemoves Dom_html.document (fun mouse_move _ ->
          _event_handler creet mouse_move;
          Lwt.return ());
      (let%lwt mouse_up = Lwt_js_events.mouseup Dom_html.document in
        _event_handler creet mouse_up;
        creet.available <- true;
        Lwt.return ());
    ]

let check_healthy_creets creets =
  List.exists (fun creet -> creet.status.condition = Healthy) creets


let create global_speed =
  let x = (max 50 (Random.int 1000 - 50)) in
  let y = (max 85 (Random.int 630 - 50)) in
  (* let x = (max 0 + int_of_float (creet_base_size /. 2.) (Random.int 1000 - (int_of_float (creet_base_size /. 2.)))) in
  let y = (max 35 + (int_of_float (creet_base_size /. 2.)) (Random.int 630 - (int_of_float creet_base_size /. 2.))) in *)
  let elt = div ~a:[
      a_class [ "creet" ];
      a_style ("position: absolute; left: " ^ string_of_int x ^ "px; top: " ^ string_of_int y ^ "px;")
  ] [] in
  let x_step, y_step = _get_random_steps () in
  let creet = {
    elt;
    dom_elt = To_dom.of_div elt;
    speed = 1.;
    global_speed;
    size = creet_base_size;
    available = true;
    coordinates = {
      x = (float_of_int x);
      x_min = creet_base_size /. 2.;
      x_max = (float_of_int gameboard_width);
      x_step;
      y = (float_of_int y);
      y_min = creet_base_size /. 2.;
      y_max = (float_of_int gameboard_height) -. (float_of_int hospital_height);
      y_step;
    };
    status = {condition = Healthy ; max_size = _max_size_for_condition Healthy};
    counter = 0;
    max_counter = 2500 + Random.int 1000;
  } in
  creet.dom_elt##.style##.backgroundColor := get_bg_color creet.status.condition;
  Lwt.async (fun () -> Lwt_js_events.mousedowns creet.dom_elt (_handle_events creet));
  creet


let move creet =
  (* _change_direction creet; *)
  (* Firebug.console##log (Js.string (Printf.sprintf "y: %d, y_min: %d" creet.coordinates.y creet.coordinates.y_min)); *)
  if creet.coordinates.x <= creet.coordinates.x_min
    || creet.coordinates.x >=  creet.coordinates.x_max -. (creet.size /. 2.) then (
    creet.coordinates.x_step <- Float.neg creet.coordinates.x_step;
    (* _move creet *)
  );
  if creet.coordinates.y <= creet.coordinates.y_min
    ||
    creet.coordinates.y >= creet.coordinates.y_max -. (creet.size /. 2.) then (
    if creet.coordinates.y -. (creet.size /. 2.) <= 0. && creet.status.condition = Healthy then _change_condition creet;
    creet.coordinates.y_step <- Float.neg creet.coordinates.y_step;
    (* _move creet; *)
  );

  (match creet.status.condition with
  | Berserk | Mean -> _change_size creet
  | _ -> () );
  _move creet;

]
