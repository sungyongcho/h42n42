[%%client
open Js_of_ocaml
open Js_of_ocaml_lwt
open Eliom_content.Html.D
open Eliom_content.Html


type creet_state = Healthy | Sick | Berserk | Mean

type coordinates = {
  mutable x : int;
  mutable x_min : int;
  mutable x_max : int;
  mutable x_change : int;
  mutable y : int;
  mutable y_min : int;
  mutable y_max : int;
  mutable y_change : int;
  mutable state: creet_state;
}

type creet = {
  dom_elt : Dom_html.divElement Js.t;
  coordinates: coordinates
}

let get_bg_color state =
  Js.string (match state with
    | Healthy -> "dodgerblue"
    | Sick -> "darkblue"
    | Berserk -> "darkcyan"
    | Mean -> "tomato"
  )

let create ~x ~y () =
  let elt ~x ~y = div ~a:[
      a_class [ "creet" ];
      a_style ("position: absolute; left: " ^ string_of_int x ^ "px; top: " ^ string_of_int y ^ "px;")
  ] [] in
  let creet = {
    dom_elt = To_dom.of_div (elt ~x ~y);
    coordinates = {
      x = x;
      x_min = 0;
      x_max = 1000;
      x_change = 1;
      y = y;
      y_min = -15;
      y_max = 651;
      y_change = 1;
      state = Healthy
    }
  } in
  creet.dom_elt##.style##.backgroundColor := get_bg_color creet.coordinates.state;
  creet



let get_px number () = Js.string (Printf.sprintf "%dpx" number)

let change_x creet () =
  creet.coordinates.x <- creet.coordinates.x + creet.coordinates.x_change;
  creet.dom_elt##.style##.left := get_px creet.coordinates.x ()

let change_y creet () =
  creet.coordinates.y <- creet.coordinates.y + creet.coordinates.y_change;
  creet.dom_elt##.style##.top := get_px creet.coordinates.y ()

let change_state creet new_state =
  creet.coordinates.state <- new_state;
  creet.dom_elt##.style##.backgroundColor := get_bg_color creet.coordinates.state;

let rec move creet () =
  let%lwt () = Lwt_js.sleep 0.005 in
  (* Firebug.console##log (Js.string (Printf.sprintf "y: %d, y_min: %d" creet.coordinates.y creet.coordinates.y_min)); *)
  if creet.coordinates.x = creet.coordinates.x_min || creet.coordinates.x = (creet.coordinates.x_max - 50) then (
    creet.coordinates.x_change <- creet.coordinates.x_change * -1;
    change_x creet ();
    move creet ())
  else if creet.coordinates.y = creet.coordinates.y_min || creet.coordinates.y = (creet.coordinates.y_max - 50) then (
      if creet.coordinates.y <= 0 then change_state creet Sick;
    creet.coordinates.y_change <- creet.coordinates.y_change * -1;
    change_y creet ();
    move creet ())
  else (
    change_y creet ();
    change_x creet ();
    move creet () )
]
