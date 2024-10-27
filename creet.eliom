[%%client
open Js_of_ocaml
open Js_of_ocaml_lwt
open Eliom_content.Html.D
open Eliom_content.Html


let elt ~x ~y= div ~a:[
    a_class [ "creet" ];
    a_style ("position: absolute; left: " ^ string_of_int x ^ "px; top: " ^ string_of_int y ^ "px;")
] []

type coordinates = {
  mutable x : int;
  mutable x_min : int;
  mutable x_max : int;
  mutable x_change : int;
  mutable y : int;
  mutable y_min : int;
  mutable y_max : int;
  mutable y_change : int;
}

type creet = {
  dom_elt : Dom_html.divElement Js.t;
  coordinates: coordinates
}

let create ~x ~y () = {
  dom_elt = To_dom.of_div (elt ~x ~y);
  coordinates = {
    x = x;
    x_min = 0;
    x_max = 1000;
    x_change = 1;
    y = y;
    y_min = 0;
    y_max = 651;
    y_change = 1
  }
}

let get_px number () = Js.string (Printf.sprintf "%dpx" number)

let change_x creet () =
  creet.coordinates.x <- creet.coordinates.x + creet.coordinates.x_change;
  creet.dom_elt##.style##.left := get_px creet.coordinates.x ()

let change_y creet () =
  creet.coordinates.y <- creet.coordinates.y + creet.coordinates.y_change;
  creet.dom_elt##.style##.top := get_px creet.coordinates.y ()

let rec move creet () =
  let%lwt () = Lwt_js.sleep 0.005 in
  if creet.coordinates.x = creet.coordinates.x_min || creet.coordinates.x = (creet.coordinates.x_max - 50)then (
    creet.coordinates.x_change <- creet.coordinates.x_change * -1;
    change_x creet ();
    move creet ())
  else if creet.coordinates.y = creet.coordinates.y_min || creet.coordinates.y = (creet.coordinates.y_max - 50)then (
    creet.coordinates.y_change <- creet.coordinates.y_change * -1;
    change_y creet ();
    move creet ())
  else (
    change_y creet ();
    change_x creet ();
    move creet () )
]
