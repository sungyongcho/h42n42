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
  mutable x: int;
  mutable y: int
}

type creet = {
  dom_elt : Dom_html.divElement Js.t;
  coordinates: coordinates
}

let create ~x ~y () = {
  dom_elt = To_dom.of_div (elt ~x ~y);
  coordinates = {x = x; y=y }
}


let get_px number () = Js.string (Printf.sprintf "%dpx" number)

let rec move creet () =
  match creet.coordinates.x with
  | 950 -> Lwt.return ()
  | x ->
    let%lwt () = Lwt_js.sleep 0.005 in
    creet.coordinates.x <- x + 1;
    creet.dom_elt##.style##.left := get_px creet.coordinates.x ();

    Firebug.console##log_2 (Js.string "left") creet.coordinates.x;
    move creet ()

]
