[%%client
open Js_of_ocaml
open Eliom_content.Html.D
open Eliom_content.Html


let elt ~x ~y= div ~a:[
    a_class [ "creet" ];
    a_style ("position: absolute; left: " ^ string_of_int x ^ "px; top: " ^ string_of_int y ^ "px;")
  ] []

type creet = { dom_elt : Dom_html.divElement Js.t }

let create ~x ~y () = {
  dom_elt = To_dom.of_div (elt ~x ~y)
}
]
