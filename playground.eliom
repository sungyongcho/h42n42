[%%shared
open Eliom_content.Html.D

let elt = div ~a:[ a_class [ "playground" ]] []]

[%%client
open Eliom_content
open Js_of_ocaml
open Creet

type playground = {
  dom_elt : Dom_html.divElement Js.t;
  mutable creets : creet list;
  mutable game_on : bool
}

let get () = {
    dom_elt = Html.To_dom.of_div ~%elt;
    creets = [];
    game_on = true
  }

let add_creet playground (creet : creet) =
  Dom.appendChild playground.dom_elt creet.dom_elt;
  playground.creets <- creet :: playground.creets;
  Firebug.console##log_2 (Js.string "creets_nb") (List.length playground.creets);
  Lwt.return ()]
