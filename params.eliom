[%%client
open Js_of_ocaml

let creet_base_size = 50
let gameboard_width = 1000
let gameboard_height = 700
let river_height = 50
let hospital_height = 50

let set_css_variable (element : Dom_html.element Js.t) (name : string) (value : string) =
  let style = Js.Unsafe.coerce element##.style in
  style##setProperty (Js.string name) (Js.string value) Js.null

let set_css_variables =
  let root = Dom_html.document##.documentElement in
  ignore (set_css_variable root "--creet-size" (string_of_int creet_base_size ^ "px"));
  ignore (set_css_variable root "--gameboard-width" (string_of_int gameboard_width ^ "px"));
  ignore (set_css_variable root "--gameboard-height" (string_of_int gameboard_height ^ "px"));
  ignore (set_css_variable root "--river-height" (string_of_int river_height ^ "px"));
  ignore (set_css_variable root "--hospital-height" (string_of_int hospital_height ^ "px"))
]
