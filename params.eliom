[%%client
open Js_of_ocaml

let creet_base_size = 50.
let gameboard_width = 1000
let gameboard_height = 700
let river_height = 50
let hospital_height = 50

type theme = Default | BlackAndWhite | OceanBreeze | TwilightPurple | ForestMist | SpringBloom

type theme_property = {
  bg_color: string;
  river_color: string;
  hospital_color: string;
  text_color: string;
  healthy_creet_color: string;
  sick_creet_color: string;
  berserk_creet_color: string;
  mean_creet_color: string;
}

let theme_to_string = function
  | Default -> "default"
  | BlackAndWhite -> "black-and-white"
  | OceanBreeze -> "OceanBreeze"
  | TwilightPurple -> "TwilightPurple"
  | ForestMist -> "ForestMist"
  | SpringBloom -> "SpringBloom"

let get_theme_property = function
  | Default -> {
      bg_color = "#f0f0f0";
      river_color = "#4ab3b4";
      hospital_color = "white";
      text_color = "black";
      healthy_creet_color = "dodgerblue";
      sick_creet_color = "darkblue";
      berserk_creet_color = "darkcyan";
      mean_creet_color = "tomato";
    }
  | BlackAndWhite -> {
    bg_color = "#FFFFFF";                (* Pure Black for Playground *)
    river_color = "#1A1A1A";             (* Very Dark Gray *)
    hospital_color = "#333333";          (* Dark Gray *)
    text_color = "#FFFFFF";              (* Pure White *)
    healthy_creet_color = "#B3B3B3";     (* Light Gray *)
    sick_creet_color = "#808080";        (* Gray *)
    berserk_creet_color = "#666666";     (* Medium Gray *)
    mean_creet_color = "#4D4D4D";        (* Dim Gray *)
    }
  | OceanBreeze -> {
    bg_color = "#1E90FF";                (* DodgerBlue for Playground *)
    river_color = "#00CED1";             (* DarkTurquoise *)
    hospital_color = "#4682B4";          (* SteelBlue *)
    text_color = "#F0F8FF";              (* AliceBlue *)
    healthy_creet_color = "#7FFFD4";      (* Aquamarine *)
    sick_creet_color = "#20B2AA";        (* LightSeaGreen *)
    berserk_creet_color = "#008B8B";     (* DarkCyan *)
    mean_creet_color = "#00008B";        (* DarkBlue *)
    }
  | TwilightPurple -> {
      bg_color = "#4B0082";                (* Indigo for Playground *)
      river_color = "#6A5ACD";             (* SlateBlue *)
      hospital_color = "#9370DB";          (* MediumPurple *)
      text_color = "#F8F8FF";              (* GhostWhite *)
      healthy_creet_color = "#BA55D3";      (* MediumOrchid *)
      sick_creet_color = "#9932CC";        (* DarkOrchid *)
      berserk_creet_color = "#8A2BE2";     (* BlueViolet *)
      mean_creet_color = "#9400D3";        (* DarkViolet *)
    }
  | ForestMist -> {
    bg_color = "#2E8B57";                (* SeaGreen for Playground *)
    river_color = "#3CB371";             (* MediumSeaGreen *)
    hospital_color = "#556B2F";          (* DarkOliveGreen *)
    text_color = "#F5FFFA";              (* MintCream *)
    healthy_creet_color = "#98FB98";     (* PaleGreen *)
    sick_creet_color = "#66CDAA";        (* MediumAquamarine *)
    berserk_creet_color = "#20B2AA";     (* LightSeaGreen *)
    mean_creet_color = "#008B8B";        (* DarkCyan *)
    }
  | SpringBloom -> {
      bg_color = "#98FB98";                (* PaleGreen for Playground *)
      river_color = "#00CED1";             (* DarkTurquoise *)
      hospital_color = "#FF69B4";          (* HotPink *)
      text_color = "#2F4F4F";              (* DarkSlateGray *)
      healthy_creet_color = "#32CD32";      (* LimeGreen *)
      sick_creet_color = "#FFA500";        (* Orange *)
      berserk_creet_color = "#FF4500";     (* OrangeRed *)
      mean_creet_color = "#8B0000";        (* DarkRed *)
    }

let set_css_variable (element : Dom_html.element Js.t) (name : string) (value : string) =
  let style = Js.Unsafe.coerce element##.style in
  style##setProperty (Js.string name) (Js.string value) Js.null

let set_css_variable_by_class class_name (name : string) (value : string) =
  let elements = Dom_html.document##getElementsByClassName (Js.string class_name) in
  for i = 0 to elements##.length - 1 do
    let element = Js.Opt.get (elements##item i) (fun () -> assert false) in
    let style = Js.Unsafe.coerce element##.style in
    ignore (style##setProperty (Js.string name) (Js.string value) Js.null)
  done

let set_css_variables =
  let root = Dom_html.document##.documentElement in
  ignore (set_css_variable root "--creet-size" (string_of_int (int_of_float creet_base_size) ^ "px"));
  ignore (set_css_variable root "--gameboard-width" (string_of_int gameboard_width ^ "px"));
  ignore (set_css_variable root "--gameboard-height" (string_of_int gameboard_height ^ "px"));
  ignore (set_css_variable root "--river-height" (string_of_int river_height ^ "px"));
  ignore (set_css_variable root "--hospital-height" (string_of_int hospital_height ^ "px"))
]
