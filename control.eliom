[%%shared
open Eliom_content
open Html.D
let speed_slider = input ~a:[
  a_input_type `Range;
  a_id "speed-slider";
  a_value ("1.000");             (* a_value remains as a string *)
  a_input_min (`Number 1);               (* Use the `Number variant *)
  a_input_max (`Number 5);               (* Use the `Number variant *)
] ()
let speed_display = span ~a:[ a_id "speed-value" ] [ txt "1.000" ]  (* Initial value displayed *)
]
