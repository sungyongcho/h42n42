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

let contam_range_slider = input ~a:[
  a_input_type `Range;
  a_id "contam-range-slider";
  a_value ("100");             (* a_value remains as a string *)
  a_input_min (`Number 1);               (* Use the `Number variant *)
  a_input_max (`Number 100);               (* Use the `Number variant *)
] ()
let contam_range_display = span ~a:[ a_id "contam-range-value" ] [ txt (Printf.sprintf "%d%%" 100) ]

let contam_percent_slider = input ~a:[
  a_input_type `Range;
  a_id "contam-percent-slider";
  a_value ("2");             (* a_value remains as a string *)
  a_input_min (`Number 1);               (* Use the `Number variant *)
  a_input_max (`Number 100);               (* Use the `Number variant *)
] ()
let contam_percent_display = span ~a:[ a_id "contam-percent-value" ] [ txt (Printf.sprintf "%d%%" 2) ]

let creet_gen_slider = input ~a:[
  a_input_type `Range;
  a_id "creet-gen-slider";
  a_value ("10");             (* a_value remains as a string *)
  a_input_min (`Number 5);               (* Use the `Number variant *)
  a_input_max (`Number 15);               (* Use the `Number variant *)
] ()
let creet_gen_display = span ~a:[ a_id "creet-gen-value" ] [ txt (Printf.sprintf "%d seconds" 10) ]
]
