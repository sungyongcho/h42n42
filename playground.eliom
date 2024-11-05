[%%shared
open Eliom_content.Html.D

let elt = div ~a:[ a_class [ "playground" ]] []
let creets_counter_div = div ~a:[ a_class [ "creets-counter" ] ] []]

[%%client
open Params
open Eliom_content
open Js_of_ocaml_lwt
open Creet
open Quadtree

type playground = {
  mutable iter : int;
  mutable creets : creet list;
  mutable global_speed : float ref;
  mutable game_on : bool;
  mutable creets_counter_span : Html_types.span elt;
}

let get () =
  let playground = {
    iter = 0;
    creets_counter_span = span [ txt "0 creets" ];
    creets = [];
    global_speed = ref 1.;
    game_on = true
  } in
  (* let dom_creets_counter_div = Html.To_dom.of_div ~%creets_counter_div in
  let creets_counter_span =
    Html.To_dom.of_element playground.creets_counter_span
  in *)
  Html.Manip.appendChild ~%creets_counter_div playground.creets_counter_span;
  playground

let _update_dom_creets_counter playground =
  let creets_nb = List.length playground.creets in
  let plural = if creets_nb = 1 then ' ' else 's' in
  let new_count = span [ txt (Printf.sprintf "%d creet%c" creets_nb plural) ] in
  let old_count = playground.creets_counter_span in
  Html.Manip.replaceSelf old_count new_count;
  playground.creets_counter_span <- new_count

let _add_creet playground =
  let creet = Creet.create playground.global_speed in
  Html.Manip.appendChild ~%elt creet.elt;
  playground.creets <- creet :: playground.creets;
  _update_dom_creets_counter playground
  (* Firebug.console##log_2 (Js.string "creets_nb") (List.length playground.creets); *)

let _move_creet playground (creet : creet) =
  if playground.game_on then
    Creet.move creet

let _is_game_over (playground : playground) =
  let any_healthy (creet : Creet.creet) = creet.status.condition = Healthy in
  List.length playground.creets = 0
  || not (List.exists any_healthy playground.creets)

let _increment_global_speed gs = gs := !gs +. 0.0001

let rec _play playground =
  let%lwt () = Lwt_js.sleep 0.01 in
  let game_on = Creet.check_healthy_creets playground.creets in
  if game_on then (
    _increment_global_speed playground.global_speed;
    playground.iter <- playground.iter + 1;
    if playground.iter = 2000 then (
      _add_creet playground;
      playground.iter <- 0
    );
    let boundary = {
      x = (float_of_int gameboard_width)/. 2.;
      y = (float_of_int gameboard_height) /. 2. +. (float_of_int river_height);
      w = (float_of_int gameboard_width) /. 2.;
      h = (float_of_int gameboard_height -. float_of_int river_height -. float_of_int hospital_height) /. 2.;
    } in
    let qt = Quadtree.create_quadtree boundary 4 in
    List.iter (fun creet -> ignore (Quadtree.insert qt creet)) playground.creets;
    (* Iterate over each creet *)
    let available_creets = List.filter (fun creet -> creet.available = true) playground.creets in

    List.iter (_move_creet playground) available_creets;

    (* Check collisions for sick creets *)
    _check_sick_creet_collisions qt playground.creets;

    (* TODO playground.global_speed <- playground.global_speed +. 0.001; *)
    _play playground
  )
  else (
    playground.game_on <- false;
    Eliom_lib.alert "GAME OVER";
    Lwt.return ()
  )
let play playground =
  for _ = 1 to 7 do
    _add_creet playground
  done;
  Lwt.async (fun () -> _play playground);
  Lwt.return ()
]
