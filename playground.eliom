[%%shared
open Eliom_content.Html.D

let elt = div ~a:[ a_class [ "playground" ]] []]

[%%client
open Eliom_content
open Js_of_ocaml
open Js_of_ocaml_lwt
open Creet

type playground = {
  dom_elt : Dom_html.divElement Js.t;
  mutable iter : int;
  mutable creets : creet list;
  mutable global_speed : float ref;
  mutable game_on : bool
}

let get () = {
    iter = 0;
    dom_elt = Html.To_dom.of_div ~%elt;
    creets = [];
    global_speed = ref 1.;
    game_on = true
  }

let _add_creet playground =
  let creet = Creet.create playground.global_speed in
  Dom.appendChild playground.dom_elt creet.dom_elt;
  playground.creets <- creet :: playground.creets
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
  if _is_game_over playground then (
    playground.game_on <- false;
    Eliom_lib.alert "GAME OVER";
    Lwt.return ())
  else (
    _increment_global_speed playground.global_speed;
    playground.iter <- playground.iter + 1;
    if playground.iter = 200 then (
      _add_creet playground;
      playground.iter <- 0
    );
    List.iter (_move_creet playground) playground.creets;
    (* TODO playground.global_speed <- playground.global_speed +. 0.001; *)
    _play playground
  )
let play playground =
  for _ = 1 to 3 do
    _add_creet playground
  done;
  Lwt.async (fun () -> _play playground);
  Lwt.return ()

]
