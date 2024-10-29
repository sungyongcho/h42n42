(* This file was generated by Eliom-distillery.
   Feel free to use it, modify it, and redistribute it as you wish. *)
[%%shared
open Eliom_content
open Html.D
]

[%%client
open Playground


let _is_game_over (playground : playground) =
  let any_healthy (creet : Creet.creet) = creet.status.condition = Healthy in
  List.length playground.creets = 0
  || not (List.exists any_healthy playground.creets)


let main () =
  Random.self_init ();
  let playground = Playground.get () in

  Lwt.async (fun () -> Playground.play playground);

  Lwt.return ()]


let%server application_name = "h42n42"
let%client application_name = Eliom_client.get_application_name ()

(* Create a module for the application. See
   https://ocsigen.org/eliom/manual/clientserver-applications for more
   information. *)
module%shared App = Eliom_registration.App (struct
    let application_name = application_name
    let global_data_path = Some ["__global_data__"]
  end)

(* As the headers (stylesheets, etc) won't change, we ask Eliom not to
   update the <head> of the page when changing page. (This also avoids
   blinking when changing page in iOS). *)
let%client _ = Eliom_client.persist_document_head ()

let%server main_service =
  Eliom_service.create ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit) ()

let%client main_service = ~%main_service


let%shared page () =
  body
    [
      div
        ~a:[ a_class [ "gameboard" ] ]
        [
          div ~a:[ a_class [ "river" ] ] [];
          Playground.elt;
          (* Hospital is a dashed line at the bottom *)
        ];
        Playground.creets_counter_div;
    ]


let%shared () =
  App.register ~service:main_service (fun () () ->
    let _ = [%client (main () : unit Lwt.t)] in
    Lwt.return
      (
        html
          (head
             (title (txt "h42n42"))
             [ css_link
                 ~uri:
                   (make_uri
                      ~service:(Eliom_service.static_dir ())
                      ["css"; "h42n42.css"]) ();
              ])
          (page ())))
