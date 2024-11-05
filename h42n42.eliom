[%%shared
open Eliom_content
open Html.D
]

[%%client
open Params

let main () =
  set_css_variables;
  let playground = Playground.get () in

  let creet1 = Creet.create ~x:100 ~y:150 () in
  Lwt.async (fun () -> Playground.add_creet playground creet1);
  Lwt.async (fun () -> Creet.move creet1);

  let creet2 = Creet.create ~x:200 ~y:300 () in
  Lwt.async (fun () -> Playground.add_creet playground creet2);
  Lwt.async (fun () -> Creet.move creet2);

  Lwt.return ()]

let%server application_name = "h42n42"
let%client application_name = Eliom_client.get_application_name ()

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
          div ~a:[ a_class [ "hospital" ] ] [];
          (* Hospital is a dashed line at the bottom *)
        ];
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
