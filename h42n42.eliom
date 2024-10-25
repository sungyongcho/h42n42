(* This file was generated by Eliom-distillery.
   Feel free to use it, modify it, and redistribute it as you wish. *)
[%%shared

open Eliom_content.Html.D
]


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

(* let%shared temp () =
  div ~a:[ a_class ["bg-black p-4 rounded-lg"] ] [ (* p-4 for padding, rounded-lg for rounded corners *)
               h1 ~a:[a_class ["text-3xl font-bold text-gray-800"]] [pcdata "Hello, Ocsigen + Tailwind!"];
  ] *)

let%shared playground_elt = div ~a:[ a_class [ "playground" ]] []
let%shared page () =
  body
    [
      div
        ~a:[ a_class [ "gameboard" ] ]
        [
          div ~a:[ a_class [ "river" ] ] [];
          playground_elt;
          (* Hospital is a dashed line at the bottom *)
        ];
    ]

[%%client
open Js_of_ocaml

let create_creet_dom_elt () =
  let elt = div ~a:[ a_class [ "creet" ] ] [] in
  Eliom_content.Html.(To_dom.of_div elt)

let init_client () =
  let playground = Eliom_content.Html.To_dom.of_div ~%playground_elt in
  Firebug.console##log_2 (Js.string "playground") playground;

  let creet_dom_elt = create_creet_dom_elt () in
  Firebug.console##log_2 (Js.string "creet_dom_elt") creet_dom_elt;

  Dom.appendChild playground creet_dom_elt]

let%shared () =
  App.register ~service:main_service (fun () () ->
    let _ = [%client (init_client () : unit)] in
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
