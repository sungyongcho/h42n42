[%%shared
open Eliom_content
open Html.D
]

[%%client
open Params
open Js_of_ocaml
open Dom_html

(* Helper function to get element by ID with option type *)
let getElementById_opt id =
  Js.Opt.to_option (document##getElementById (Js.string id))


let main () =
  set_css_variables;
  Random.self_init ();
  let playground = Playground.get () in

  (* Access the Start button by its ID *)
  match getElementById_opt "start-button" with
  | Some btn ->
      (* Add a click event listener to the Start button *)
      btn##.onclick := Dom_html.handler (fun _ ->
        (* Remove the Start button from the DOM *)
        btn##.style##.display := Js.string "none";

        (* Start the game asynchronously *)
        Lwt.async (fun () -> Playground.play playground);

        (* Prevent default behavior and stop propagation *)
        Js._false
      );
      Lwt.return ()  (* Return unit *)
  | None ->
      (* If the Start button is not found, start the game automatically *)
      Lwt.async (fun () -> Playground.play playground);
      Lwt.return ()  (* Return unit *)

]

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
  body [
    div ~a:[ a_class [ "gameboard" ] ] [
      div ~a:[ a_class [ "river" ] ] [];
      Playground.elt;
      div ~a:[ a_class [ "hospital" ] ] [ txt "hospital"]; (* Hospital div added *)
      button ~a:[ a_class [ "start-button" ]; a_id "start-button"] [ txt "Start" ];
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
