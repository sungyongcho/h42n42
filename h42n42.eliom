[%%shared
open Eliom_content
open Html.D
open Control
]

[%%client
open Params
open Layout
open Js_of_ocaml

let attach_id_action button_id action =
  match getElementById_opt button_id with
  | Some button ->
      button##.onclick := Dom_html.handler (fun _ ->
        action ();  (* Call the passed function *)
        Js._false
      )
  | None -> ()

  let main () =
    set_css_variables;
    Random.self_init ();
    let playground = Playground.get () in

    update_theme_display ();
    (* Attach event listeners to the theme selector buttons *)
    attach_id_action "theme-left" (fun () -> handle_theme_change (-1));
    attach_id_action "theme-right" (fun () -> handle_theme_change 1);

    (* Attach event listener to the Start button *)
    attach_id_action "start-button" (fun () ->
      (* Hide the Start button and theme selector *)
      (match getElementById_opt "button-container" with
       | Some btn -> btn##.style##.display := Js.string "none"
       | None -> ());
      (* Start the game asynchronously *)
      Lwt.async (fun () -> Playground.play playground);
    );

    (* Attach event listeners to Restart and Back to Start buttons *)
    attach_id_action "restart-button" (fun () ->
      Playground.restart_game playground
    );

    attach_id_action "back-to-start-button" (fun () ->
      Playground.back_to_start playground
    );

    let s_base_speed = Eliom_content.Html.To_dom.of_input ~%speed_slider in

    (* Event listener for updating the `base_speed` dynamically *)
    s_base_speed##.oninput := Dom_html.handler (fun _ ->
      let base_speed_value = Js.to_string s_base_speed##.value |> float_of_string in
      let span_speed_value = Eliom_content.Html.To_dom.of_span ~%speed_display in
      (* Perform actions with updated base_speed_value *)
      playground.global_speed := base_speed_value;
      span_speed_value##.textContent := (Js.some (Js.string (Printf.sprintf "%.4f" base_speed_value)));
      Js_of_ocaml.Firebug.console##log (Js.string (Printf.sprintf "Base speed: %.2f" base_speed_value));
      Js._false
    );

    Lwt.return ()

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
      div ~a:[ a_class [ "hospital" ] ] [ txt "hospital" ];

      (* Container for start button and theme selector *)
      div ~a:[ a_class [ "button-container" ]; a_id "button-container" ] [
        button ~a:[ a_class [ "start-button" ]; a_id "start-button" ] [ txt "Start" ];
        div ~a:[ a_class [ "theme-selector" ] ] [
          button ~a:[ a_id "theme-left" ] [ txt "<" ];
          span ~a:[ a_id "theme-display" ] [ txt "default" ];
          button ~a:[ a_id "theme-right" ] [ txt ">" ]
        ];
      ];

      (* Game Over Container: Initially Hidden *)
      div ~a:[
        a_class [ "game-over-container" ];
        a_id "game-over-container";
        a_style "display: none; text-align: center; margin-top: 20px;"
      ] [
        button ~a:[ a_id "restart-button"; a_class ["restart-button"] ] [ txt "Restart" ];
        button ~a:[ a_id "back-to-start-button"; a_class ["back-to-start-button"] ] [ txt "Back to Start" ];
      ];
    ];
    Playground.creets_counter_div;
    div ~a:[ a_class [ "speed-slider-container" ] ] [
      label ~a:[ a_label_for "speed-slider" ] [ txt "Global Speed: " ];
      speed_slider;
      speed_display;
    ]
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
