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

(* * Define the themes and initial theme state *)
let themes = [| Default; BlackAndWhite; Transparent |]
let current_theme_index = ref 0

(* Function to update the theme text display *)
let update_theme_display () =
  match getElementById_opt "theme-display" with
  | Some theme_display ->
    (
      let theme_name = theme_to_string themes.(!current_theme_index) in
      theme_display##.innerHTML := Js.string theme_name;
      let theme = match themes.(!current_theme_index) with
      | Default -> {
        bg_color = "#f0f0f0";
        river_color = "#4ab3b4";
        hospital_color = "white"
      }
      | BlackAndWhite -> {
        bg_color = "black";
        river_color = "white";
        hospital_color = "red"
      }
      | Transparent -> {
        bg_color = "gray";
        river_color = "yellow";
        hospital_color = "purple"
      }
      in
      set_css_variable_by_class "playground" "background-color" theme.bg_color;
      set_css_variable_by_class "river" "background-color" theme.river_color;
      set_css_variable_by_class "hospital" "background-color" theme.hospital_color
    )
  | None -> ()

(* Function to handle left and right button clicks for changing theme *)
let handle_theme_change direction =
  current_theme_index := (!current_theme_index + direction + Array.length themes) mod Array.length themes;
  update_theme_display ()

let main () =
  set_css_variables;
  Random.self_init ();
  let playground = Playground.get () in

  update_theme_display ();
  (* Attach event listeners to the theme selector buttons *)
  begin
    match getElementById_opt "theme-left" with
    | Some left_button ->
        left_button##.onclick := Dom_html.handler (fun _ ->
          handle_theme_change (-1);
          Js._false
        )
    | None -> ()
  end;
  begin
    match getElementById_opt "theme-right" with
    | Some right_button ->
        right_button##.onclick := Dom_html.handler (fun _ ->
          handle_theme_change 1;
          Js._false
        )
    | None -> ()
  end;

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
      div ~a:[ a_class [ "hospital" ] ] [ txt "hospital" ]; (* Hospital div added *)

      (* Container for start button and theme selector *)
      div ~a:[ a_class [ "button-container" ] ] [
        button ~a:[ a_class [ "start-button" ]; a_id "start-button" ] [ txt "Start" ];
        div ~a:[ a_class [ "theme-selector" ] ] [
          button ~a:[ a_id "theme-left" ] [ txt "<" ];
          span ~a:[ a_id "theme-display" ] [ txt "default" ];  (* Initial theme display *)
          button ~a:[ a_id "theme-right" ] [ txt ">" ]
        ];
      ];
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
