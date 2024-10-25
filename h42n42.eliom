open%server Eliom_content
open%server Eliom_content.Html.D
open%client Js_of_ocaml
module%server Graffiti_app =
  Eliom_registration.App (
    struct
      let application_name = "h42n42"
      let global_data_path = None
    end)

let%server width  = 700
let%server height = 400

let%client draw ctx ((r, g, b), size, (x1, y1), (x2, y2)) =
  let color = CSS.Color.string_of_t (CSS.Color.rgb r g b) in
  ctx##.strokeStyle := (Js.string color);
  ctx##.lineWidth := float size;
  ctx##beginPath;
  ctx##(moveTo (float x1) (float y1));
  ctx##(lineTo (float x2) (float y2));
  ctx##stroke

let%server canvas_elt =
  Html.D.canvas ~a:[Html.D.a_width width; Html.D.a_height height]
    [Html.D.txt "your browser doesn't support canvas"]

let%server page () =
  html
     (head (title (txt "Graffiti")) [])
     (body [h1 [txt "Graffiti"];
            canvas_elt])
let%client init_client () =
  let canvas = Eliom_content.Html.To_dom.of_canvas ~%canvas_elt in
  let ctx = canvas##(getContext (Dom_html._2d_)) in
  ctx##.lineCap := Js.string "round";
  draw ctx ((0, 0, 0), 12, (10, 10), (200, 100))
let%server main_service =
  Eliom_service.create
    ~path:(Eliom_service.Path ["graff"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let%server () =
  Graffiti_app.register
    ~service:main_service
    (fun () () ->
       (* Cf. section "Client side side-effects on the server" *)
       let _ = [%client (init_client () : unit) ] in
       Lwt.return (page ()))
