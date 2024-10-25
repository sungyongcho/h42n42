(* This file was generated by Eliom-distillery.
   Feel free to use it, modify it, and redistribute it as you wish. *)

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

let%shared () =
  App.register ~service:main_service (fun () () ->
    Lwt.return
      Eliom_content.Html.F.(
        html
          (head
             (title (txt "h42n42"))
             [ css_link
                 ~uri:
                   (make_uri
                      ~service:(Eliom_service.static_dir ())
                      ["css"; "h42n42.css"])
                 () ])
          (body [h1 [txt "Welcome to Eliom!"]])))
