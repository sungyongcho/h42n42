[%%client
open Params
open Js_of_ocaml
open Dom_html

let getElementById_opt id =
  Js.Opt.to_option (document##getElementById (Js.string id))

let themes = [| Default; BlackAndWhite; OceanBreeze ; TwilightPurple ; ForestMist ; SpringBloom |]
let current_theme_index = ref 0

let update_theme_display () =
  match getElementById_opt "theme-display" with
  | Some theme_display ->
    (
      let theme_name = theme_to_string themes.(!current_theme_index) in
      theme_display##.innerHTML := Js.string theme_name;
      let theme = get_theme_property themes.(!current_theme_index) in
      set_css_variable_by_class "playground" "background-color" theme.bg_color;
      set_css_variable_by_class "river" "color" theme.text_color;
      set_css_variable_by_class "river" "background-color" theme.river_color;
      set_css_variable_by_class "hospital" "color" theme.text_color;
      set_css_variable_by_class "hospital" "background-color" theme.hospital_color;
      set_css_variable_by_class "creet" "background-color" theme.healthy_creet_color;
    )
  | None -> ()

let handle_theme_change direction =
  current_theme_index := (!current_theme_index + direction + Array.length themes) mod Array.length themes;
  update_theme_display ()

]
