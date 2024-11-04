[%%client
open Creet
open Js_of_ocaml

type rectangle = {
  x: float;  (* Center x-coordinate *)
  y: float;  (* Center y-coordinate *)
  w: float;  (* Half-width *)
  h: float;  (* Half-height *)
}

type circle = {
  x: float;
  y: float;
  r: float;
}

let contains (rect: rectangle) creet =
  creet.coordinates.x >= rect.x -. rect.w &&
  creet.coordinates.x <= rect.x +. rect.w &&
  creet.coordinates.y >= rect.y -. rect.h &&
  creet.coordinates.y <= rect.y +. rect.h

type quadtree = {
  boundary: rectangle;
  capacity: int;
  mutable creets: creet list;
  mutable divided: bool;
  mutable northwest: quadtree option;
  mutable northeast: quadtree option;
  mutable southwest: quadtree option;
  mutable southeast: quadtree option;
}

let create_quadtree boundary capacity = {
  boundary = boundary;
  capacity = capacity;
  creets = [];
  divided = false;
  northwest = None;
  northeast = None;
  southwest = None;
  southeast = None;
}

let subdivide (qt: quadtree) =
  let x = qt.boundary.x in
  let y = qt.boundary.y in
  let half_w = qt.boundary.w /. 2. in
  let half_h = qt.boundary.h /. 2. in

  (* Define quadrant boundaries based on center mode *)
  let nw_rect = { x = x -. half_w; y = y -. half_h; w = half_w; h = half_h } in
  let ne_rect = { x = x +. half_w; y = y -. half_h; w = half_w; h = half_h } in
  let sw_rect = { x = x -. half_w; y = y +. half_h; w = half_w; h = half_h } in
  let se_rect = { x = x +. half_w; y = y +. half_h; w = half_w; h = half_h } in

  (* Create child quadtrees *)
  qt.northwest <- Some (create_quadtree nw_rect qt.capacity);
  qt.northeast <- Some (create_quadtree ne_rect qt.capacity);
  qt.southwest <- Some (create_quadtree sw_rect qt.capacity);
  qt.southeast <- Some (create_quadtree se_rect qt.capacity);

  (* Mark the node as divided *)
  qt.divided <- true

let rec insert qt creet =
  if not (contains qt.boundary creet) then
    false
  else if List.length qt.creets < qt.capacity then (
    qt.creets <- creet :: qt.creets;
    Printf.printf "Inserted creet directly into current quadtree.\n";
    true
  ) else (
    if not qt.divided then subdivide qt;

    match qt.northwest, qt.northeast, qt.southwest, qt.southeast with
    | Some nw, Some ne, Some sw, Some se ->
      if insert nw creet then (
        Firebug.console##log (Js.string "Creet inserted into northwest quadrant.");
        true
      ) else if insert ne creet then (
        Firebug.console##log (Js.string "Creet inserted into northeast quadrant.");
        true
      ) else if insert sw creet then (
        Firebug.console##log (Js.string "Creet inserted into southwest quadrant.");
        true
      ) else if insert se creet then (
        Firebug.console##log (Js.string "Creet inserted into southeast quadrant.");
        true
      ) else
        false
    | _ -> false
  )

let boundary_intersects_range (boundary: rectangle) (range: circle) =
  let x_dist = abs_float (range.x -. boundary.x) in
  let y_dist = abs_float (range.y -. boundary.y) in
  let w = boundary.w in
  let h = boundary.h in
  let r = range.r in

  if x_dist > (w +. r) || y_dist > (h +. r) then
    false  (* No intersection *)
  else if x_dist <= w || y_dist <= h then
    true   (* Circle intersects within the rectangle's sides *)
  else
    let corner_distance_sq =
      (x_dist -. w) ** 2.0 +. (y_dist -. h) ** 2.0
    in
    corner_distance_sq <= r ** 2.0  (* Circle intersects at the corners *)

let range_intersects_creet (range: circle) (creet: creet) =
  let dx = range.x -. creet.coordinates.x in
  let dy = range.y -. creet.coordinates.y in
  let distance_sq = dx *. dx +. dy *. dy in
  let radii_sum = range.r +. (creet.size /. 2.) in
  distance_sq <= radii_sum *. radii_sum

let rec query ?(found=[]) qt range =
  if not (boundary_intersects_range qt.boundary range) then
    found  (* If boundary doesn't intersect range, return current found list *)
  else
    let found =
      List.fold_left
        (fun acc creet ->
          if range_intersects_creet range creet then
            creet :: acc
          else
            acc)
        found
        qt.creets
    in
    if qt.divided then
      let found = match qt.northwest with
        | Some nw -> query ~found nw range
        | None -> found
      in
      let found = match qt.northeast with
        | Some ne -> query ~found ne range
        | None -> found
      in
      let found = match qt.southwest with
        | Some sw -> query ~found sw range
        | None -> found
      in
      let found = match qt.southeast with
        | Some se -> query ~found se range
        | None -> found
      in
      found
    else
      found
]
