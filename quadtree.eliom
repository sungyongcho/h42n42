[%%client
open Creet

type rectangle = {
  x: float;  (* Center x-coordinate *)
  y: float;  (* Center y-coordinate *)
  w: float;  (* Half-width *)
  h: float;  (* Half-height *)
}

let contains rect creet =
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
        Printf.printf "Creet inserted into northwest quadrant.\n";
        true
      ) else if insert ne creet then (
        Printf.printf "Creet inserted into northeast quadrant.\n";
        true
      ) else if insert sw creet then (
        Printf.printf "Creet inserted into southwest quadrant.\n";
        true
      ) else if insert se creet then (
        Printf.printf "Creet inserted into southeast quadrant.\n";
        true
      ) else
        false
    | _ -> false
  )
]
