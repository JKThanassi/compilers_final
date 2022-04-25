open Printf
module NeighborSet = Set.Make (String)

type neighborst = NeighborSet.t

module Graph = Map.Make (String)

type grapht = neighborst Graph.t

module StringSet = Set.Make (String)

type livet = StringSet.t

let empty : grapht = Graph.empty

let add_node (g : grapht) (name : string) : grapht =
  if Graph.mem name g then g else Graph.add name NeighborSet.empty g
;;

let add_directed_edge (g : grapht) (n1 : string) (n2 : string) : grapht =
  let g' = add_node (add_node g n1) n2 in
  let curr_neighbors = Graph.find n1 g' in
  Graph.add n1 (NeighborSet.add n2 curr_neighbors) g'
;;

let add_edge (g : grapht) (n1 : string) (n2 : string) : grapht =
  if n1 != n2
  then (
    let g' = add_directed_edge g n1 n2 in
    add_directed_edge g' n2 n1)
  else g
;;

let get_neighbors (g : grapht) (name : string) : string list =
  if Graph.mem name g
  then NeighborSet.fold (fun n ns -> n :: ns) (Graph.find name g) []
  else []
;;

let get_vertices (g : grapht) : string list =
  let keys, _ = List.split (Graph.bindings g) in
  keys
;;

let string_of_graph (g : grapht) : string =
  let string_of_neighbors (n : string) : string =
    ExtString.String.join ", " (get_neighbors g n)
  in
  ExtString.String.join
    "\n"
    (List.map (fun k -> sprintf "%s: %s" k (string_of_neighbors k)) (get_vertices g))
;;

let remove_node_and_edges (node : string) (g : grapht) : grapht =
  let new_g = Graph.remove node g in
  Graph.map (fun m -> NeighborSet.remove node m) new_g
;;

let create_worklist_from_graph (g : grapht) : string Stack.t =
  let worklist : string Stack.t = Stack.create () in
  let verts = get_vertices g in
  let sorted_nodes =
    List.sort
      (fun node1 node2 ->
        (node1 |> get_neighbors g |> List.length)
        - (node2 |> get_neighbors g |> List.length))
      verts
  in
  let _ =
    List.fold_left
      (fun g' node ->
        Stack.push node worklist;
        remove_node_and_edges node g')
      g
      sorted_nodes
  in
  worklist
;;
