exception Cycle
type mark = NotVisited | InProgress | Visited

type 'a graph =
    { mutable g_nodes : ('a,'a node) Hashtbl.t;
    }
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

let mk_graph () = { g_nodes = Hashtbl.create 17 }

let add_node g x =
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
  Hashtbl.add g.g_nodes x n

let node_for_label g x =
  Hashtbl.find g.g_nodes x

let add_edge g id1 id2 =
  let n1 = node_for_label g id1 in
  let n2 = node_for_label g id2 in
  n1.n_link_to <- n2::n1.n_link_to;
  n2.n_linked_by <- n1::n2.n_linked_by

let clear_marks g =
  Hashtbl.iter (fun _ n -> n.n_mark <- NotVisited) g.g_nodes

let has_cycle g = 
  let rec dfs n=match n.n_mark with InProgress -> raise Cycle
	| Visited -> ()
	| NotVisited -> n.n_mark<-InProgress; List.iter dfs n.n_link_to; n.n_mark<-Visited
  in try (clear_marks g; Hashtbl.iter (fun _ n -> dfs n) g.g_nodes; false)
  with Cycle -> true

let topological g = 
  let rec dfs l n=match n.n_mark with Visited -> l
  	| InProgress -> raise Cycle
  	| NotVisited -> n.n_mark<-InProgress; let nl=n.n_label::(List.fold_left dfs l n.n_link_to) in n.n_mark<-Visited; nl
  in clear_marks g; Hashtbl.fold (fun _ d i->dfs i d) g.g_nodes [] ;

