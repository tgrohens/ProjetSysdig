open Netlist_ast
open Graph
exception Combinational_cycle

let read_exp (id,eq) =
let extr a=match a with Avar(i)-> [i] | _ -> [] in
match eq with 
|Earg(a) -> extr a
|Ereg(i) -> []
|Enot(a) -> extr a
|Ebinop(_,a,b) -> List.concat [extr a;extr b]
|Emux(a,b,c)->List.concat [extr a;extr b;extr c]
|Econcat(a,b)-> List.concat [extr a;extr b]
|Eslice(_,_,a)-> extr a
|Eselect(_,a) -> extr a
|Erom(_,_,a) -> extr a
|Eram(_,_,a,b,c,d) -> extr a

let schedule p = let g=mk_graph() in
List.iter (fun a->add_node g a) p.p_eqs;
let table=Hashtbl.create 17 in
List.iter (fun a->Hashtbl.add table (fst a) a) p.p_eqs;
List.iter (fun eq->List.iter (fun nom ->try add_edge g (Hashtbl.find table nom) eq with _ -> ()) (read_exp eq)) p.p_eqs;
try {p with p_eqs=topological g} with Cycle-> raise Combinational_cycle

