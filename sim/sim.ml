open Netlist_ast

let valeurs=Array.init 2 (fun _ -> Hashtbl.create 17);;
let memoire=Hashtbl.create 17;;


let rec gen = function
	|[] -> 0
	|h::t -> (if h then 1 else 0) + 2*(gen t)

(*Fonction d'affichage de la date*)
let ai ram n = gen ram.(n)

let affiche_temps () =
	let ram = Hashtbl.find memoire "lecture" in
	Printf.printf "%.2d/%.2d/%.4d %.2d:%.2d:%.2d\n" (ai ram 3) (ai ram 4) (ai ram 7 + (1 lsl 8)*(ai ram 6) + (1 lsl 16)+(ai ram 5)) (ai ram 2) (ai ram 1) (ai ram 0)


let taille=function
        | TBit -> 1
        | TBitArray(a) -> a

let rec creer a=function
        | 0 -> []
        | n -> a::(creer a (n-1))

let rec lit s=
        try (
        let ch=ref "" and l=ref [] in
        ch:=read_line();
        for i=s-1 downto 0 do
                match !ch.[i] with
                | '0' -> l:=false::!l
                | '1' -> l:=true::!l
                | _ -> failwith "Erreur de lecture"
        done;
        !l
        ) with _ -> lit s

let init p= Array.iter (fun t->Hashtbl.reset t) valeurs;
        Hashtbl.reset memoire;
        Env.iter (fun id t->Hashtbl.add valeurs.(1) id (creer false (taille t))) p.p_vars;
        let creerRam (id,eq)=match eq with
        | Eram(addr,taille,_,_,_,_) -> Hashtbl.add memoire id (Array.init (1 lsl addr) (fun _ -> creer false taille))
        | Erom(addr,taille,_) -> Hashtbl.add memoire id (Array.init (1 lsl addr) (fun i -> lit taille))
        | _ -> ()
        in
        List.iter creerRam p.p_eqs

let rec conv=function [] -> 0
        | t::q -> (if t then 1 else 0)+2*(conv q)

let execute p iter=
        let convertit v=match v with VBit(b) -> [b]
                        | VBitArray(v) -> Array.to_list v

        in let valeur v=match v with Aconst(v) -> convertit v
                        | Avar(s) -> Hashtbl.find valeurs.(iter) s
        and ex_bin e a2 b2=let a=List.hd a2 and b=List.hd b2 in [match e with
                | Or -> a||b
                | Xor -> a!=b
                | And -> a&&b
                | Nand -> not (a&&b)]
        in let rec selec d f v=match d with
        | 0 -> if f>=0 then (List.hd v)::(selec 0 (f-1) (List.tl v)) else []
        | _ -> selec (d-1) (f-1) (List.tl v)
        in let exec (id,e)=Hashtbl.replace valeurs.(iter) id (match e with
        | Earg(a) -> valeur a
        | Ereg(s) -> Hashtbl.find valeurs.(1-iter) s
        | Enot(a) -> [not (List.hd (valeur a))]
        | Ebinop(e,a,b) -> ex_bin e (valeur a) (valeur b)
        | Emux(e,a,b) -> if List.hd (valeur e) then valeur b else valeur a
        | Econcat(a,b) -> (valeur a)@(valeur b)
        | Eslice(d,f,v) -> selec d f (valeur v)
        | Eselect(d,v) -> selec d d (valeur v)
        | Erom(_,_,addr) -> (Hashtbl.find memoire id).(conv (valeur addr))
        | Eram(_,_,lec,_,_,_) -> (Hashtbl.find memoire id).(conv (valeur lec)) 
        )
        and execEcr (id,e)=match e with
        | Eram(_,_,_,becr,ecrAddr,ecrVal) -> if List.hd (valeur becr) then
                (let tab=Hashtbl.find memoire id in tab.(conv (valeur ecrAddr))<-valeur ecrVal; Hashtbl.replace memoire id tab)
        | _ -> () 
        in let rec affliste v=match v with [] ->print_newline()
        | t::q -> if t then print_string "1" else print_string "0";affliste q
        in let affiche s=
        Printf.printf "%s = " s;
        affliste (valeur (Avar s));
        and litVar id=
        let s=taille (Env.find id p.p_vars) in
        Printf.printf "%s (%d) ? %!" id s;
        lit s
        in
        List.iter (fun s->Hashtbl.replace valeurs.(iter) s (litVar s)) p.p_inputs;
        List.iter exec p.p_eqs;
        List.iter execEcr p.p_eqs;
        List.iter affiche p.p_outputs

let simule p n=match n with
| -1 -> init p;
	let it=ref 0 in
	while true do
        let temps = Sys.time () in
		execute p !it; (* avance d'un cycle *)
		if gen (Hashtbl.find valeurs.(!it) "r10c") = 1 then
			affiche_temps ();
        while (Sys.time () -. temps < 1.) do () done
	done
| _ ->init p; for i=0 to n-1 do execute p (i mod 2); done 



