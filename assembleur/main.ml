open Ast
open Parser
open Lexing
open Printf



let print_prog oc label_list instr_list =
  let print_bool b =
    fprintf oc (if b then "1" else "0")
  in
  let binary_of_int n t =
    let res = Array.make t false in
    let rec aux k n = 
      if k >= 0 then (
	res.(k) <- (n mod 2 = 1);
	aux (k-1) (n/2)
      )
    in
    aux (t-1) n;
    res
  in
  let rec pow2 k =
    if k = 1 then 2
    else if k = 0 then 1
    else if k mod 2 = 0 then let x = pow2 (k/2) in x*x
    else let x = pow2 (k/2) in x*x*2
  in
  let print_unsigned_int n t =
    if n >= 0 then (fprintf oc "0" ; let res_n = binary_of_int n (t-1) in Array.iter print_bool res_n)
    else (fprintf oc "1" ; let res_n = binary_of_int ((pow2 t) + n) (t-1) in Array.iter print_bool res_n)
  in
  let print_register r =
    let res = binary_of_int r 4 in
    Array.iter print_bool res
  in
  let print_shifter = function
    |Reg r -> fprintf oc "00000000"; print_register r
    |Imm n ->
      (
	let res = binary_of_int n 32 in
        let k = ref 0 in
	let b = ref false in
	while (!k < 15 && not(!b)) do
	  b := true;
	  for i = 0 to 23 do
	    if res.((i + 2*(!k)) mod 32) then b := false;
	  done;
	  if not(!b) then incr k;
	done;
	if not(!b) then failwith "Valeur immédiate non-valide"
	else (
	  let res_k = binary_of_int (!k) 4 in
	  Array.iter print_bool res_k;
	  for i = 24 to 31 do
	    print_bool res.((i + 2*(!k)) mod 32);
	  done;
	)
      )
    |Lsl_by_i (rm, n) ->
      let res_n = binary_of_int n 5 in
      Array.iter print_bool res_n;
      fprintf oc "000";
      let res_rm = binary_of_int rm 4 in
      Array.iter print_bool res_rm;
    |Lsl_by_r (rm, rs) ->
      let res_rs = binary_of_int rs 4 in
      let res_rm = binary_of_int rm 4 in
      Array.iter print_bool res_rs;
      fprintf oc "0001";
      Array.iter print_bool res_rm;
    |Lsr_by_i (rm, n) ->
      let res_n = binary_of_int n 5 in
      Array.iter print_bool res_n;
      fprintf oc "010";
      let res_rm = binary_of_int rm 4 in
      Array.iter print_bool res_rm;
    |Lsr_by_r (rm, rs) ->
      let res_rs = binary_of_int rs 4 in
      let res_rm = binary_of_int rm 4 in
      Array.iter print_bool res_rs;
      fprintf oc "0011";
      Array.iter print_bool res_rm;
    |Asr_by_i (rm, n) ->
      let res_n = binary_of_int n 5 in
      Array.iter print_bool res_n;
      fprintf oc "100";
      let res_rm = binary_of_int rm 4 in
      Array.iter print_bool res_rm;
    |Asr_by_r (rm, rs) ->
      let res_rs = binary_of_int rs 4 in
      let res_rm = binary_of_int rm 4 in
      Array.iter print_bool res_rs;
      fprintf oc "0101";
      Array.iter print_bool res_rm;
    |Ror_by_i (rm, n) ->
      let res_n = binary_of_int n 5 in
      Array.iter print_bool res_n;
      fprintf oc "110";
      let res_rm = binary_of_int rm 4 in
      Array.iter print_bool res_rm;
    |Ror_by_r (rm, rs) ->
      let res_rs = binary_of_int rs 4 in
      let res_rm = binary_of_int rm 4 in
      Array.iter print_bool res_rs;
      fprintf oc "0111";
      Array.iter print_bool res_rm;
      
  in
  let print_cond = function
    |Eq -> fprintf oc "0000"
    |Ne -> fprintf oc "0001"
    |Cs -> fprintf oc "0010"
    |Cc -> fprintf oc "0011"
    |Mi -> fprintf oc "0100"
    |Pl -> fprintf oc "0101"
    |Vs -> fprintf oc "0110"
    |Vc -> fprintf oc "0111"
    |Hi -> fprintf oc "1000"
    |Ls -> fprintf oc "1001"
    |Ge -> fprintf oc "1010"
    |Lt -> fprintf oc "1011"
    |Gt -> fprintf oc "1100"
    |Le -> fprintf oc "1101"
    |Al -> fprintf oc "1110"
    |Nv -> fprintf oc "1111"
  in
  let print_instr n_instr = function
    |Adc (c, s, rn, rd, shift) -> ( print_cond c ; fprintf oc (match shift with |Imm _ -> "001" |_ -> "000") ; fprintf oc "0101" ; print_bool s ; print_register rn ; print_register rd ; print_shifter shift )
    |Add (c, s, rn, rd, shift) -> ( print_cond c ; fprintf oc (match shift with |Imm _ -> "001" |_ -> "000") ; fprintf oc "0100" ; print_bool s ; print_register rn ; print_register rd ; print_shifter shift )
    |And (c, s, rn, rd, shift) -> ( print_cond c ; fprintf oc (match shift with |Imm _ -> "001" |_ -> "000") ; fprintf oc "0000" ; print_bool s ; print_register rn ; print_register rd ; print_shifter shift )
    |Bic (c, s, rn, rd, shift) -> ( print_cond c ; fprintf oc (match shift with |Imm _ -> "001" |_ -> "000") ; fprintf oc "1110" ; print_bool s ; print_register rn ; print_register rd ; print_shifter shift )
    |Eor (c, s, rn, rd, shift) -> ( print_cond c ; fprintf oc (match shift with |Imm _ -> "001" |_ -> "000") ; fprintf oc "0001" ; print_bool s ; print_register rn ; print_register rd ; print_shifter shift )
    |Orr (c, s, rn, rd, shift) -> ( print_cond c ; fprintf oc (match shift with |Imm _ -> "001" |_ -> "000") ; fprintf oc "1100" ; print_bool s ; print_register rn ; print_register rd ; print_shifter shift )
    |Rsb (c, s, rn, rd, shift) -> ( print_cond c ; fprintf oc (match shift with |Imm _ -> "001" |_ -> "000") ; fprintf oc "0011" ; print_bool s ; print_register rn ; print_register rd ; print_shifter shift )
    |Rsc (c, s, rn, rd, shift) -> ( print_cond c ; fprintf oc (match shift with |Imm _ -> "001" |_ -> "000") ; fprintf oc "0111" ; print_bool s ; print_register rn ; print_register rd ; print_shifter shift )
    |Sbc (c, s, rn, rd, shift) -> ( print_cond c ; fprintf oc (match shift with |Imm _ -> "001" |_ -> "000") ; fprintf oc "0110" ; print_bool s ; print_register rn ; print_register rd ; print_shifter shift )
    |Sub (c, s, rn, rd, shift) -> ( print_cond c ; fprintf oc (match shift with |Imm _ -> "001" |_ -> "000") ; fprintf oc "0010" ; print_bool s ; print_register rn ; print_register rd ; print_shifter shift )

    |Mov (c, s, rd, shift) -> ( print_cond c ; fprintf oc (match shift with |Imm _ -> "001" |_ -> "000") ; fprintf oc "1101" ; print_bool s ; fprintf oc "0000" ; print_register rd ; print_shifter shift )
    |Mvn (c, s, rd, shift) -> ( print_cond c ; fprintf oc (match shift with |Imm _ -> "001" |_ -> "000") ; fprintf oc "1111" ; print_bool s ; fprintf oc "0000" ; print_register rd ; print_shifter shift )
    
    |Branch (c, l, label) -> ( print_cond c ; fprintf oc "101" ; print_bool l ; print_unsigned_int ((snd (List.find (fun x -> fst x = label) label_list)) - n_instr) 24 )
    |Branch_reg (c, l, r) -> ( print_cond c ; fprintf oc "0001001000000000000000" ; print_bool l ; fprintf oc "1" ; print_register r )
    |Cmp (c, rn, shift) -> ( print_cond c ; fprintf oc (match shift with |Imm _ -> "001" |_ -> "000") ; fprintf oc "10101" ; print_register rn ; fprintf oc "0000" ; print_shifter shift )

    |Cmn (c, rn, shift) -> ( print_cond c ; fprintf oc (match shift with |Imm _ -> "001" |_ -> "000") ; fprintf oc "10111" ; print_register rn ; fprintf oc "0000" ; print_shifter shift )

    |Teq (c, rn, shift) -> ( print_cond c ; fprintf oc (match shift with |Imm _ -> "001" |_ -> "000") ; fprintf oc "10011" ; print_register rn ; fprintf oc "0000" ; print_shifter shift )

    |Tst (c, rn, shift) -> ( print_cond c ; fprintf oc (match shift with |Imm _ -> "001" |_ -> "000") ; fprintf oc "10001" ; print_register rn ; fprintf oc "0000" ; print_shifter shift )


    |Swi (c, n) -> ( print_cond c ; fprintf oc "1111" ; let res = binary_of_int n 24 in Array.iter print_bool res)

    |Clz (c, rd, rm) -> (print_cond c ; fprintf oc "000101100000" ; print_register rd ; fprintf oc "00000001" ; print_register rm )

    |Load_store_offset (c, u, l, i, rd, rn, n) -> (print_cond c ; fprintf oc "010" ; print_bool (i <= 0) ; print_bool u ; fprintf oc "0" ; print_bool (i = -1); print_bool l ; print_register rn ; print_register rd ; let res_n = binary_of_int n 12 in Array.iter print_bool res_n)

    |Load_store_register (c, u, l, i, rd, rn, rm) -> (print_cond c ; fprintf oc "011" ; print_bool (i <= 0) ; print_bool u ; fprintf oc "0" ; print_bool (i = -1) ; print_bool l ; print_register rn ; print_register rd ; fprintf oc "00000000" ; print_register rm)

    |Mul(c, s, rd, rm, rs) -> (print_cond c ; fprintf oc "0000000" ; print_bool s ; print_register rd ; fprintf oc "0000" ; print_register rs ; fprintf oc "1001" ; print_register rm )
    |Mla(c, s, rd, rm, rs, rn) -> (print_cond c ; fprintf oc "0000001" ; print_bool s ; print_register rd ; print_register rn ; print_register rs ; fprintf oc "1001" ; print_register rm )

    |Mrs (c, r, rd) -> (print_cond c ; fprintf oc "00010" ; print_bool r ; fprintf oc "000000" ; print_register rd ; fprintf oc "000000000000" )


    |_ -> assert false
  in
  List.iteri (fun n i -> print_instr n i ; fprintf oc "\n" ) instr_list;
  let n = List.length instr_list in
  for k = n to (1 lsl 16)-1 do
    for l = 0 to 31 do
        fprintf oc "0";
    done;
    fprintf oc "\n";
  done
  


let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  Printf.eprintf "File \"%s\", line %d, characters %d-%d:\n" pos.pos_fname l (c-1) c

let find_file filename =
  try
    open_in filename
  with
    | _ -> raise (failwith "No such file '%s'")

let compile filename =
  let ic = find_file filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  
  try
    let label_list, instr_list = Parser.file Lexer.token lexbuf in
    let oc = open_out ((Filename.chop_suffix filename ".s")^".byte") in
    print_prog oc label_list instr_list;
  with
    | Parser.Error ->
      localisation (Lexing.lexeme_start_p lexbuf);
      Printf.eprintf "syntax error\n";
      exit 1

    | Lexer.Lexing_error c ->
      localisation (Lexing.lexeme_start_p lexbuf);
      Printf.eprintf "Erreur lexicale: %s\n" c;
      exit 1


let main () =
  Arg.parse 
    []
    compile
    ""
;;

main ()
