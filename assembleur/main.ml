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
	res.(k) <- (n mod 2 == 1);
	aux (k-1) (n/2)
      )
    in
    aux (t-1) n;
    res
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
	while (!k < 7 && not(!b)) do
	  b := true;
	  for i = 0 to 23 do
	    if res.((i + 2*(!k)) mod 32) then b := false;
	  done;
	  if not(!b) then incr k;
	done;
	if not(!b) then failwith "Valeur immédiate non-valide"
	else (
	  let res_k = binary_of_int (!k) 3 in
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
  let print_instr = function
    |Adc (c, s, rn, rd, shift) -> ( print_cond c ; fprintf oc (match shift with |Imm _ -> "001" |_ -> "000") ; fprintf oc "0101" ; print_bool s ; print_register rn ; print_register rd ; print_shifter shift )
    |_ -> assert false
  in
  List.iter print_instr instr_list
  


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
