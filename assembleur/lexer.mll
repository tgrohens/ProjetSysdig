(*Analyse lexicale*)

{
  open Lexing
  open Parser

  exception Lexing_error of string

  (* fonction à appeler à chaque retour chariot (caractère '\n') *)
  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      {pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

}


let alpha = ['a'-'z']
let chiffre = ['0'-'9']
let immediate = chiffre+


rule token = parse
  | " " { token lexbuf }
  | "\t" { token lexbuf }
  | "\n" { newline lexbuf ; token lexbuf }
  | "R1" { REGISTER 1 }
  | 'R' ((chiffre | '1' ['0'-'5']) as c) { REGISTER (int_of_string c) }
  | eof { EOF }
  | "PC" { REGISTER 15 }
  | "LR" { REGISTER 14 }
  | "SP" { REGISTER 13 }
  | ',' { COMMA }
  | ':' { COLON }
  | '[' { CROCHET_O }
  | ']' { CROCHET_F }
  | '#' ('+')? (immediate as n) { IMM (int_of_string n) }
  | '#' '-' (immediate as n) { IMM (-(int_of_string n)) }
  | immediate as n { QUICK_IMM (int_of_string n) }
  | '+' { PLUS }
  | '-' { MINUS }
  | "CPSR" { CPSR }
  | "SPSR" { SPSR }

(* Instructions *)
  | "ADC" { ADC }
  | "ADD" { ADD }
  | "AND" { AND }
  | "BIC" { BIC }
  | "EOR" { EOR }
  | "ORR" { ORR }
  | "RSB" { RSB }
  | "RSC" { RSC }
  | "SBC" { SBC }
  | "SUB" { SUB }
  | "MOV" { MOV }
  | "MVN" { MVN }

  | "B" { B }
  | "BL" { BL }
  | "CMP" { CMP }
  | "CMN" { CMN }
  
  | "SWI" { SWI }

  | "CLZ" { CLZ }

  | "LDR" { LDR }
  | "STR" { STR }

  | "MUL" { MUL }
  | "MLA" { MLA }

  | "MRS" { MRS }
  (*| "MSR" { MSR }*)

(* Conditions *)
  | "S" { S }
  | "EQ" { EQ }
  | "NE" { NE }
  | "CS" { CS }
  | "CC" { CC }
  | "MI" { MI }
  | "PL" { PL }
  | "VS" { VS }
  | "VC" { VC }
  | "HI" { HI }
  | "LS" { LS }
  | "GE" { GE }
  | "LT" { LT }
  | "GT" { GT }
  | "LE" { LE }
  | "AL" { AL }

(* Shifters *)
  | "LSL" { LSL }
  | "LSR" { LSR }
  | "ASR" { ASR }
  | "ROR" { ROR }
  | (alpha (alpha | chiffre)*) as s ":" { LABEL s }
  | (alpha (alpha | chiffre)*) as s { LABEL_B s }
