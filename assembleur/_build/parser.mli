exception Error

type token = 
  | VS
  | VC
  | SWI
  | SUB
  | STR
  | SPSR
  | SBC
  | S
  | RSC
  | RSB
  | ROR
  | REGISTER of (Ast.register)
  | PLUS
  | PL
  | ORR
  | NE
  | MVN
  | MUL
  | MRS
  | MOV
  | MLA
  | MINUS
  | MI
  | LT
  | LSR
  | LSL
  | LS
  | LE
  | LDR
  | LABEL of (Ast.label)
  | IMM of (int)
  | HI
  | GT
  | GE
  | EQ
  | EOR
  | EOF
  | CS
  | CROCHET_O
  | CROCHET_F
  | CPSR
  | COMMA
  | COLON
  | CMP
  | CMN
  | CLZ
  | CC
  | BL
  | BIC
  | B
  | ASR
  | AND
  | AL
  | ADD
  | ADC


val file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (((Ast.label*int) list) * Ast.instr list)