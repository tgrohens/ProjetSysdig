/* Analyseur syntaxique avec l'outil menhir */

%{
  open Ast
  open Lexing
  exception Parsing_error of position*position*string

  let l = ref 0
%}


/* Définition des token */
%token <Ast.register> REGISTER
%token <int> IMM
%token COMMA
%token COLON
%token CROCHET_O CROCHET_F
%token PLUS MINUS
%token EOF
%token <Ast.label> LABEL
%token CPSR SPSR

(*Instructions*)
%token ADC ADD AND BIC EOR ORR RSB RSC SBC SUB MOV MVN

%token B BL CMP CMN

%token SWI

%token CLZ

%token LDR STR

%token MUL MLA

%token MRS /*MSR*/

(*Termes spéciaux*)
%token S

(*Conditions*)
%token EQ NE CS CC MI PL VS VC HI LS GE LT GT LE AL

(*Shifters*)
%token LSL LSR ASR ROR




%start file
%type <((Ast.label*int) list) * Ast.instr list> file


%%

file:
  |s = label f = file
   { let (l1, l2) = f in (s::l1, l2) }
  |i = instruction f = file
   { let (l1, l2) = f in (l1, i::l2) }
  |EOF
   { ([], []) }
;


label:
  |s = LABEL COLON
   { (s, !l) }
;

instruction:
  |ADC c = option(cond) s = boption(S) rd = REGISTER COMMA rn = REGISTER COMMA shift = shifter
       { incr l ; Adc ((match c with |None -> Al |Some c -> c), s, rn, rd, shift) }
  |ADD c = option(cond) s = boption(S) rd = REGISTER COMMA rn = REGISTER COMMA shift = shifter
       { incr l ; Add ((match c with |None -> Al |Some c -> c), s, rn, rd, shift) }
  |AND c = option(cond) s = boption(S) rd = REGISTER COMMA rn = REGISTER COMMA shift = shifter
       { incr l ; And ((match c with |None -> Al |Some c -> c), s, rn, rd, shift) }
  |BIC c = option(cond) s = boption(S) rd = REGISTER COMMA rn = REGISTER COMMA shift = shifter
       { incr l ; Bic ((match c with |None -> Al |Some c -> c), s, rn, rd, shift) }
  |EOR c = option(cond) s = boption(S) rd = REGISTER COMMA rn = REGISTER COMMA shift = shifter
       { incr l ; Eor ((match c with |None -> Al |Some c -> c), s, rn, rd, shift) }
  |ORR c = option(cond) s = boption(S) rd = REGISTER COMMA rn = REGISTER COMMA shift = shifter
       { incr l ; Orr ((match c with |None -> Al |Some c -> c), s, rn, rd, shift) }
  |RSB c = option(cond) s = boption(S) rd = REGISTER COMMA rn = REGISTER COMMA shift = shifter
       { incr l ; Rsb ((match c with |None -> Al |Some c -> c), s, rn, rd, shift) }
  |RSC c = option(cond) s = boption(S) rd = REGISTER COMMA rn = REGISTER COMMA shift = shifter
       { incr l ; Rsc ((match c with |None -> Al |Some c -> c), s, rn, rd, shift) }
  |SBC c = option(cond) s = boption(S) rd = REGISTER COMMA rn = REGISTER COMMA shift = shifter
       { incr l ; Sbc ((match c with |None -> Al |Some c -> c), s, rn, rd, shift) }
  |SUB c = option(cond) s = boption(S) rd = REGISTER COMMA rn = REGISTER COMMA shift = shifter
       { incr l ; Sub ((match c with |None -> Al |Some c -> c), s, rn, rd, shift) }

  |MOV c = option(cond) s = boption(S) rd = REGISTER COMMA shift = shifter
       { incr l ; Mov ((match c with |None -> Al |Some c -> c), s, rd, shift) }
  |MVN c = option(cond) s = boption(S) rd = REGISTER COMMA shift = shifter
       { incr l ; Mvn ((match c with |None -> Al |Some c -> c), s, rd, shift) }


  |B c = option(cond) lab = LABEL
       { incr l ; Branch ((match c with |None -> Al |Some c -> c), false, lab) }
  |BL c = option(cond) lab = LABEL
       { incr l ; Branch ((match c with |None -> Al |Some c -> c), true, lab) }
  |CMP c = option(cond) rn = REGISTER COMMA shift = shifter
       { incr l ; Cmp ((match c with |None -> Al |Some c -> c), rn, shift) }
  |CMN c = option(cond) rn = REGISTER COMMA shift = shifter
       { incr l ; Cmn ((match c with |None -> Al |Some c -> c), rn, shift) }

  |SWI c = option(cond) n = IMM
       { incr l ; Swi ((match c with |None -> Al |Some c -> c), n) }

  |CLZ c = option(cond) rd = REGISTER COMMA rm = REGISTER
       { incr l ; Clz ((match c with |None -> Al |Some c -> c), rd, rm) }

  |LDR c = option(cond) rd = REGISTER COMMA CROCHET_O rn = REGISTER COMMA n = IMM CROCHET_F
       { incr l ; Load_store_offset ((match c with |None -> Al |Some c -> c), (n >= 0), true, rd, rn, if n >= 0 then n else -n) }
  |LDR c = option(cond) rd = REGISTER COMMA CROCHET_O rn = REGISTER COMMA PLUS rm = REGISTER CROCHET_F
       { incr l ; Load_store_register ((match c with |None -> Al |Some c -> c), true, true, rd, rn, rm) }
  |LDR c = option(cond) rd = REGISTER COMMA CROCHET_O rn = REGISTER COMMA MINUS rm = REGISTER CROCHET_F
       { incr l ; Load_store_register ((match c with |None -> Al |Some c -> c), false, true, rd, rn, rm) }
  |STR c = option(cond) rd = REGISTER COMMA CROCHET_O rn = REGISTER COMMA n = IMM CROCHET_F
       { incr l ; Load_store_offset ((match c with |None -> Al |Some c -> c), (n >= 0), false, rd, rn, if n >= 0 then n else -n) }
  |STR c = option(cond) rd = REGISTER COMMA CROCHET_O rn = REGISTER COMMA PLUS rm = REGISTER CROCHET_F
       { incr l ; Load_store_register ((match c with |None -> Al |Some c -> c), true, false, rd, rn, rm) }
  |STR c = option(cond) rd = REGISTER COMMA CROCHET_O rn = REGISTER COMMA MINUS rm = REGISTER CROCHET_F
       { incr l ; Load_store_register ((match c with |None -> Al |Some c -> c), false, false, rd, rn, rm) }


  |MUL c = option(cond) s = boption(S) rd = REGISTER COMMA rm = REGISTER COMMA rs = REGISTER
       { incr l ; Mul ((match c with |None -> Al |Some c -> c), s, rd, rm, rs) }
  |MLA c = option(cond) s = boption(S) rd = REGISTER COMMA rm = REGISTER COMMA rs = REGISTER COMMA rn = REGISTER
       { incr l ; Mla ((match c with |None -> Al |Some c -> c), s, rd, rm, rs, rn) }

  |MRS c = option(cond) rd = REGISTER COMMA CPSR
       { incr l ; Mrs ((match c with |None -> Al |Some c -> c), false, rd) }
  |MRS c = option(cond) rd = REGISTER COMMA SPSR
       { incr l ; Mrs ((match c with |None -> Al |Some c -> c), true, rd) }
;

cond:
  |AL { Al }
  |EQ { Eq }
  |NE { Ne }
  |CS { Cs }
  |CC { Cc }
  |MI { Mi }
  |PL { Pl }
  |VS { Vs }
  |VC { Vc }
  |HI { Hi }
  |LS { Ls }
  |GE { Ge }
  |LT { Lt }
  |GT { Gt }
  |LE { Le }
; 

shifter:
  |n = IMM { Imm n }
  |rm = REGISTER { Reg rm }
  |rm = REGISTER COMMA LSL n = IMM { Lsl_by_i (rm, n) }
  |rm = REGISTER COMMA LSL rs = REGISTER { Lsl_by_r (rm, rs) }
  |rm = REGISTER COMMA LSR n = IMM { Lsr_by_i (rm, n) }
  |rm = REGISTER COMMA LSR rs = REGISTER { Lsr_by_r (rm, rs) }
  |rm = REGISTER COMMA ASR n = IMM { Asr_by_i (rm, n) }
  |rm = REGISTER COMMA ASR rs = REGISTER { Asr_by_r (rm, rs) }
  |rm = REGISTER COMMA ROR n = IMM { Ror_by_i (rm, n) }
  |rm = REGISTER COMMA ROR rs = REGISTER { Ror_by_r (rm, rs) }
;
