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

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState207
  | MenhirState206
  | MenhirState204
  | MenhirState199
  | MenhirState198
  | MenhirState196
  | MenhirState191
  | MenhirState190
  | MenhirState188
  | MenhirState183
  | MenhirState182
  | MenhirState179
  | MenhirState177
  | MenhirState172
  | MenhirState171
  | MenhirState168
  | MenhirState163
  | MenhirState161
  | MenhirState158
  | MenhirState156
  | MenhirState153
  | MenhirState150
  | MenhirState145
  | MenhirState144
  | MenhirState127
  | MenhirState118
  | MenhirState117
  | MenhirState115
  | MenhirState112
  | MenhirState111
  | MenhirState105
  | MenhirState98
  | MenhirState97
  | MenhirState95
  | MenhirState92
  | MenhirState91
  | MenhirState89
  | MenhirState84
  | MenhirState83
  | MenhirState81
  | MenhirState76
  | MenhirState75
  | MenhirState73
  | MenhirState68
  | MenhirState67
  | MenhirState65
  | MenhirState60
  | MenhirState59
  | MenhirState44
  | MenhirState27
  | MenhirState21
  | MenhirState20
  | MenhirState1
  | MenhirState0

  
  open Ast
  open Lexing
  exception Parsing_error of position*position*string

  let l = ref 0
let _eRR =
  Error

let rec _menhir_goto_shifter : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.shifter) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let shift = _v in
        let (((((_menhir_stack, _menhir_s), _, c), _, s), rd), rn) = _menhir_stack in
        let _v : (Ast.instr) =        ( incr l ; Sub ((match c with |None -> Al |Some c -> c), s, rn, rd, shift) ) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let shift = _v in
        let (((((_menhir_stack, _menhir_s), _, c), _, s), rd), rn) = _menhir_stack in
        let _v : (Ast.instr) =        ( incr l ; Sbc ((match c with |None -> Al |Some c -> c), s, rn, rd, shift) ) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let shift = _v in
        let (((((_menhir_stack, _menhir_s), _, c), _, s), rd), rn) = _menhir_stack in
        let _v : (Ast.instr) =        ( incr l ; Rsc ((match c with |None -> Al |Some c -> c), s, rn, rd, shift) ) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let shift = _v in
        let (((((_menhir_stack, _menhir_s), _, c), _, s), rd), rn) = _menhir_stack in
        let _v : (Ast.instr) =        ( incr l ; Rsb ((match c with |None -> Al |Some c -> c), s, rn, rd, shift) ) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let shift = _v in
        let (((((_menhir_stack, _menhir_s), _, c), _, s), rd), rn) = _menhir_stack in
        let _v : (Ast.instr) =        ( incr l ; Orr ((match c with |None -> Al |Some c -> c), s, rn, rd, shift) ) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let shift = _v in
        let ((((_menhir_stack, _menhir_s), _, c), _, s), rd) = _menhir_stack in
        let _v : (Ast.instr) =        ( incr l ; Mvn ((match c with |None -> Al |Some c -> c), s, rd, shift) ) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let shift = _v in
        let ((((_menhir_stack, _menhir_s), _, c), _, s), rd) = _menhir_stack in
        let _v : (Ast.instr) =        ( incr l ; Mov ((match c with |None -> Al |Some c -> c), s, rd, shift) ) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let shift = _v in
        let (((((_menhir_stack, _menhir_s), _, c), _, s), rd), rn) = _menhir_stack in
        let _v : (Ast.instr) =        ( incr l ; Eor ((match c with |None -> Al |Some c -> c), s, rn, rd, shift) ) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState156 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let shift = _v in
        let (((_menhir_stack, _menhir_s), _, c), rn) = _menhir_stack in
        let _v : (Ast.instr) =        ( incr l ; Cmp ((match c with |None -> Al |Some c -> c), rn, shift) ) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let shift = _v in
        let (((_menhir_stack, _menhir_s), _, c), rn) = _menhir_stack in
        let _v : (Ast.instr) =        ( incr l ; Cmn ((match c with |None -> Al |Some c -> c), rn, shift) ) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let shift = _v in
        let (((((_menhir_stack, _menhir_s), _, c), _, s), rd), rn) = _menhir_stack in
        let _v : (Ast.instr) =        ( incr l ; Bic ((match c with |None -> Al |Some c -> c), s, rn, rd, shift) ) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let shift = _v in
        let (((((_menhir_stack, _menhir_s), _, c), _, s), rd), rn) = _menhir_stack in
        let _v : (Ast.instr) =        ( incr l ; And ((match c with |None -> Al |Some c -> c), s, rn, rd, shift) ) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState196 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let shift = _v in
        let (((((_menhir_stack, _menhir_s), _, c), _, s), rd), rn) = _menhir_stack in
        let _v : (Ast.instr) =        ( incr l ; Add ((match c with |None -> Al |Some c -> c), s, rn, rd, shift) ) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState204 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let shift = _v in
        let (((((_menhir_stack, _menhir_s), _, c), _, s), rd), rn) = _menhir_stack in
        let _v : (Ast.instr) =        ( incr l ; Adc ((match c with |None -> Al |Some c -> c), s, rn, rd, shift) ) in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_boption_S_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (bool) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | REGISTER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | REGISTER _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | COMMA ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _tok = _menhir_discard _menhir_env in
                        (match _tok with
                        | IMM _v ->
                            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
                        | REGISTER _v ->
                            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
                        | _ ->
                            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                            _menhir_env._menhir_shifted <- (-1);
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27)
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | REGISTER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | REGISTER _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | COMMA ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _tok = _menhir_discard _menhir_env in
                        (match _tok with
                        | IMM _v ->
                            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
                        | REGISTER _v ->
                            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
                        | _ ->
                            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                            _menhir_env._menhir_shifted <- (-1);
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | REGISTER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | REGISTER _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | COMMA ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _tok = _menhir_discard _menhir_env in
                        (match _tok with
                        | IMM _v ->
                            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
                        | REGISTER _v ->
                            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
                        | _ ->
                            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                            _menhir_env._menhir_shifted <- (-1);
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | REGISTER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | REGISTER _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | COMMA ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _tok = _menhir_discard _menhir_env in
                        (match _tok with
                        | IMM _v ->
                            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
                        | REGISTER _v ->
                            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
                        | _ ->
                            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                            _menhir_env._menhir_shifted <- (-1);
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | REGISTER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | REGISTER _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | COMMA ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _tok = _menhir_discard _menhir_env in
                        (match _tok with
                        | IMM _v ->
                            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
                        | REGISTER _v ->
                            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
                        | _ ->
                            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                            _menhir_env._menhir_shifted <- (-1);
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | REGISTER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | IMM _v ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
                | REGISTER _v ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState95 _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | REGISTER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | REGISTER _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | COMMA ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _tok = _menhir_discard _menhir_env in
                        (match _tok with
                        | REGISTER _v ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _ = _menhir_discard _menhir_env in
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let rs = _v in
                            let (((((_menhir_stack, _menhir_s), _, c), _, s), rd), rm) = _menhir_stack in
                            let _v : (Ast.instr) =        ( incr l ; Mul ((match c with |None -> Al |Some c -> c), s, rd, rm, rs) ) in
                            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
                        | _ ->
                            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                            _menhir_env._menhir_shifted <- (-1);
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | REGISTER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | IMM _v ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
                | REGISTER _v ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | REGISTER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | REGISTER _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | COMMA ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _tok = _menhir_discard _menhir_env in
                        (match _tok with
                        | REGISTER _v ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_stack = (_menhir_stack, _v) in
                            let _tok = _menhir_discard _menhir_env in
                            (match _tok with
                            | COMMA ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _tok = _menhir_discard _menhir_env in
                                (match _tok with
                                | REGISTER _v ->
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let _ = _menhir_discard _menhir_env in
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let rn = _v in
                                    let ((((((_menhir_stack, _menhir_s), _, c), _, s), rd), rm), rs) = _menhir_stack in
                                    let _v : (Ast.instr) =        ( incr l ; Mla ((match c with |None -> Al |Some c -> c), s, rd, rm, rs, rn) ) in
                                    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
                                | _ ->
                                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                                    _menhir_env._menhir_shifted <- (-1);
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let ((((_menhir_stack, _menhir_s, _), _), _), _) = _menhir_stack in
                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                            | _ ->
                                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                                _menhir_env._menhir_shifted <- (-1);
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let ((((_menhir_stack, _menhir_s, _), _), _), _) = _menhir_stack in
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                        | _ ->
                            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                            _menhir_env._menhir_shifted <- (-1);
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | REGISTER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | REGISTER _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | COMMA ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _tok = _menhir_discard _menhir_env in
                        (match _tok with
                        | IMM _v ->
                            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
                        | REGISTER _v ->
                            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _v
                        | _ ->
                            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                            _menhir_env._menhir_shifted <- (-1);
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150)
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | REGISTER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | REGISTER _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | COMMA ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _tok = _menhir_discard _menhir_env in
                        (match _tok with
                        | IMM _v ->
                            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
                        | REGISTER _v ->
                            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
                        | _ ->
                            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                            _menhir_env._menhir_shifted <- (-1);
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState177)
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | REGISTER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | REGISTER _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | COMMA ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _tok = _menhir_discard _menhir_env in
                        (match _tok with
                        | IMM _v ->
                            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
                        | REGISTER _v ->
                            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
                        | _ ->
                            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                            _menhir_env._menhir_shifted <- (-1);
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState188)
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState191 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | REGISTER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | REGISTER _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | COMMA ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _tok = _menhir_discard _menhir_env in
                        (match _tok with
                        | IMM _v ->
                            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState196 _v
                        | REGISTER _v ->
                            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState196 _v
                        | _ ->
                            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                            _menhir_env._menhir_shifted <- (-1);
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState196)
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState199 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | REGISTER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | REGISTER _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | COMMA ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _tok = _menhir_discard _menhir_env in
                        (match _tok with
                        | IMM _v ->
                            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState204 _v
                        | REGISTER _v ->
                            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState204 _v
                        | _ ->
                            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                            _menhir_env._menhir_shifted <- (-1);
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState204)
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.register) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ASR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | IMM _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let n = _v in
                let (_menhir_stack, _menhir_s, rm) = _menhir_stack in
                let _v : (Ast.shifter) =                                    ( Asr_by_i (rm, n) ) in
                _menhir_goto_shifter _menhir_env _menhir_stack _menhir_s _v
            | REGISTER _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let rs = _v in
                let (_menhir_stack, _menhir_s, rm) = _menhir_stack in
                let _v : (Ast.shifter) =                                          ( Asr_by_r (rm, rs) ) in
                _menhir_goto_shifter _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LSL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | IMM _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let n = _v in
                let (_menhir_stack, _menhir_s, rm) = _menhir_stack in
                let _v : (Ast.shifter) =                                    ( Lsl_by_i (rm, n) ) in
                _menhir_goto_shifter _menhir_env _menhir_stack _menhir_s _v
            | REGISTER _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let rs = _v in
                let (_menhir_stack, _menhir_s, rm) = _menhir_stack in
                let _v : (Ast.shifter) =                                          ( Lsl_by_r (rm, rs) ) in
                _menhir_goto_shifter _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | LSR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | IMM _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let n = _v in
                let (_menhir_stack, _menhir_s, rm) = _menhir_stack in
                let _v : (Ast.shifter) =                                    ( Lsr_by_i (rm, n) ) in
                _menhir_goto_shifter _menhir_env _menhir_stack _menhir_s _v
            | REGISTER _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let rs = _v in
                let (_menhir_stack, _menhir_s, rm) = _menhir_stack in
                let _v : (Ast.shifter) =                                          ( Lsr_by_r (rm, rs) ) in
                _menhir_goto_shifter _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | ROR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | IMM _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let n = _v in
                let (_menhir_stack, _menhir_s, rm) = _menhir_stack in
                let _v : (Ast.shifter) =                                    ( Ror_by_i (rm, n) ) in
                _menhir_goto_shifter _menhir_env _menhir_stack _menhir_s _v
            | REGISTER _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let rs = _v in
                let (_menhir_stack, _menhir_s, rm) = _menhir_stack in
                let _v : (Ast.shifter) =                                          ( Ror_by_r (rm, rs) ) in
                _menhir_goto_shifter _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | ADC | ADD | AND | B | BIC | BL | CLZ | CMN | CMP | EOF | EOR | LABEL _ | LDR | MLA | MOV | MRS | MUL | MVN | ORR | RSB | RSC | SBC | STR | SUB | SWI ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, rm) = _menhir_stack in
        let _v : (Ast.shifter) =                  ( Reg rm ) in
        _menhir_goto_shifter _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let n = _v in
    let _v : (Ast.shifter) =            ( Imm n ) in
    _menhir_goto_shifter _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (bool) =     ( false ) in
    _menhir_goto_boption_S_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (bool) =     ( true ) in
    _menhir_goto_boption_S_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.instr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADC ->
        _menhir_run198 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | ADD ->
        _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | AND ->
        _menhir_run182 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | B ->
        _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | BIC ->
        _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | BL ->
        _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | CLZ ->
        _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | CMN ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | CMP ->
        _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | EOF ->
        _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | EOR ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | LABEL _v ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _v
    | LDR ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | MLA ->
        _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | MOV ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | MRS ->
        _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | MUL ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | MVN ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | ORR ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | RSB ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | RSC ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | SBC ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | STR ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | SUB ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | SWI ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState207
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState207

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_option_cond_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.cond option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IMM _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let n = _v in
            let ((_menhir_stack, _menhir_s), _, c) = _menhir_stack in
            let _v : (Ast.instr) =        ( incr l ; Swi ((match c with |None -> Al |Some c -> c), n) ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | S ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | REGISTER _ ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState21
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | REGISTER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | CROCHET_O ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | REGISTER _v ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = (_menhir_stack, _v) in
                        let _tok = _menhir_discard _menhir_env in
                        (match _tok with
                        | COMMA ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _tok = _menhir_discard _menhir_env in
                            (match _tok with
                            | IMM _v ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_stack = (_menhir_stack, _v) in
                                let _tok = _menhir_discard _menhir_env in
                                (match _tok with
                                | CROCHET_F ->
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let _ = _menhir_discard _menhir_env in
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let (((((_menhir_stack, _menhir_s), _, c), rd), rn), n) = _menhir_stack in
                                    let _v : (Ast.instr) =        ( incr l ; Load_store_offset ((match c with |None -> Al |Some c -> c), (n >= 0), false, rd, rn, if n >= 0 then n else -n) ) in
                                    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
                                | _ ->
                                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                                    _menhir_env._menhir_shifted <- (-1);
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let ((((_menhir_stack, _menhir_s, _), _), _), _) = _menhir_stack in
                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                            | MINUS ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _tok = _menhir_discard _menhir_env in
                                (match _tok with
                                | REGISTER _v ->
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let _menhir_stack = (_menhir_stack, _v) in
                                    let _tok = _menhir_discard _menhir_env in
                                    (match _tok with
                                    | CROCHET_F ->
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let _ = _menhir_discard _menhir_env in
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let (((((_menhir_stack, _menhir_s), _, c), rd), rn), rm) = _menhir_stack in
                                        let _v : (Ast.instr) =        ( incr l ; Load_store_register ((match c with |None -> Al |Some c -> c), false, false, rd, rn, rm) ) in
                                        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
                                    | _ ->
                                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                                        _menhir_env._menhir_shifted <- (-1);
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let ((((_menhir_stack, _menhir_s, _), _), _), _) = _menhir_stack in
                                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                | _ ->
                                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                                    _menhir_env._menhir_shifted <- (-1);
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                            | PLUS ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _tok = _menhir_discard _menhir_env in
                                (match _tok with
                                | REGISTER _v ->
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let _menhir_stack = (_menhir_stack, _v) in
                                    let _tok = _menhir_discard _menhir_env in
                                    (match _tok with
                                    | CROCHET_F ->
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let _ = _menhir_discard _menhir_env in
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let (((((_menhir_stack, _menhir_s), _, c), rd), rn), rm) = _menhir_stack in
                                        let _v : (Ast.instr) =        ( incr l ; Load_store_register ((match c with |None -> Al |Some c -> c), true, false, rd, rn, rm) ) in
                                        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
                                    | _ ->
                                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                                        _menhir_env._menhir_shifted <- (-1);
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let ((((_menhir_stack, _menhir_s, _), _), _), _) = _menhir_stack in
                                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                | _ ->
                                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                                    _menhir_env._menhir_shifted <- (-1);
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                            | _ ->
                                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                                _menhir_env._menhir_shifted <- (-1);
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                        | _ ->
                            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                            _menhir_env._menhir_shifted <- (-1);
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | S ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | REGISTER _ ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | S ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | REGISTER _ ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | S ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | REGISTER _ ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | S ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | REGISTER _ ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | S ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | REGISTER _ ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | S ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | REGISTER _ ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | REGISTER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | CPSR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _ = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), _, c), rd) = _menhir_stack in
                    let _v : (Ast.instr) =        ( incr l ; Mrs ((match c with |None -> Al |Some c -> c), false, rd) ) in
                    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
                | SPSR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _ = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), _, c), rd) = _menhir_stack in
                    let _v : (Ast.instr) =        ( incr l ; Mrs ((match c with |None -> Al |Some c -> c), true, rd) ) in
                    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | S ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | REGISTER _ ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | S ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | REGISTER _ ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState118
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | REGISTER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | CROCHET_O ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | REGISTER _v ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = (_menhir_stack, _v) in
                        let _tok = _menhir_discard _menhir_env in
                        (match _tok with
                        | COMMA ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _tok = _menhir_discard _menhir_env in
                            (match _tok with
                            | IMM _v ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_stack = (_menhir_stack, _v) in
                                let _tok = _menhir_discard _menhir_env in
                                (match _tok with
                                | CROCHET_F ->
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let _ = _menhir_discard _menhir_env in
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let (((((_menhir_stack, _menhir_s), _, c), rd), rn), n) = _menhir_stack in
                                    let _v : (Ast.instr) =        ( incr l ; Load_store_offset ((match c with |None -> Al |Some c -> c), (n >= 0), true, rd, rn, if n >= 0 then n else -n) ) in
                                    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
                                | _ ->
                                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                                    _menhir_env._menhir_shifted <- (-1);
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let ((((_menhir_stack, _menhir_s, _), _), _), _) = _menhir_stack in
                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                            | MINUS ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _tok = _menhir_discard _menhir_env in
                                (match _tok with
                                | REGISTER _v ->
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let _menhir_stack = (_menhir_stack, _v) in
                                    let _tok = _menhir_discard _menhir_env in
                                    (match _tok with
                                    | CROCHET_F ->
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let _ = _menhir_discard _menhir_env in
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let (((((_menhir_stack, _menhir_s), _, c), rd), rn), rm) = _menhir_stack in
                                        let _v : (Ast.instr) =        ( incr l ; Load_store_register ((match c with |None -> Al |Some c -> c), false, true, rd, rn, rm) ) in
                                        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
                                    | _ ->
                                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                                        _menhir_env._menhir_shifted <- (-1);
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let ((((_menhir_stack, _menhir_s, _), _), _), _) = _menhir_stack in
                                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                | _ ->
                                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                                    _menhir_env._menhir_shifted <- (-1);
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                            | PLUS ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _tok = _menhir_discard _menhir_env in
                                (match _tok with
                                | REGISTER _v ->
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let _menhir_stack = (_menhir_stack, _v) in
                                    let _tok = _menhir_discard _menhir_env in
                                    (match _tok with
                                    | CROCHET_F ->
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let _ = _menhir_discard _menhir_env in
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let (((((_menhir_stack, _menhir_s), _, c), rd), rn), rm) = _menhir_stack in
                                        let _v : (Ast.instr) =        ( incr l ; Load_store_register ((match c with |None -> Al |Some c -> c), true, true, rd, rn, rm) ) in
                                        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
                                    | _ ->
                                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                                        _menhir_env._menhir_shifted <- (-1);
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let ((((_menhir_stack, _menhir_s, _), _), _), _) = _menhir_stack in
                                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                                | _ ->
                                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                                    _menhir_env._menhir_shifted <- (-1);
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                            | _ ->
                                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                                _menhir_env._menhir_shifted <- (-1);
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                        | _ ->
                            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                            _menhir_env._menhir_shifted <- (-1);
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | S ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | REGISTER _ ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState145
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | REGISTER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | IMM _v ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
                | REGISTER _v ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState156)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | REGISTER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | IMM _v ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
                | REGISTER _v ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | REGISTER _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | REGISTER _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _ = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let rm = _v in
                    let (((_menhir_stack, _menhir_s), _, c), rd) = _menhir_stack in
                    let _v : (Ast.instr) =        ( incr l ; Clz ((match c with |None -> Al |Some c -> c), rd, rm) ) in
                    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LABEL _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let lab = _v in
            let ((_menhir_stack, _menhir_s), _, c) = _menhir_stack in
            let _v : (Ast.instr) =        ( incr l ; Branch ((match c with |None -> Al |Some c -> c), true, lab) ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState171 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | S ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | REGISTER _ ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState172
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState172)
    | MenhirState179 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LABEL _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let lab = _v in
            let ((_menhir_stack, _menhir_s), _, c) = _menhir_stack in
            let _v : (Ast.instr) =        ( incr l ; Branch ((match c with |None -> Al |Some c -> c), false, lab) ) in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState182 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | S ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | REGISTER _ ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState183
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState183)
    | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | S ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | REGISTER _ ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState191)
    | MenhirState198 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | S ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | REGISTER _ ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack) MenhirState199
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState199)
    | _ ->
        _menhir_fail ()

and _menhir_goto_cond : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.cond) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = _v in
    let _v : (Ast.cond option) =     ( Some x ) in
    _menhir_goto_option_cond_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_file : _menhir_env -> 'ttv_tail -> _menhir_state -> (((Ast.label*int) list) * Ast.instr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState207 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let f = _v in
        let (_menhir_stack, _menhir_s, i) = _menhir_stack in
        let _v : (((Ast.label*int) list) * Ast.instr list) =    ( let (l1, l2) = f in (l1, i::l2) ) in
        _menhir_goto_file _menhir_env _menhir_stack _menhir_s _v
    | MenhirState206 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let f = _v in
        let (_menhir_stack, _menhir_s, s) = _menhir_stack in
        let _v : (((Ast.label*int) list) * Ast.instr list) =    ( let (l1, l2) = f in (s::l1, l2) ) in
        _menhir_goto_file _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        Obj.magic _1
    | _ ->
        _menhir_fail ()

and _menhir_reduce50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.cond option) =     ( None ) in
    _menhir_goto_option_cond_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.cond) =       ( Vs ) in
    _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.cond) =       ( Vc ) in
    _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.cond) =       ( Pl ) in
    _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.cond) =       ( Ne ) in
    _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.cond) =       ( Mi ) in
    _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.cond) =       ( Lt ) in
    _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.cond) =       ( Ls ) in
    _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.cond) =       ( Le ) in
    _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.cond) =       ( Hi ) in
    _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.cond) =       ( Gt ) in
    _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.cond) =       ( Ge ) in
    _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.cond) =       ( Eq ) in
    _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.cond) =       ( Cs ) in
    _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.cond) =       ( Cc ) in
    _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.cond) =       ( Al ) in
    _menhir_goto_cond _menhir_env _menhir_stack _menhir_s _v

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState207 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState206 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState204 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState199 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState198 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState196 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState191 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState183 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState182 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState179 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState172 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState171 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState156 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState150 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | CC ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | CS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | EQ ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | GE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | GT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | HI ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | MI ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | NE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | PL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | VC ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | VS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | IMM _ ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | CC ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | CS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | EQ ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | GE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | GT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | HI ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | LE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | LS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | LT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | MI ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | NE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | PL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | VC ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | VS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | REGISTER _ | S ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | CC ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | CS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | EQ ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | GE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | GT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | HI ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | LE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | LS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | LT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | MI ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | NE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | PL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | VC ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | VS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | REGISTER _ ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | CC ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | CS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | EQ ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | GE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | GT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | HI ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | MI ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | PL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | VC ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | VS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | REGISTER _ | S ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | CC ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | CS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | EQ ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | GE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | GT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | HI ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | MI ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | PL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | VC ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | VS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | REGISTER _ | S ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | CC ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | CS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | EQ ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | GE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | GT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | HI ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | LE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | LS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | LT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | MI ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | NE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | PL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | VC ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | VS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | REGISTER _ | S ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75

and _menhir_run83 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | CC ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | CS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | EQ ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | GE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | GT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | HI ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | LE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | LS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | LT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | MI ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | NE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | PL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | VC ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | VS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | REGISTER _ | S ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run91 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | CC ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | CS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | EQ ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | GE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | GT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | HI ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | LE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | LS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | LT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | MI ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | NE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | PL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | VC ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | VS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | REGISTER _ | S ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91

and _menhir_run97 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | CC ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | CS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | EQ ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | GE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | GT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | HI ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | LE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | LS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | LT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | MI ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | NE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | PL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | VC ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | VS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | REGISTER _ | S ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState97
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97

and _menhir_run105 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | CC ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | CS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | EQ ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | GE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | GT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | HI ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | LE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | LS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | LT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | MI ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | NE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | PL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | VC ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | VS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | REGISTER _ ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105

and _menhir_run111 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | CC ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | CS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | EQ ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | GE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | GT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | HI ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | LE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | LS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | LT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | MI ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | NE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | PL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | VC ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | VS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | REGISTER _ | S ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState111
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111

and _menhir_run117 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | CC ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | CS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | EQ ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | GE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | GT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | HI ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | LE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | LS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | LT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | MI ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | NE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | PL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | VC ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | VS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | REGISTER _ | S ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState117
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117

and _menhir_run127 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | CC ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | CS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | EQ ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | GE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | GT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | HI ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | LE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | LS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | LT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | MI ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | NE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | PL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | VC ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | VS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | REGISTER _ ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127

and _menhir_run142 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.label) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | COLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, s) = _menhir_stack in
        let _v : (Ast.label * int) =    ( (s, !l) ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADC ->
            _menhir_run198 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | ADD ->
            _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | AND ->
            _menhir_run182 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | B ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | BIC ->
            _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | BL ->
            _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | CLZ ->
            _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | CMN ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | CMP ->
            _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | EOF ->
            _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | EOR ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | LABEL _v ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState206 _v
        | LDR ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | MLA ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | MOV ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | MRS ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | MUL ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | MVN ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | ORR ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | RSB ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | RSC ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | SBC ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | STR ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | SUB ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | SWI ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState206
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState206)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run144 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | CC ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | CS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | EQ ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | GE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | GT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | HI ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | LE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | LS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | LT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | MI ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | NE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | PL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | VC ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | VS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | REGISTER _ | S ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState144
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144

and _menhir_run152 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (((Ast.label*int) list) * Ast.instr list) =    ( ([], []) ) in
    _menhir_goto_file _menhir_env _menhir_stack _menhir_s _v

and _menhir_run153 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | CC ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | CS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | EQ ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | GE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | GT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | HI ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | LE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | LS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | LT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | MI ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | NE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | PL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | VC ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | VS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | REGISTER _ ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState153
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153

and _menhir_run158 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | CC ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | CS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | EQ ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | GE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | GT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | HI ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | LE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | LS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | LT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | MI ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | NE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | PL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | VC ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | VS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | REGISTER _ ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158

and _menhir_run163 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | CC ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | CS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | EQ ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | GE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | GT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | HI ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | LE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | LS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | LT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | MI ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | NE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | PL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | VC ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | VS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | REGISTER _ ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState163
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163

and _menhir_run168 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | CC ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | CS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | EQ ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | GE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | GT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | HI ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | LE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | LS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | LT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | MI ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | NE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | PL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | VC ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | VS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | LABEL _ ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState168
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168

and _menhir_run171 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | CC ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | CS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | EQ ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | GE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | GT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | HI ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | LE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | LS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | LT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | MI ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | NE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | PL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | VC ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | VS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | REGISTER _ | S ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState171
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState171

and _menhir_run179 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | CC ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | CS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | EQ ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | GE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | GT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | HI ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | LE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | LS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | LT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | MI ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | NE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | PL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | VC ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | VS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | LABEL _ ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState179
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState179

and _menhir_run182 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | CC ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | CS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | EQ ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | GE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | GT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | HI ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | LE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | LS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | LT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | MI ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | NE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | PL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | VC ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | VS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | REGISTER _ | S ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState182
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState182

and _menhir_run190 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | CC ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | CS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | EQ ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | GE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | GT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | HI ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | LE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | LS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | LT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | MI ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | NE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | PL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | VC ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | VS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | REGISTER _ | S ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState190

and _menhir_run198 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AL ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | CC ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | CS ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | EQ ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | GE ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | GT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | HI ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | LE ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | LS ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | LT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | MI ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | NE ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | PL ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | VC ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | VS ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | REGISTER _ | S ->
        _menhir_reduce50 _menhir_env (Obj.magic _menhir_stack) MenhirState198
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState198

and file : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (((Ast.label*int) list) * Ast.instr list) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_startp = lexbuf.Lexing.lex_start_p;
      _menhir_endp = lexbuf.Lexing.lex_curr_p;
      _menhir_shifted = max_int;
      } in
    Obj.magic (let _menhir_stack = () in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADC ->
        _menhir_run198 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ADD ->
        _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | AND ->
        _menhir_run182 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | B ->
        _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | BIC ->
        _menhir_run171 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | BL ->
        _menhir_run168 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | CLZ ->
        _menhir_run163 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | CMN ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | CMP ->
        _menhir_run153 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_run152 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOR ->
        _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LABEL _v ->
        _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LDR ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | MLA ->
        _menhir_run117 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | MOV ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | MRS ->
        _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | MUL ->
        _menhir_run97 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | MVN ->
        _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ORR ->
        _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | RSB ->
        _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | RSC ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SBC ->
        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | STR ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SUB ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SWI ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)



