
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | TOK_id of (
# 32 "frontend/Parser.mly"
       (string)
# 11 "frontend/Parser.ml"
  )
    | TOK_const of (
# 68 "frontend/Parser.mly"
       (string)
# 16 "frontend/Parser.ml"
  )
    | TOK_WHILE
    | TOK_VOID
    | TOK_URANDOM
    | TOK_UINT
    | TOK_TRUE
    | TOK_SEMICOLON
    | TOK_RPAREN
    | TOK_RETURN
    | TOK_RCURLY
    | TOK_RBRACKET
    | TOK_RANDOM
    | TOK_PLUS_PLUS
    | TOK_PLUS_EQUAL
    | TOK_PLUS
    | TOK_OR
    | TOK_NOT_EQUAL
    | TOK_NOT
    | TOK_MULTIPLY_EQUAL
    | TOK_MULTIPLY
    | TOK_MODULO_EQUAL
    | TOK_MODULO
    | TOK_MINUS_MINUS
    | TOK_MINUS_EQUAL
    | TOK_MINUS
    | TOK_LPAREN
    | TOK_LESS_EQUAL
    | TOK_LESS
    | TOK_LCURLY
    | TOK_LBRACKET
    | TOK_INT
    | TOK_IF
    | TOK_GREATER_EQUAL
    | TOK_GREATER
    | TOK_FOR
    | TOK_FALSE
    | TOK_EQUAL_EQUAL
    | TOK_EQUAL
    | TOK_EOF
    | TOK_ELSE
    | TOK_DIVIDE_EQUAL
    | TOK_DIVIDE
    | TOK_CPP_IFNDEF
    | TOK_CPP_IFDEF
    | TOK_CPP_IF
    | TOK_CPP_ENDIF
    | TOK_CPP_ELSE
    | TOK_CPP_DEFINE
    | TOK_COMMA
    | TOK_COLON
    | TOK_ASSERT
    | TOK_AND
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState224
  | MenhirState220
  | MenhirState218
  | MenhirState216
  | MenhirState213
  | MenhirState206
  | MenhirState205
  | MenhirState202
  | MenhirState198
  | MenhirState196
  | MenhirState192
  | MenhirState187
  | MenhirState184
  | MenhirState182
  | MenhirState180
  | MenhirState170
  | MenhirState155
  | MenhirState153
  | MenhirState151
  | MenhirState149
  | MenhirState147
  | MenhirState145
  | MenhirState141
  | MenhirState138
  | MenhirState133
  | MenhirState132
  | MenhirState130
  | MenhirState128
  | MenhirState126
  | MenhirState121
  | MenhirState119
  | MenhirState111
  | MenhirState105
  | MenhirState100
  | MenhirState97
  | MenhirState94
  | MenhirState91
  | MenhirState89
  | MenhirState87
  | MenhirState84
  | MenhirState81
  | MenhirState78
  | MenhirState76
  | MenhirState74
  | MenhirState71
  | MenhirState69
  | MenhirState51
  | MenhirState43
  | MenhirState38
  | MenhirState34
  | MenhirState33
  | MenhirState31
  | MenhirState28
  | MenhirState26
  | MenhirState21
  | MenhirState17
  | MenhirState16
  | MenhirState13
  | MenhirState10
  | MenhirState4
  | MenhirState1
  | MenhirState0

# 9 "frontend/Parser.mly"
  
open IntermediateSyntax

# 152 "frontend/Parser.ml"

let rec _menhir_goto_separated_nonempty_list_TOK_COMMA_annotate_exp__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (IntermediateSyntax.exp IntermediateSyntax.annotated list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (IntermediateSyntax.exp IntermediateSyntax.annotated list)) = _v in
        let _v : (IntermediateSyntax.exp IntermediateSyntax.annotated list) = 
# 130 "/usr/share/menhir/standard.mly"
    ( x )
# 164 "frontend/Parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_TOK_COMMA_annotate_exp___ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (IntermediateSyntax.exp IntermediateSyntax.annotated list)) = _v in
        let (_menhir_stack, _endpos_x_, _menhir_s, (x : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_x_) = _menhir_stack in
        let _2 = () in
        let _v : (IntermediateSyntax.exp IntermediateSyntax.annotated list) = 
# 217 "/usr/share/menhir/standard.mly"
    ( x :: xs )
# 176 "frontend/Parser.ml"
         in
        _menhir_goto_separated_nonempty_list_TOK_COMMA_annotate_exp__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run97 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (IntermediateSyntax.exp IntermediateSyntax.annotated) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS_MINUS ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MULTIPLY ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_NOT ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS_PLUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RANDOM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_URANDOM ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState97 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_const _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState97 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState97 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (IntermediateSyntax.exp) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState206 | MenhirState192 | MenhirState17 | MenhirState119 | MenhirState187 | MenhirState128 | MenhirState184 | MenhirState182 | MenhirState180 | MenhirState149 | MenhirState170 | MenhirState153 | MenhirState155 | MenhirState151 | MenhirState147 | MenhirState145 | MenhirState141 | MenhirState130 | MenhirState126 | MenhirState121 | MenhirState21 | MenhirState111 | MenhirState69 | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _endpos_x_, _menhir_s, (x : (IntermediateSyntax.exp)), _startpos_x_) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : (IntermediateSyntax.exp IntermediateSyntax.annotated) = let _endpos = _endpos_x_ in
        let _startpos = _startpos_x_ in
        
# 228 "frontend/Parser.mly"
          ( x, (_startpos, _endpos) )
# 235 "frontend/Parser.ml"
         in
        let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
        (match _menhir_s with
        | MenhirState69 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e1_), (o : (IntermediateSyntax.assignOp))), _endpos_e2_, _, (e2 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e2_) = _menhir_stack in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (IntermediateSyntax.exp) = 
# 158 "frontend/Parser.mly"
                                                             ( I_assign (e1,o,e2) )
# 248 "frontend/Parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | MenhirState111 | MenhirState51 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TOK_COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | TOK_FALSE ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LBRACKET ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LPAREN ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MINUS ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MINUS_MINUS ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MULTIPLY ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_NOT ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_PLUS ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_PLUS_PLUS ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_RANDOM ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_TRUE ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_URANDOM ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_const _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_id _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
            | TOK_RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _endpos_x_, _menhir_s, (x : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_x_) = _menhir_stack in
                let _v : (IntermediateSyntax.exp IntermediateSyntax.annotated list) = 
# 215 "/usr/share/menhir/standard.mly"
    ( [ x ] )
# 299 "frontend/Parser.ml"
                 in
                _menhir_goto_separated_nonempty_list_TOK_COMMA_annotate_exp__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState21 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TOK_RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _menhir_stack = (_menhir_stack, _endpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | TOK_ASSERT ->
                    _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_CPP_IF ->
                    _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_FALSE ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_FOR ->
                    _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_IF ->
                    _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_INT ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LBRACKET ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LCURLY ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LPAREN ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MINUS ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MINUS_MINUS ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MULTIPLY ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_NOT ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_PLUS ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_PLUS_PLUS ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_RANDOM ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_RETURN ->
                    _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_SEMICOLON ->
                    _menhir_run120 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_TRUE ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_UINT ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_URANDOM ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_WHILE ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_const _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_id _v ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState121 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _endpos_x_, _menhir_s, (x : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_x_) = _menhir_stack in
            let _v : (IntermediateSyntax.exp IntermediateSyntax.annotated option) = 
# 102 "/usr/share/menhir/standard.mly"
    ( Some x )
# 385 "frontend/Parser.ml"
             in
            _menhir_goto_option_annotate_exp__ _menhir_env _menhir_stack _menhir_s _v
        | MenhirState126 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TOK_RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _menhir_stack = (_menhir_stack, _endpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | TOK_ASSERT ->
                    _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_CPP_IF ->
                    _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_FALSE ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_FOR ->
                    _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_IF ->
                    _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_INT ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LBRACKET ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LCURLY ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LPAREN ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MINUS ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MINUS_MINUS ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MULTIPLY ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_NOT ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_PLUS ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_PLUS_PLUS ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_RANDOM ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_RETURN ->
                    _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_SEMICOLON ->
                    _menhir_run120 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_TRUE ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_UINT ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_URANDOM ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_WHILE ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_const _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState128 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_id _v ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState128 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState141 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (d : (string IntermediateSyntax.annotated)), _startpos_d_), _endpos_e_, _, (e : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e_) = _menhir_stack in
            let _2 = () in
            let _v : (IntermediateSyntax.declarator) = 
# 95 "frontend/Parser.mly"
                                                      ( d, Some e )
# 466 "frontend/Parser.ml"
             in
            _menhir_goto_declarator _menhir_env _menhir_stack _menhir_s _v
        | MenhirState145 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TOK_SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                let _menhir_stack = (_menhir_stack, _endpos, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | TOK_FALSE ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LBRACKET ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LPAREN ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MINUS ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MINUS_MINUS ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MULTIPLY ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_NOT ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_PLUS ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_PLUS_PLUS ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_RANDOM ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_TRUE ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_URANDOM ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_const _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState147 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_id _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState147 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState147 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TOK_RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _menhir_stack = (_menhir_stack, _endpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | TOK_ASSERT ->
                    _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_CPP_IF ->
                    _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_FALSE ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_FOR ->
                    _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_IF ->
                    _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_INT ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LBRACKET ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LCURLY ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LPAREN ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MINUS ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MINUS_MINUS ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MULTIPLY ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_NOT ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_PLUS ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_PLUS_PLUS ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_RANDOM ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_RETURN ->
                    _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_SEMICOLON ->
                    _menhir_run120 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_TRUE ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_UINT ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_URANDOM ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_WHILE ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_const _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState149 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_id _v ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState149 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState151 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TOK_RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _menhir_stack = (_menhir_stack, _endpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | TOK_ASSERT ->
                    _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_CPP_IF ->
                    _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_FALSE ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_FOR ->
                    _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_IF ->
                    _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_INT ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LBRACKET ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LCURLY ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LPAREN ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MINUS ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MINUS_MINUS ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MULTIPLY ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_NOT ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_PLUS ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_PLUS_PLUS ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_RANDOM ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_RETURN ->
                    _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_SEMICOLON ->
                    _menhir_run120 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_TRUE ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_UINT ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_URANDOM ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_WHILE ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_const _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState153 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_id _v ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState153 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState155 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TOK_RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _menhir_stack = (_menhir_stack, _endpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | TOK_SEMICOLON ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _endpos__5_ = _endpos in
                    let _startpos__5_ = _startpos in
                    let ((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_e_, _, (e : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e_), _endpos__4_) = _menhir_stack in
                    let _5 = () in
                    let _4 = () in
                    let _2 = () in
                    let _1 = () in
                    let _startpos = _startpos__1_ in
                    let _endpos = _endpos__5_ in
                    let _v : (IntermediateSyntax.stmt) = 
# 132 "frontend/Parser.mly"
                                                                  ( I_assert e )
# 690 "frontend/Parser.ml"
                     in
                    _menhir_goto_exp_stmt _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState17 | MenhirState192 | MenhirState119 | MenhirState128 | MenhirState187 | MenhirState184 | MenhirState149 | MenhirState153 | MenhirState170 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TOK_SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos__2_ = _endpos in
                let _startpos__2_ = _startpos in
                let (_menhir_stack, _endpos_e_, _menhir_s, (e : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e_) = _menhir_stack in
                let _2 = () in
                let _startpos = _startpos_e_ in
                let _endpos = _endpos__2_ in
                let _v : (IntermediateSyntax.stmt) = 
# 131 "frontend/Parser.mly"
                                           ( I_exp e )
# 725 "frontend/Parser.ml"
                 in
                _menhir_goto_exp_stmt _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState130 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TOK_SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                let _menhir_stack = (_menhir_stack, _endpos, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | TOK_FALSE ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LBRACKET ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LPAREN ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MINUS ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MINUS_MINUS ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MULTIPLY ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_NOT ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_PLUS ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_PLUS_PLUS ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_RANDOM ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_TRUE ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_URANDOM ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_const _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState180 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_id _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState180 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState180)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState180 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TOK_SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                let _menhir_stack = (_menhir_stack, _endpos, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | TOK_FALSE ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LBRACKET ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LPAREN ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MINUS ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MINUS_MINUS ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MULTIPLY ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_NOT ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_PLUS ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_PLUS_PLUS ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_RANDOM ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_TRUE ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_URANDOM ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState182 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_const _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState182 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_id _v ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState182 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState182)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState182 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TOK_RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _menhir_stack = (_menhir_stack, _endpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | TOK_ASSERT ->
                    _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_CPP_IF ->
                    _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_FALSE ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_FOR ->
                    _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_IF ->
                    _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_INT ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LBRACKET ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LCURLY ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_LPAREN ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MINUS ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MINUS_MINUS ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_MULTIPLY ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_NOT ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_PLUS ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_PLUS_PLUS ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_RANDOM ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_RETURN ->
                    _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_SEMICOLON ->
                    _menhir_run120 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_TRUE ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_UINT ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_URANDOM ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_WHILE ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState184 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_const _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState184 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_id _v ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState184 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState184)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState206 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _startpos__1_), _, (t : (string IntermediateSyntax.annotated)), _startpos_t_), _endpos_e_, _, (e : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e_ in
            let _v : (IntermediateSyntax.globalDefine) = 
# 98 "frontend/Parser.mly"
                                                             ( t, e )
# 916 "frontend/Parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos_x_ = _endpos in
            let (x : (IntermediateSyntax.globalDefine)) = _v in
            let _startpos_x_ = _startpos in
            let _startpos = _startpos_x_ in
            let _endpos = _endpos_x_ in
            let _v : (IntermediateSyntax.globalDefine IntermediateSyntax.annotated) = let _endpos = _endpos_x_ in
            let _startpos = _startpos_x_ in
            
# 228 "frontend/Parser.mly"
          ( x, (_startpos, _endpos) )
# 930 "frontend/Parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos_d_ = _endpos in
            let (d : (IntermediateSyntax.globalDefine IntermediateSyntax.annotated)) = _v in
            let _startpos_d_ = _startpos in
            let _startpos = _startpos_d_ in
            let _endpos = _endpos_d_ in
            let _v : (IntermediateSyntax.decl) = 
# 80 "frontend/Parser.mly"
                                ( I_define d )
# 942 "frontend/Parser.ml"
             in
            _menhir_goto_decl _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            _menhir_fail ())
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : (IntermediateSyntax.exp)), _startpos_e_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (IntermediateSyntax.exp) = 
# 225 "frontend/Parser.mly"
                                                     ( e )
# 966 "frontend/Parser.ml"
             in
            _menhir_goto_primary_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run100 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (IntermediateSyntax.exp IntermediateSyntax.annotated) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS_MINUS ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MULTIPLY ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_NOT ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS_PLUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RANDOM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_URANDOM ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_const _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState100 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100

and _menhir_run105 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (IntermediateSyntax.exp IntermediateSyntax.annotated) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS_MINUS ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MULTIPLY ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_NOT ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS_PLUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RANDOM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_URANDOM ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_const _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105

and _menhir_reduce14 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (IntermediateSyntax.exp) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _endpos_x_, _menhir_s, (x : (IntermediateSyntax.exp)), _startpos_x_) = _menhir_stack in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (IntermediateSyntax.exp IntermediateSyntax.annotated) = let _endpos = _endpos_x_ in
    let _startpos = _startpos_x_ in
    
# 228 "frontend/Parser.mly"
          ( x, (_startpos, _endpos) )
# 1064 "frontend/Parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_AND ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | TOK_COMMA | TOK_CPP_DEFINE | TOK_EOF | TOK_INT | TOK_OR | TOK_RPAREN | TOK_SEMICOLON | TOK_UINT | TOK_VOID ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e1_), _endpos_e2_, _, (e2 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (IntermediateSyntax.exp) = 
# 170 "frontend/Parser.mly"
                                                                       ( I_or (e1,e2) )
# 1084 "frontend/Parser.ml"
             in
            _menhir_goto_logical_or_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState206 | MenhirState17 | MenhirState192 | MenhirState119 | MenhirState128 | MenhirState187 | MenhirState184 | MenhirState182 | MenhirState180 | MenhirState149 | MenhirState153 | MenhirState170 | MenhirState155 | MenhirState151 | MenhirState147 | MenhirState145 | MenhirState141 | MenhirState130 | MenhirState126 | MenhirState121 | MenhirState21 | MenhirState33 | MenhirState111 | MenhirState51 | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_AND ->
            _menhir_run97 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_logical_or_exp : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (IntermediateSyntax.exp) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_COMMA | TOK_CPP_DEFINE | TOK_EOF | TOK_INT | TOK_RPAREN | TOK_SEMICOLON | TOK_UINT | TOK_VOID ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _endpos_e_, _menhir_s, (e : (IntermediateSyntax.exp)), _startpos_e_) = _menhir_stack in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : (IntermediateSyntax.exp) = 
# 157 "frontend/Parser.mly"
                               ( e )
# 1124 "frontend/Parser.ml"
         in
        _menhir_goto_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | TOK_OR ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _endpos_x_, _menhir_s, (x : (IntermediateSyntax.exp)), _startpos_x_) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _v : (IntermediateSyntax.exp IntermediateSyntax.annotated) = let _endpos = _endpos_x_ in
        let _startpos = _startpos_x_ in
        
# 228 "frontend/Parser.mly"
          ( x, (_startpos, _endpos) )
# 1136 "frontend/Parser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_OR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TOK_FALSE ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LBRACKET ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MINUS_MINUS ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MULTIPLY ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_NOT ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_PLUS ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_PLUS_PLUS ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RANDOM ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_TRUE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_URANDOM ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_const _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_id _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState94 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run71 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (IntermediateSyntax.exp IntermediateSyntax.annotated) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS_MINUS ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MULTIPLY ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_NOT ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS_PLUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RANDOM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_URANDOM ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_const _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState71 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_run87 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (IntermediateSyntax.exp IntermediateSyntax.annotated) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS_MINUS ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MULTIPLY ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_NOT ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS_PLUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RANDOM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_URANDOM ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_const _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_run89 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (IntermediateSyntax.exp IntermediateSyntax.annotated) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS_MINUS ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MULTIPLY ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_NOT ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS_PLUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RANDOM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_URANDOM ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_const _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89

and _menhir_run91 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (IntermediateSyntax.exp IntermediateSyntax.annotated) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS_MINUS ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MULTIPLY ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_NOT ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS_PLUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RANDOM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_URANDOM ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState91 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_const _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState91 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState91 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91

and _menhir_reduce6 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (IntermediateSyntax.exp) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _endpos_x_, _menhir_s, (x : (IntermediateSyntax.exp)), _startpos_x_) = _menhir_stack in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (IntermediateSyntax.exp IntermediateSyntax.annotated) = let _endpos = _endpos_x_ in
    let _startpos = _startpos_x_ in
    
# 228 "frontend/Parser.mly"
          ( x, (_startpos, _endpos) )
# 1355 "frontend/Parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_EQUAL_EQUAL ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | TOK_NOT_EQUAL ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | TOK_AND | TOK_COMMA | TOK_CPP_DEFINE | TOK_EOF | TOK_INT | TOK_OR | TOK_RPAREN | TOK_SEMICOLON | TOK_UINT | TOK_VOID ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e1_), _endpos_e2_, _, (e2 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (IntermediateSyntax.exp) = 
# 174 "frontend/Parser.mly"
                                                                      ( I_and (e1,e2) )
# 1377 "frontend/Parser.ml"
             in
            _menhir_goto_logical_and_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState206 | MenhirState17 | MenhirState192 | MenhirState21 | MenhirState119 | MenhirState126 | MenhirState128 | MenhirState187 | MenhirState130 | MenhirState180 | MenhirState182 | MenhirState184 | MenhirState145 | MenhirState147 | MenhirState149 | MenhirState151 | MenhirState153 | MenhirState170 | MenhirState155 | MenhirState141 | MenhirState121 | MenhirState33 | MenhirState51 | MenhirState111 | MenhirState69 | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_EQUAL_EQUAL ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack)
        | TOK_NOT_EQUAL ->
            _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_logical_and_exp : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (IntermediateSyntax.exp) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState206 | MenhirState192 | MenhirState17 | MenhirState119 | MenhirState187 | MenhirState128 | MenhirState184 | MenhirState182 | MenhirState180 | MenhirState149 | MenhirState170 | MenhirState153 | MenhirState155 | MenhirState151 | MenhirState147 | MenhirState145 | MenhirState141 | MenhirState130 | MenhirState126 | MenhirState121 | MenhirState21 | MenhirState33 | MenhirState111 | MenhirState69 | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_COMMA | TOK_CPP_DEFINE | TOK_EOF | TOK_INT | TOK_OR | TOK_RPAREN | TOK_SEMICOLON | TOK_UINT | TOK_VOID ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _endpos_e_, _menhir_s, (e : (IntermediateSyntax.exp)), _startpos_e_) = _menhir_stack in
            let _startpos = _startpos_e_ in
            let _endpos = _endpos_e_ in
            let _v : (IntermediateSyntax.exp) = 
# 169 "frontend/Parser.mly"
                                   ( e )
# 1421 "frontend/Parser.ml"
             in
            _menhir_goto_logical_or_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | TOK_AND ->
            _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        _menhir_fail ()

and _menhir_reduce18 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (IntermediateSyntax.exp) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _endpos_x_, _menhir_s, (x : (IntermediateSyntax.exp)), _startpos_x_) = _menhir_stack in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (IntermediateSyntax.exp IntermediateSyntax.annotated) = let _endpos = _endpos_x_ in
    let _startpos = _startpos_x_ in
    
# 228 "frontend/Parser.mly"
          ( x, (_startpos, _endpos) )
# 1448 "frontend/Parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState206 | MenhirState17 | MenhirState192 | MenhirState119 | MenhirState128 | MenhirState187 | MenhirState184 | MenhirState182 | MenhirState180 | MenhirState149 | MenhirState153 | MenhirState170 | MenhirState155 | MenhirState151 | MenhirState147 | MenhirState145 | MenhirState141 | MenhirState130 | MenhirState126 | MenhirState121 | MenhirState21 | MenhirState33 | MenhirState111 | MenhirState51 | MenhirState97 | MenhirState94 | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_GREATER ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | TOK_GREATER_EQUAL ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | TOK_LESS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | TOK_LESS_EQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_GREATER ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | TOK_GREATER_EQUAL ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | TOK_LESS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | TOK_LESS_EQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TOK_AND | TOK_COMMA | TOK_CPP_DEFINE | TOK_EOF | TOK_EQUAL_EQUAL | TOK_INT | TOK_NOT_EQUAL | TOK_OR | TOK_RPAREN | TOK_SEMICOLON | TOK_UINT | TOK_VOID ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e1_), _endpos_e2_, _, (e2 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (IntermediateSyntax.exp) = 
# 179 "frontend/Parser.mly"
                                                                            ( I_neq (e1,e2) )
# 1493 "frontend/Parser.ml"
             in
            _menhir_goto_equality_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_GREATER ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | TOK_GREATER_EQUAL ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | TOK_LESS ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | TOK_LESS_EQUAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | TOK_AND | TOK_COMMA | TOK_CPP_DEFINE | TOK_EOF | TOK_EQUAL_EQUAL | TOK_INT | TOK_NOT_EQUAL | TOK_OR | TOK_RPAREN | TOK_SEMICOLON | TOK_UINT | TOK_VOID ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e1_), _endpos_e2_, _, (e2 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (IntermediateSyntax.exp) = 
# 178 "frontend/Parser.mly"
                                                                              ( I_eq (e1,e2) )
# 1524 "frontend/Parser.ml"
             in
            _menhir_goto_equality_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_equality_exp : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (IntermediateSyntax.exp) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState206 | MenhirState192 | MenhirState17 | MenhirState119 | MenhirState187 | MenhirState128 | MenhirState184 | MenhirState182 | MenhirState180 | MenhirState149 | MenhirState170 | MenhirState153 | MenhirState155 | MenhirState151 | MenhirState147 | MenhirState145 | MenhirState141 | MenhirState130 | MenhirState126 | MenhirState121 | MenhirState21 | MenhirState33 | MenhirState111 | MenhirState94 | MenhirState69 | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_AND | TOK_COMMA | TOK_CPP_DEFINE | TOK_EOF | TOK_INT | TOK_OR | TOK_RPAREN | TOK_SEMICOLON | TOK_UINT | TOK_VOID ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _endpos_e_, _menhir_s, (e : (IntermediateSyntax.exp)), _startpos_e_) = _menhir_stack in
            let _startpos = _startpos_e_ in
            let _endpos = _endpos_e_ in
            let _v : (IntermediateSyntax.exp) = 
# 173 "frontend/Parser.mly"
                                 ( e )
# 1553 "frontend/Parser.ml"
             in
            _menhir_goto_logical_and_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | TOK_EQUAL_EQUAL | TOK_NOT_EQUAL ->
            _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        _menhir_fail ()

and _menhir_run81 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (IntermediateSyntax.exp IntermediateSyntax.annotated) * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS_MINUS ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MULTIPLY ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_NOT ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS_PLUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RANDOM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_URANDOM ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_const _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_run84 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (IntermediateSyntax.exp IntermediateSyntax.annotated) * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS_MINUS ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MULTIPLY ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_NOT ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS_PLUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RANDOM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_URANDOM ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_const _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState84 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84

and _menhir_goto_relational_exp : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (IntermediateSyntax.exp) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState206 | MenhirState192 | MenhirState17 | MenhirState119 | MenhirState187 | MenhirState128 | MenhirState184 | MenhirState182 | MenhirState180 | MenhirState149 | MenhirState170 | MenhirState153 | MenhirState155 | MenhirState151 | MenhirState147 | MenhirState145 | MenhirState141 | MenhirState130 | MenhirState126 | MenhirState121 | MenhirState21 | MenhirState33 | MenhirState111 | MenhirState97 | MenhirState94 | MenhirState69 | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_AND | TOK_COMMA | TOK_CPP_DEFINE | TOK_EOF | TOK_EQUAL_EQUAL | TOK_INT | TOK_NOT_EQUAL | TOK_OR | TOK_RPAREN | TOK_SEMICOLON | TOK_UINT | TOK_VOID ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _endpos_e_, _menhir_s, (e : (IntermediateSyntax.exp)), _startpos_e_) = _menhir_stack in
            let _startpos = _startpos_e_ in
            let _endpos = _endpos_e_ in
            let _v : (IntermediateSyntax.exp) = 
# 177 "frontend/Parser.mly"
                                    ( e )
# 1665 "frontend/Parser.ml"
             in
            _menhir_goto_equality_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | TOK_GREATER | TOK_GREATER_EQUAL | TOK_LESS | TOK_LESS_EQUAL ->
            _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState105 | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        _menhir_fail ()

and _menhir_reduce5 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (IntermediateSyntax.exp) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _endpos_x_, _menhir_s, (x : (IntermediateSyntax.exp)), _startpos_x_) = _menhir_stack in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (IntermediateSyntax.exp IntermediateSyntax.annotated) = let _endpos = _endpos_x_ in
    let _startpos = _startpos_x_ in
    
# 228 "frontend/Parser.mly"
          ( x, (_startpos, _endpos) )
# 1692 "frontend/Parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_MINUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_AND | TOK_COMMA | TOK_CPP_DEFINE | TOK_EOF | TOK_EQUAL_EQUAL | TOK_GREATER | TOK_GREATER_EQUAL | TOK_INT | TOK_LESS | TOK_LESS_EQUAL | TOK_NOT_EQUAL | TOK_OR | TOK_RPAREN | TOK_SEMICOLON | TOK_UINT | TOK_VOID ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e1_), _endpos_e2_, _, (e2 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (IntermediateSyntax.exp) = 
# 184 "frontend/Parser.mly"
                                                                        ( I_leq (e1,e2) )
# 1714 "frontend/Parser.ml"
             in
            _menhir_goto_relational_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_MINUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_AND | TOK_COMMA | TOK_CPP_DEFINE | TOK_EOF | TOK_EQUAL_EQUAL | TOK_GREATER | TOK_GREATER_EQUAL | TOK_INT | TOK_LESS | TOK_LESS_EQUAL | TOK_NOT_EQUAL | TOK_OR | TOK_RPAREN | TOK_SEMICOLON | TOK_UINT | TOK_VOID ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e1_), _endpos_e2_, _, (e2 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (IntermediateSyntax.exp) = 
# 183 "frontend/Parser.mly"
                                                                    ( I_less (e1,e2) )
# 1741 "frontend/Parser.ml"
             in
            _menhir_goto_relational_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_MINUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_AND | TOK_COMMA | TOK_CPP_DEFINE | TOK_EOF | TOK_EQUAL_EQUAL | TOK_GREATER | TOK_GREATER_EQUAL | TOK_INT | TOK_LESS | TOK_LESS_EQUAL | TOK_NOT_EQUAL | TOK_OR | TOK_RPAREN | TOK_SEMICOLON | TOK_UINT | TOK_VOID ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e1_), _endpos_e2_, _, (e2 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (IntermediateSyntax.exp) = 
# 186 "frontend/Parser.mly"
                                                                          ( I_geq (e1,e2) )
# 1768 "frontend/Parser.ml"
             in
            _menhir_goto_relational_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_MINUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_AND | TOK_COMMA | TOK_CPP_DEFINE | TOK_EOF | TOK_EQUAL_EQUAL | TOK_GREATER | TOK_GREATER_EQUAL | TOK_INT | TOK_LESS | TOK_LESS_EQUAL | TOK_NOT_EQUAL | TOK_OR | TOK_RPAREN | TOK_SEMICOLON | TOK_UINT | TOK_VOID ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e1_), _endpos_e2_, _, (e2 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (IntermediateSyntax.exp) = 
# 185 "frontend/Parser.mly"
                                                                      ( I_greater (e1,e2) )
# 1795 "frontend/Parser.ml"
             in
            _menhir_goto_relational_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState206 | MenhirState17 | MenhirState192 | MenhirState21 | MenhirState119 | MenhirState126 | MenhirState128 | MenhirState187 | MenhirState130 | MenhirState180 | MenhirState182 | MenhirState184 | MenhirState145 | MenhirState147 | MenhirState149 | MenhirState151 | MenhirState153 | MenhirState170 | MenhirState155 | MenhirState141 | MenhirState121 | MenhirState33 | MenhirState51 | MenhirState111 | MenhirState69 | MenhirState94 | MenhirState97 | MenhirState105 | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_MINUS ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run74 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (IntermediateSyntax.exp IntermediateSyntax.annotated) * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS_MINUS ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MULTIPLY ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_NOT ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS_PLUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RANDOM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_URANDOM ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_const _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState74 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState74 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_run76 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (IntermediateSyntax.exp IntermediateSyntax.annotated) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS_MINUS ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MULTIPLY ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_NOT ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS_PLUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RANDOM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_URANDOM ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState76 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_const _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState76 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState76 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76

and _menhir_run78 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (IntermediateSyntax.exp IntermediateSyntax.annotated) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS_MINUS ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MULTIPLY ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_NOT ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS_PLUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RANDOM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_URANDOM ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState78 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_const _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState78 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState78 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78

and _menhir_goto_add_exp : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (IntermediateSyntax.exp) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState91 | MenhirState89 | MenhirState87 | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState206 | MenhirState17 | MenhirState192 | MenhirState21 | MenhirState119 | MenhirState126 | MenhirState128 | MenhirState187 | MenhirState130 | MenhirState180 | MenhirState182 | MenhirState184 | MenhirState145 | MenhirState147 | MenhirState149 | MenhirState151 | MenhirState153 | MenhirState170 | MenhirState155 | MenhirState141 | MenhirState121 | MenhirState33 | MenhirState51 | MenhirState111 | MenhirState69 | MenhirState94 | MenhirState97 | MenhirState105 | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_AND | TOK_COMMA | TOK_CPP_DEFINE | TOK_EOF | TOK_EQUAL_EQUAL | TOK_GREATER | TOK_GREATER_EQUAL | TOK_INT | TOK_LESS | TOK_LESS_EQUAL | TOK_NOT_EQUAL | TOK_OR | TOK_RPAREN | TOK_SEMICOLON | TOK_UINT | TOK_VOID ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _endpos_e_, _menhir_s, (e : (IntermediateSyntax.exp)), _startpos_e_) = _menhir_stack in
            let _startpos = _startpos_e_ in
            let _endpos = _endpos_e_ in
            let _v : (IntermediateSyntax.exp) = 
# 182 "frontend/Parser.mly"
                              ( e )
# 1957 "frontend/Parser.ml"
             in
            _menhir_goto_relational_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | TOK_MINUS | TOK_PLUS ->
            _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce16 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (IntermediateSyntax.exp) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _endpos_x_, _menhir_s, (x : (IntermediateSyntax.exp)), _startpos_x_) = _menhir_stack in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (IntermediateSyntax.exp IntermediateSyntax.annotated) = let _endpos = _endpos_x_ in
    let _startpos = _startpos_x_ in
    
# 228 "frontend/Parser.mly"
          ( x, (_startpos, _endpos) )
# 1981 "frontend/Parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState206 | MenhirState17 | MenhirState192 | MenhirState119 | MenhirState128 | MenhirState187 | MenhirState184 | MenhirState182 | MenhirState180 | MenhirState149 | MenhirState153 | MenhirState170 | MenhirState155 | MenhirState151 | MenhirState147 | MenhirState145 | MenhirState141 | MenhirState130 | MenhirState126 | MenhirState121 | MenhirState21 | MenhirState33 | MenhirState111 | MenhirState51 | MenhirState105 | MenhirState100 | MenhirState97 | MenhirState94 | MenhirState69 | MenhirState91 | MenhirState89 | MenhirState87 | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_DIVIDE ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MODULO ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MULTIPLY ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_DIVIDE ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MODULO ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MULTIPLY ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_AND | TOK_COMMA | TOK_CPP_DEFINE | TOK_EOF | TOK_EQUAL_EQUAL | TOK_GREATER | TOK_GREATER_EQUAL | TOK_INT | TOK_LESS | TOK_LESS_EQUAL | TOK_MINUS | TOK_NOT_EQUAL | TOK_OR | TOK_PLUS | TOK_RPAREN | TOK_SEMICOLON | TOK_UINT | TOK_VOID ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e1_), _startpos__2_), _endpos_e2_, _, (e2 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (IntermediateSyntax.exp) = 
# 190 "frontend/Parser.mly"
                                                          ( I_add (e1,e2) )
# 2022 "frontend/Parser.ml"
             in
            _menhir_goto_add_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_DIVIDE ->
            _menhir_run78 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MODULO ->
            _menhir_run76 _menhir_env (Obj.magic _menhir_stack)
        | TOK_MULTIPLY ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_AND | TOK_COMMA | TOK_CPP_DEFINE | TOK_EOF | TOK_EQUAL_EQUAL | TOK_GREATER | TOK_GREATER_EQUAL | TOK_INT | TOK_LESS | TOK_LESS_EQUAL | TOK_MINUS | TOK_NOT_EQUAL | TOK_OR | TOK_PLUS | TOK_RPAREN | TOK_SEMICOLON | TOK_UINT | TOK_VOID ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e1_), _startpos__2_), _endpos_e2_, _, (e2 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : (IntermediateSyntax.exp) = 
# 191 "frontend/Parser.mly"
                                                           ( I_minus (e1,e2) )
# 2051 "frontend/Parser.ml"
             in
            _menhir_goto_add_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_mul_exp : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (IntermediateSyntax.exp) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState206 | MenhirState192 | MenhirState17 | MenhirState119 | MenhirState187 | MenhirState128 | MenhirState184 | MenhirState182 | MenhirState180 | MenhirState149 | MenhirState170 | MenhirState153 | MenhirState155 | MenhirState151 | MenhirState147 | MenhirState145 | MenhirState141 | MenhirState130 | MenhirState126 | MenhirState121 | MenhirState21 | MenhirState33 | MenhirState111 | MenhirState105 | MenhirState100 | MenhirState97 | MenhirState94 | MenhirState91 | MenhirState89 | MenhirState87 | MenhirState71 | MenhirState69 | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_DIVIDE | TOK_MODULO | TOK_MULTIPLY ->
            _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
        | TOK_AND | TOK_COMMA | TOK_CPP_DEFINE | TOK_EOF | TOK_EQUAL_EQUAL | TOK_GREATER | TOK_GREATER_EQUAL | TOK_INT | TOK_LESS | TOK_LESS_EQUAL | TOK_MINUS | TOK_NOT_EQUAL | TOK_OR | TOK_PLUS | TOK_RPAREN | TOK_SEMICOLON | TOK_UINT | TOK_VOID ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _endpos_e_, _menhir_s, (e : (IntermediateSyntax.exp)), _startpos_e_) = _menhir_stack in
            let _startpos = _startpos_e_ in
            let _endpos = _endpos_e_ in
            let _v : (IntermediateSyntax.exp) = 
# 189 "frontend/Parser.mly"
                          ( e )
# 2082 "frontend/Parser.ml"
             in
            _menhir_goto_add_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState84 | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce16 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        _menhir_fail ()

and _menhir_goto_assign_op : _menhir_env -> 'ttv_tail -> (IntermediateSyntax.assignOp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS_MINUS ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MULTIPLY ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_NOT ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS_PLUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RANDOM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_URANDOM ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_const _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState69 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_reduce79 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (IntermediateSyntax.exp) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _endpos_e_, _menhir_s, (e : (IntermediateSyntax.exp)), _startpos_e_) = _menhir_stack in
    let _startpos = _startpos_e_ in
    let _endpos = _endpos_e_ in
    let _v : (IntermediateSyntax.exp) = 
# 194 "frontend/Parser.mly"
                             ( e )
# 2145 "frontend/Parser.ml"
     in
    _menhir_goto_mul_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_reduce20 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (IntermediateSyntax.exp) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _endpos_x_, _menhir_s, (x : (IntermediateSyntax.exp)), _startpos_x_) = _menhir_stack in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (IntermediateSyntax.exp IntermediateSyntax.annotated) = let _endpos = _endpos_x_ in
    let _startpos = _startpos_x_ in
    
# 228 "frontend/Parser.mly"
          ( x, (_startpos, _endpos) )
# 2159 "frontend/Parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (o : (IntermediateSyntax.unaryOp)), _startpos_o_), _endpos_e_, _, (e : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e_) = _menhir_stack in
        let _startpos = _startpos_o_ in
        let _endpos = _endpos_e_ in
        let _v : (IntermediateSyntax.exp) = 
# 204 "frontend/Parser.mly"
                                         ( I_unary (o,e) )
# 2172 "frontend/Parser.ml"
         in
        _menhir_goto_unary_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | MenhirState206 | MenhirState192 | MenhirState17 | MenhirState119 | MenhirState187 | MenhirState128 | MenhirState184 | MenhirState182 | MenhirState180 | MenhirState149 | MenhirState170 | MenhirState153 | MenhirState155 | MenhirState151 | MenhirState147 | MenhirState145 | MenhirState141 | MenhirState130 | MenhirState126 | MenhirState121 | MenhirState21 | MenhirState33 | MenhirState111 | MenhirState69 | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_DIVIDE_EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (IntermediateSyntax.assignOp) = 
# 165 "frontend/Parser.mly"
                     ( I_DIVIDE_EQUAL )
# 2188 "frontend/Parser.ml"
             in
            _menhir_goto_assign_op _menhir_env _menhir_stack _v
        | TOK_EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (IntermediateSyntax.assignOp) = 
# 161 "frontend/Parser.mly"
                ( I_EQUAL )
# 2199 "frontend/Parser.ml"
             in
            _menhir_goto_assign_op _menhir_env _menhir_stack _v
        | TOK_MINUS_EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (IntermediateSyntax.assignOp) = 
# 163 "frontend/Parser.mly"
                    ( I_MINUS_EQUAL )
# 2210 "frontend/Parser.ml"
             in
            _menhir_goto_assign_op _menhir_env _menhir_stack _v
        | TOK_MODULO_EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (IntermediateSyntax.assignOp) = 
# 166 "frontend/Parser.mly"
                     ( I_MODULO_EQUAL )
# 2221 "frontend/Parser.ml"
             in
            _menhir_goto_assign_op _menhir_env _menhir_stack _v
        | TOK_MULTIPLY_EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (IntermediateSyntax.assignOp) = 
# 164 "frontend/Parser.mly"
                      ( I_MULTIPLY_EQUAL )
# 2232 "frontend/Parser.ml"
             in
            _menhir_goto_assign_op _menhir_env _menhir_stack _v
        | TOK_PLUS_EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = () in
            let _v : (IntermediateSyntax.assignOp) = 
# 162 "frontend/Parser.mly"
                   ( I_PLUS_EQUAL )
# 2243 "frontend/Parser.ml"
             in
            _menhir_goto_assign_op _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e1_), _startpos__2_), _endpos_e2_, _, (e2 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e2_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos_e1_ in
        let _endpos = _endpos_e2_ in
        let _v : (IntermediateSyntax.exp) = 
# 195 "frontend/Parser.mly"
                                                                ( I_mul (e1,e2) )
# 2262 "frontend/Parser.ml"
         in
        _menhir_goto_mul_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e1_), _endpos_e2_, _, (e2 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e2_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos_e1_ in
        let _endpos = _endpos_e2_ in
        let _v : (IntermediateSyntax.exp) = 
# 197 "frontend/Parser.mly"
                                                              ( I_mod (e1,e2) )
# 2275 "frontend/Parser.ml"
         in
        _menhir_goto_mul_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e1_), _endpos_e2_, _, (e2 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e2_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos_e1_ in
        let _endpos = _endpos_e2_ in
        let _v : (IntermediateSyntax.exp) = 
# 196 "frontend/Parser.mly"
                                                              ( I_div (e1,e2) )
# 2288 "frontend/Parser.ml"
         in
        _menhir_goto_mul_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos_e_, _, (e : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e_) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_e_ in
        let _v : (IntermediateSyntax.exp) = 
# 202 "frontend/Parser.mly"
                                           ( I_predecr e )
# 2301 "frontend/Parser.ml"
         in
        _menhir_goto_unary_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e_) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_e_ in
        let _v : (IntermediateSyntax.exp) = 
# 203 "frontend/Parser.mly"
                                     ( I_not e )
# 2314 "frontend/Parser.ml"
         in
        _menhir_goto_unary_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos_e_, _, (e : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e_) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_e_ in
        let _v : (IntermediateSyntax.exp) = 
# 201 "frontend/Parser.mly"
                                          ( I_preincr e )
# 2327 "frontend/Parser.ml"
         in
        _menhir_goto_unary_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_TOK_COMMA_annotate_exp___ : _menhir_env -> 'ttv_tail -> _menhir_state -> (IntermediateSyntax.exp IntermediateSyntax.annotated list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__4_ = _endpos in
        let (((_menhir_stack, _menhir_s, (e : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e_), _startpos__2_), _, (xs0 : (IntermediateSyntax.exp IntermediateSyntax.annotated list))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos__4_ in
        let _v : (IntermediateSyntax.exp) = let p =
          let xs = xs0 in
          
# 206 "/usr/share/menhir/standard.mly"
    ( xs )
# 2356 "frontend/Parser.ml"
          
        in
        
# 214 "frontend/Parser.mly"
                                                                                               ( I_call (e,p) )
# 2362 "frontend/Parser.ml"
         in
        _menhir_goto_postfix_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_unary_exp : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (IntermediateSyntax.exp) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState26 | MenhirState28 | MenhirState31 | MenhirState78 | MenhirState76 | MenhirState74 | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce20 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState206 | MenhirState192 | MenhirState17 | MenhirState119 | MenhirState187 | MenhirState128 | MenhirState184 | MenhirState182 | MenhirState180 | MenhirState149 | MenhirState170 | MenhirState153 | MenhirState155 | MenhirState151 | MenhirState147 | MenhirState145 | MenhirState141 | MenhirState130 | MenhirState126 | MenhirState121 | MenhirState21 | MenhirState33 | MenhirState111 | MenhirState69 | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_AND | TOK_COMMA | TOK_CPP_DEFINE | TOK_DIVIDE | TOK_EOF | TOK_EQUAL_EQUAL | TOK_GREATER | TOK_GREATER_EQUAL | TOK_INT | TOK_LESS | TOK_LESS_EQUAL | TOK_MINUS | TOK_MODULO | TOK_MULTIPLY | TOK_NOT_EQUAL | TOK_OR | TOK_PLUS | TOK_RPAREN | TOK_SEMICOLON | TOK_UINT | TOK_VOID ->
            _menhir_reduce79 _menhir_env (Obj.magic _menhir_stack)
        | TOK_DIVIDE_EQUAL | TOK_EQUAL | TOK_MINUS_EQUAL | TOK_MODULO_EQUAL | TOK_MULTIPLY_EQUAL | TOK_PLUS_EQUAL ->
            _menhir_reduce20 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState105 | MenhirState100 | MenhirState97 | MenhirState94 | MenhirState91 | MenhirState89 | MenhirState87 | MenhirState84 | MenhirState81 | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce79 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_unary_op_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (IntermediateSyntax.unaryOp option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_const _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TOK_COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | TOK_MINUS ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_PLUS ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | TOK_const _ ->
                    _menhir_reduce85 _menhir_env (Obj.magic _menhir_stack) MenhirState38
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_const _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TOK_RBRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos__7_ = _endpos in
                let (((((_menhir_stack, _menhir_s, _startpos__1_), _, (o1 : (IntermediateSyntax.unaryOp option))), _endpos_e1_, (e1 : (
# 68 "frontend/Parser.mly"
       (string)
# 2466 "frontend/Parser.ml"
                )), _startpos_e1_), _, (o2 : (IntermediateSyntax.unaryOp option))), _endpos_e2_, (e2 : (
# 68 "frontend/Parser.mly"
       (string)
# 2470 "frontend/Parser.ml"
                )), _startpos_e2_) = _menhir_stack in
                let _7 = () in
                let _4 = () in
                let _1 = () in
                let _startpos = _startpos__1_ in
                let _endpos = _endpos__7_ in
                let _v : (IntermediateSyntax.exp) = 
# 224 "frontend/Parser.mly"
                                                                                                               ( I_interval (o1,e1,o2,e2) )
# 2480 "frontend/Parser.ml"
                 in
                _menhir_goto_primary_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_postfix_exp : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (IntermediateSyntax.exp) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_AND | TOK_COMMA | TOK_CPP_DEFINE | TOK_DIVIDE | TOK_DIVIDE_EQUAL | TOK_EOF | TOK_EQUAL | TOK_EQUAL_EQUAL | TOK_GREATER | TOK_GREATER_EQUAL | TOK_INT | TOK_LESS | TOK_LESS_EQUAL | TOK_MINUS | TOK_MINUS_EQUAL | TOK_MODULO | TOK_MODULO_EQUAL | TOK_MULTIPLY | TOK_MULTIPLY_EQUAL | TOK_NOT_EQUAL | TOK_OR | TOK_PLUS | TOK_PLUS_EQUAL | TOK_RPAREN | TOK_SEMICOLON | TOK_UINT | TOK_VOID ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _endpos_e_, _menhir_s, (e : (IntermediateSyntax.exp)), _startpos_e_) = _menhir_stack in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : (IntermediateSyntax.exp) = 
# 200 "frontend/Parser.mly"
                         ( e )
# 2513 "frontend/Parser.ml"
         in
        _menhir_goto_unary_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | TOK_LPAREN | TOK_MINUS_MINUS | TOK_PLUS_PLUS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _endpos_x_, _menhir_s, (x : (IntermediateSyntax.exp)), _startpos_x_) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _v : (IntermediateSyntax.exp IntermediateSyntax.annotated) = let _endpos = _endpos_x_ in
        let _startpos = _startpos_x_ in
        
# 228 "frontend/Parser.mly"
          ( x, (_startpos, _endpos) )
# 2525 "frontend/Parser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TOK_FALSE ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LBRACKET ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MINUS_MINUS ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MULTIPLY ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_NOT ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_PLUS ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_PLUS_PLUS ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RANDOM ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_TRUE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_URANDOM ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_const _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_id _v ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState51 in
                let _v : (IntermediateSyntax.exp IntermediateSyntax.annotated list) = 
# 128 "/usr/share/menhir/standard.mly"
    ( [] )
# 2573 "frontend/Parser.ml"
                 in
                _menhir_goto_loption_separated_nonempty_list_TOK_COMMA_annotate_exp___ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
        | TOK_MINUS_MINUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__2_ = _endpos in
            let _startpos__2_ = _startpos in
            let (_menhir_stack, _menhir_s, (e : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e_ in
            let _endpos = _endpos__2_ in
            let _v : (IntermediateSyntax.exp) = 
# 213 "frontend/Parser.mly"
                                                          ( I_decr e )
# 2595 "frontend/Parser.ml"
             in
            _menhir_goto_postfix_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | TOK_PLUS_PLUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__2_ = _endpos in
            let _startpos__2_ = _startpos in
            let (_menhir_stack, _menhir_s, (e : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e_ in
            let _endpos = _endpos__2_ in
            let _v : (IntermediateSyntax.exp) = 
# 212 "frontend/Parser.mly"
                                                        ( I_incr e )
# 2613 "frontend/Parser.ml"
             in
            _menhir_goto_postfix_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_list_annotate_stmt__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (IntermediateSyntax.stmt IntermediateSyntax.annotated list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_RCURLY ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, (s : (IntermediateSyntax.stmt IntermediateSyntax.annotated list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (IntermediateSyntax.stmt IntermediateSyntax.annotated list) = 
# 154 "frontend/Parser.mly"
                                                  ( s )
# 2652 "frontend/Parser.ml"
             in
            (match _menhir_s with
            | MenhirState192 | MenhirState17 | MenhirState119 | MenhirState187 | MenhirState128 | MenhirState184 | MenhirState149 | MenhirState170 | MenhirState153 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos_s_ = _endpos in
                let (s : (IntermediateSyntax.stmt IntermediateSyntax.annotated list)) = _v in
                let _startpos_s_ = _startpos in
                let _startpos = _startpos_s_ in
                let _endpos = _endpos_s_ in
                let _v : (IntermediateSyntax.stmt) = 
# 124 "frontend/Parser.mly"
                   ( I_block s )
# 2666 "frontend/Parser.ml"
                 in
                _menhir_goto_stmt _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
            | MenhirState16 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos_b_ = _endpos in
                let (b : (IntermediateSyntax.stmt IntermediateSyntax.annotated list)) = _v in
                let _startpos_b_ = _startpos in
                let (((((_menhir_stack, _menhir_s, _startpos__1_), _, _startpos__2_), _startpos__3_), _, (xs0 : ((IntermediateSyntax.typ IntermediateSyntax.annotated *
   string IntermediateSyntax.annotated)
  list))), _endpos__5_) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _2 = () in
                let _1 = () in
                let _startpos = _startpos__1_ in
                let _endpos = _endpos_b_ in
                let _v : (unit) = let p =
                  let xs = xs0 in
                  
# 206 "/usr/share/menhir/standard.mly"
    ( xs )
# 2689 "frontend/Parser.ml"
                  
                in
                
# 101 "frontend/Parser.mly"
                                                                                                     ( )
# 2695 "frontend/Parser.ml"
                 in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos_x_ = _endpos in
                let (x : (unit)) = _v in
                let _startpos_x_ = _startpos in
                let _startpos = _startpos_x_ in
                let _endpos = _endpos_x_ in
                let _v : (unit * (Lexing.position * Lexing.position)) = let _endpos = _endpos_x_ in
                let _startpos = _startpos_x_ in
                
# 228 "frontend/Parser.mly"
          ( x, (_startpos, _endpos) )
# 2709 "frontend/Parser.ml"
                 in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos_d_ = _endpos in
                let (d : (unit * (Lexing.position * Lexing.position))) = _v in
                let _startpos_d_ = _startpos in
                let _startpos = _startpos_d_ in
                let _endpos = _endpos_d_ in
                let _v : (IntermediateSyntax.decl) = 
# 82 "frontend/Parser.mly"
                             ( I_ignore )
# 2721 "frontend/Parser.ml"
                 in
                _menhir_goto_decl _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
            | MenhirState198 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos_b_ = _endpos in
                let (b : (IntermediateSyntax.stmt IntermediateSyntax.annotated list)) = _v in
                let _startpos_b_ = _startpos in
                let (((((_menhir_stack, _menhir_s, _startpos__10_), _, (f : (string IntermediateSyntax.annotated)), _startpos_f_), _startpos__3_), _, _startpos__4_), _endpos__5_) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _3 = () in
                let _10 = () in
                let _startpos = _startpos__10_ in
                let _endpos = _endpos_b_ in
                let _v : (IntermediateSyntax.functionDecl) = let t =
                  let _1 = _10 in
                  
# 111 "frontend/Parser.mly"
                ( None )
# 2742 "frontend/Parser.ml"
                  
                in
                
# 105 "frontend/Parser.mly"
                                                                                               ( t, f, [], b )
# 2748 "frontend/Parser.ml"
                 in
                _menhir_goto_function_decl _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
            | MenhirState202 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos_b_ = _endpos in
                let (b : (IntermediateSyntax.stmt IntermediateSyntax.annotated list)) = _v in
                let _startpos_b_ = _startpos in
                let (((((_menhir_stack, _menhir_s, _startpos__10_), _, (f : (string IntermediateSyntax.annotated)), _startpos_f_), _startpos__3_), _, (xs0 : ((IntermediateSyntax.typ IntermediateSyntax.annotated *
   string IntermediateSyntax.annotated)
  list))), _endpos__5_) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _10 = () in
                let _startpos = _startpos__10_ in
                let _endpos = _endpos_b_ in
                let _v : (IntermediateSyntax.functionDecl) = let p =
                  let xs = xs0 in
                  
# 206 "/usr/share/menhir/standard.mly"
    ( xs )
# 2770 "frontend/Parser.ml"
                  
                in
                let t =
                  let _1 = _10 in
                  
# 111 "frontend/Parser.mly"
                ( None )
# 2778 "frontend/Parser.ml"
                  
                in
                
# 107 "frontend/Parser.mly"
                                                                                                                      ( t, f, p, b )
# 2784 "frontend/Parser.ml"
                 in
                _menhir_goto_function_decl _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
            | MenhirState220 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos_b_ = _endpos in
                let (b : (IntermediateSyntax.stmt IntermediateSyntax.annotated list)) = _v in
                let _startpos_b_ = _startpos in
                let (((((_menhir_stack, _menhir_s, (t0 : (IntermediateSyntax.typ IntermediateSyntax.annotated)), _startpos_t0_), _, (f : (string IntermediateSyntax.annotated)), _startpos_f_), _startpos__3_), _, _startpos__4_), _endpos__5_) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _3 = () in
                let _startpos = _startpos_t0_ in
                let _endpos = _endpos_b_ in
                let _v : (IntermediateSyntax.functionDecl) = let t =
                  let t = t0 in
                  
# 110 "frontend/Parser.mly"
                            ( Some t )
# 2804 "frontend/Parser.ml"
                  
                in
                
# 105 "frontend/Parser.mly"
                                                                                               ( t, f, [], b )
# 2810 "frontend/Parser.ml"
                 in
                _menhir_goto_function_decl _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
            | MenhirState224 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos_b_ = _endpos in
                let (b : (IntermediateSyntax.stmt IntermediateSyntax.annotated list)) = _v in
                let _startpos_b_ = _startpos in
                let (((((_menhir_stack, _menhir_s, (t0 : (IntermediateSyntax.typ IntermediateSyntax.annotated)), _startpos_t0_), _, (f : (string IntermediateSyntax.annotated)), _startpos_f_), _startpos__3_), _, (xs0 : ((IntermediateSyntax.typ IntermediateSyntax.annotated *
   string IntermediateSyntax.annotated)
  list))), _endpos__5_) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _startpos = _startpos_t0_ in
                let _endpos = _endpos_b_ in
                let _v : (IntermediateSyntax.functionDecl) = let p =
                  let xs = xs0 in
                  
# 206 "/usr/share/menhir/standard.mly"
    ( xs )
# 2831 "frontend/Parser.ml"
                  
                in
                let t =
                  let t = t0 in
                  
# 110 "frontend/Parser.mly"
                            ( Some t )
# 2839 "frontend/Parser.ml"
                  
                in
                
# 107 "frontend/Parser.mly"
                                                                                                                      ( t, f, p, b )
# 2845 "frontend/Parser.ml"
                 in
                _menhir_goto_function_decl _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState192 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _endpos_x_, _menhir_s, (x : (IntermediateSyntax.stmt IntermediateSyntax.annotated))), _, (xs : (IntermediateSyntax.stmt IntermediateSyntax.annotated list))) = _menhir_stack in
        let _v : (IntermediateSyntax.stmt IntermediateSyntax.annotated list) = 
# 187 "/usr/share/menhir/standard.mly"
    ( x :: xs )
# 2863 "frontend/Parser.ml"
         in
        _menhir_goto_list_annotate_stmt__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_exp_stmt : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (IntermediateSyntax.stmt) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_s_ = _endpos in
    let (s : (IntermediateSyntax.stmt)) = _v in
    let _startpos_s_ = _startpos in
    let _startpos = _startpos_s_ in
    let _endpos = _endpos_s_ in
    let _v : (IntermediateSyntax.stmt) = 
# 118 "frontend/Parser.mly"
                  ( s )
# 2881 "frontend/Parser.ml"
     in
    _menhir_goto_stmt _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_goto_option_annotate_exp__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (IntermediateSyntax.exp IntermediateSyntax.annotated option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__3_ = _endpos in
        let _startpos__3_ = _startpos in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, (e : (IntermediateSyntax.exp IntermediateSyntax.annotated option))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : (IntermediateSyntax.stmt) = 
# 148 "frontend/Parser.mly"
                                                    ( I_return e )
# 2908 "frontend/Parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_s_ = _endpos in
        let (s : (IntermediateSyntax.stmt)) = _v in
        let _startpos_s_ = _startpos in
        let _startpos = _startpos_s_ in
        let _endpos = _endpos_s_ in
        let _v : (IntermediateSyntax.stmt) = 
# 122 "frontend/Parser.mly"
                   ( s )
# 2920 "frontend/Parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_separated_nonempty_list_TOK_COMMA_declarator_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (IntermediateSyntax.declarator list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState216 | MenhirState132 | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (IntermediateSyntax.declarator list)) = _v in
        let _v : (IntermediateSyntax.declarator list) = 
# 130 "/usr/share/menhir/standard.mly"
    ( x )
# 2940 "frontend/Parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_TOK_COMMA_declarator__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (IntermediateSyntax.declarator list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (IntermediateSyntax.declarator))) = _menhir_stack in
        let _2 = () in
        let _v : (IntermediateSyntax.declarator list) = 
# 217 "/usr/share/menhir/standard.mly"
    ( x :: xs )
# 2952 "frontend/Parser.ml"
         in
        _menhir_goto_separated_nonempty_list_TOK_COMMA_declarator_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_global_decl : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (IntermediateSyntax.globalDecl) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_x_ = _endpos in
    let (x : (IntermediateSyntax.globalDecl)) = _v in
    let _startpos_x_ = _startpos in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (IntermediateSyntax.globalDecl IntermediateSyntax.annotated) = let _endpos = _endpos_x_ in
    let _startpos = _startpos_x_ in
    
# 228 "frontend/Parser.mly"
          ( x, (_startpos, _endpos) )
# 2972 "frontend/Parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_FALSE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LBRACKET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LPAREN ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS_MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MULTIPLY ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS_PLUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RANDOM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_URANDOM ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState145 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_const _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState145 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_id _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState145 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
    | MenhirState17 | MenhirState192 | MenhirState119 | MenhirState128 | MenhirState187 | MenhirState184 | MenhirState149 | MenhirState153 | MenhirState170 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _endpos_d_, _menhir_s, (d : (IntermediateSyntax.globalDecl IntermediateSyntax.annotated)), _startpos_d_) = _menhir_stack in
        let _startpos = _startpos_d_ in
        let _endpos = _endpos_d_ in
        let _v : (IntermediateSyntax.stmt) = 
# 151 "frontend/Parser.mly"
                             ( I_local d )
# 3022 "frontend/Parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_s_ = _endpos in
        let (s : (IntermediateSyntax.stmt)) = _v in
        let _startpos_s_ = _startpos in
        let _startpos = _startpos_s_ in
        let _endpos = _endpos_s_ in
        let _v : (IntermediateSyntax.stmt) = 
# 123 "frontend/Parser.mly"
                        ( s )
# 3034 "frontend/Parser.ml"
         in
        _menhir_goto_stmt _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | MenhirState0 | MenhirState213 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _endpos_d_, _menhir_s, (d : (IntermediateSyntax.globalDecl IntermediateSyntax.annotated)), _startpos_d_) = _menhir_stack in
        let _startpos = _startpos_d_ in
        let _endpos = _endpos_d_ in
        let _v : (IntermediateSyntax.decl) = 
# 79 "frontend/Parser.mly"
                              ( I_global d )
# 3046 "frontend/Parser.ml"
         in
        _menhir_goto_decl _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | _ ->
        _menhir_fail ()

and _menhir_reduce96 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 32 "frontend/Parser.mly"
       (string)
# 3055 "frontend/Parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _endpos_e_, _menhir_s, (e : (
# 32 "frontend/Parser.mly"
       (string)
# 3061 "frontend/Parser.ml"
    )), _startpos_e_) = _menhir_stack in
    let _startpos = _startpos_e_ in
    let _endpos = _endpos_e_ in
    let _v : (IntermediateSyntax.exp) = 
# 221 "frontend/Parser.mly"
                                      ( I_id e )
# 3068 "frontend/Parser.ml"
     in
    _menhir_goto_primary_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_goto_unary_op : _menhir_env -> 'ttv_tail -> _menhir_state -> (IntermediateSyntax.unaryOp) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState38 | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (IntermediateSyntax.unaryOp)), _startpos_x_) = _menhir_stack in
        let _v : (IntermediateSyntax.unaryOp option) = 
# 102 "/usr/share/menhir/standard.mly"
    ( Some x )
# 3083 "frontend/Parser.ml"
         in
        _menhir_goto_option_unary_op_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState206 | MenhirState192 | MenhirState17 | MenhirState119 | MenhirState187 | MenhirState128 | MenhirState184 | MenhirState182 | MenhirState180 | MenhirState149 | MenhirState170 | MenhirState153 | MenhirState155 | MenhirState151 | MenhirState147 | MenhirState145 | MenhirState141 | MenhirState130 | MenhirState126 | MenhirState121 | MenhirState21 | MenhirState26 | MenhirState28 | MenhirState31 | MenhirState111 | MenhirState105 | MenhirState100 | MenhirState97 | MenhirState94 | MenhirState91 | MenhirState89 | MenhirState87 | MenhirState84 | MenhirState81 | MenhirState78 | MenhirState76 | MenhirState74 | MenhirState71 | MenhirState69 | MenhirState51 | MenhirState43 | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_FALSE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LBRACKET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LPAREN ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS_MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MULTIPLY ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS_PLUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RANDOM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_URANDOM ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_const _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_id _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
    | _ ->
        _menhir_fail ()

and _menhir_reduce85 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (IntermediateSyntax.unaryOp option) = 
# 100 "/usr/share/menhir/standard.mly"
    ( None )
# 3131 "frontend/Parser.ml"
     in
    _menhir_goto_option_unary_op_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_primary_exp : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (IntermediateSyntax.exp) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_e_ = _endpos in
    let (e : (IntermediateSyntax.exp)) = _v in
    let _startpos_e_ = _startpos in
    let _startpos = _startpos_e_ in
    let _endpos = _endpos_e_ in
    let _v : (IntermediateSyntax.exp) = 
# 211 "frontend/Parser.mly"
                                      ( e )
# 3147 "frontend/Parser.ml"
     in
    _menhir_goto_postfix_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_goto_decl : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (IntermediateSyntax.decl) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_CPP_DEFINE ->
        _menhir_run205 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_INT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState213 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_UINT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EOF ->
        _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState213
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState213

and _menhir_reduce65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (IntermediateSyntax.stmt IntermediateSyntax.annotated list) = 
# 185 "/usr/share/menhir/standard.mly"
    ( [] )
# 3178 "frontend/Parser.ml"
     in
    _menhir_goto_list_annotate_stmt__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_selection_stmt : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (IntermediateSyntax.stmt) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_s_ = _endpos in
    let (s : (IntermediateSyntax.stmt)) = _v in
    let _startpos_s_ = _startpos in
    let _startpos = _startpos_s_ in
    let _endpos = _endpos_s_ in
    let _v : (IntermediateSyntax.stmt) = 
# 119 "frontend/Parser.mly"
                      ( s )
# 3194 "frontend/Parser.ml"
     in
    _menhir_goto_stmt _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_goto_iteration_stmt : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (IntermediateSyntax.stmt) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_s_ = _endpos in
    let (s : (IntermediateSyntax.stmt)) = _v in
    let _startpos_s_ = _startpos in
    let _startpos = _startpos_s_ in
    let _endpos = _endpos_s_ in
    let _v : (IntermediateSyntax.stmt) = 
# 121 "frontend/Parser.mly"
                      ( s )
# 3210 "frontend/Parser.ml"
     in
    _menhir_goto_stmt _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_goto_conditional_stmt : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (IntermediateSyntax.stmt) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_s_ = _endpos in
    let (s : (IntermediateSyntax.stmt)) = _v in
    let _startpos_s_ = _startpos in
    let _startpos = _startpos_s_ in
    let _endpos = _endpos_s_ in
    let _v : (IntermediateSyntax.stmt) = 
# 120 "frontend/Parser.mly"
                        ( s )
# 3226 "frontend/Parser.ml"
     in
    _menhir_goto_stmt _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run18 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 32 "frontend/Parser.mly"
       (string)
# 3233 "frontend/Parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_AND | TOK_DIVIDE | TOK_DIVIDE_EQUAL | TOK_EQUAL | TOK_EQUAL_EQUAL | TOK_GREATER | TOK_GREATER_EQUAL | TOK_LESS | TOK_LESS_EQUAL | TOK_LPAREN | TOK_MINUS | TOK_MINUS_EQUAL | TOK_MINUS_MINUS | TOK_MODULO | TOK_MODULO_EQUAL | TOK_MULTIPLY | TOK_MULTIPLY_EQUAL | TOK_NOT_EQUAL | TOK_OR | TOK_PLUS | TOK_PLUS_EQUAL | TOK_PLUS_PLUS | TOK_SEMICOLON ->
        _menhir_reduce96 _menhir_env (Obj.magic _menhir_stack)
    | TOK_COLON ->
        _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_FALSE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState21 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LBRACKET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LPAREN ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS_MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState21 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MULTIPLY ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS_PLUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState21 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RANDOM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState21 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState21 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_URANDOM ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState21 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_const _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState21 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_id _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState21 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run120 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (IntermediateSyntax.stmt) = 
# 130 "frontend/Parser.mly"
                              ( I_SKIP )
# 3315 "frontend/Parser.ml"
     in
    _menhir_goto_exp_stmt _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run121 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS_MINUS ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MULTIPLY ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_NOT ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS_PLUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RANDOM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_URANDOM ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_const _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState121 in
        let _v : (IntermediateSyntax.exp IntermediateSyntax.annotated option) = 
# 100 "/usr/share/menhir/standard.mly"
    ( None )
# 3359 "frontend/Parser.ml"
         in
        _menhir_goto_option_annotate_exp__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121

and _menhir_run125 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_FALSE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LBRACKET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LPAREN ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS_MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MULTIPLY ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS_PLUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RANDOM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_URANDOM ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_const _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState126 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_id _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState126 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run129 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_FALSE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_INT ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LBRACKET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LPAREN ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS_MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MULTIPLY ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS_PLUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RANDOM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_UINT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_URANDOM ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_const _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_id _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run150 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_FALSE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LBRACKET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LPAREN ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS_MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MULTIPLY ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS_PLUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RANDOM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_URANDOM ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState151 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_const _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState151 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_id _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState151 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run154 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_FALSE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LBRACKET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LPAREN ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS_MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MULTIPLY ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS_PLUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RANDOM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_URANDOM ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState155 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_const _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState155 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_id _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState155 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_declarator : _menhir_env -> 'ttv_tail -> _menhir_state -> (IntermediateSyntax.declarator) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_id _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState138 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138)
    | TOK_SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (IntermediateSyntax.declarator))) = _menhir_stack in
        let _v : (IntermediateSyntax.declarator list) = 
# 215 "/usr/share/menhir/standard.mly"
    ( [ x ] )
# 3603 "frontend/Parser.ml"
         in
        _menhir_goto_separated_nonempty_list_TOK_COMMA_declarator_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_loption_separated_nonempty_list_TOK_COMMA_declarator__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (IntermediateSyntax.declarator list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__4_ = _endpos in
            let _startpos__4_ = _startpos in
            let (((_menhir_stack, _menhir_s, (t : (IntermediateSyntax.typ IntermediateSyntax.annotated)), _startpos_t_), _, _startpos__2_), _, (xs0 : (IntermediateSyntax.declarator list))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _startpos = _startpos_t_ in
            let _endpos = _endpos__4_ in
            let _v : (IntermediateSyntax.globalDecl) = let g =
              let xs = xs0 in
              
# 206 "/usr/share/menhir/standard.mly"
    ( xs )
# 3640 "frontend/Parser.ml"
              
            in
            
# 87 "frontend/Parser.mly"
                                                                                                 ( t, g )
# 3646 "frontend/Parser.ml"
             in
            _menhir_goto_global_decl _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState216 | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let _startpos__3_ = _startpos in
            let ((_menhir_stack, _menhir_s, (t : (IntermediateSyntax.typ IntermediateSyntax.annotated)), _startpos_t_), _, (xs0 : (IntermediateSyntax.declarator list))) = _menhir_stack in
            let _3 = () in
            let _startpos = _startpos_t_ in
            let _endpos = _endpos__3_ in
            let _v : (IntermediateSyntax.globalDecl) = let g =
              let xs = xs0 in
              
# 206 "/usr/share/menhir/standard.mly"
    ( xs )
# 3677 "frontend/Parser.ml"
              
            in
            
# 86 "frontend/Parser.mly"
                                                                                       ( t, g )
# 3683 "frontend/Parser.ml"
             in
            _menhir_goto_global_decl _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run22 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 32 "frontend/Parser.mly"
       (string)
# 3698 "frontend/Parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce96 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run19 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 68 "frontend/Parser.mly"
       (string)
# 3708 "frontend/Parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_e_ = _endpos in
    let (e : (
# 68 "frontend/Parser.mly"
       (string)
# 3717 "frontend/Parser.ml"
    )) = _v in
    let _startpos_e_ = _startpos in
    let _startpos = _startpos_e_ in
    let _endpos = _endpos_e_ in
    let _v : (IntermediateSyntax.exp) = 
# 223 "frontend/Parser.mly"
                                         ( I_const e )
# 3725 "frontend/Parser.ml"
     in
    _menhir_goto_primary_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run23 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (IntermediateSyntax.exp) = 
# 219 "frontend/Parser.mly"
                                       ( I_URANDOM )
# 3741 "frontend/Parser.ml"
     in
    _menhir_goto_primary_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run24 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (IntermediateSyntax.exp) = 
# 217 "frontend/Parser.mly"
                                     ( I_TRUE )
# 3757 "frontend/Parser.ml"
     in
    _menhir_goto_primary_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run25 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (IntermediateSyntax.exp) = 
# 218 "frontend/Parser.mly"
                                      ( I_RANDOM )
# 3773 "frontend/Parser.ml"
     in
    _menhir_goto_primary_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run26 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS_MINUS ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MULTIPLY ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_NOT ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS_PLUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RANDOM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_URANDOM ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState26 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_const _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState26 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState26 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (IntermediateSyntax.unaryOp) = 
# 207 "frontend/Parser.mly"
            ( I_PLUS )
# 3826 "frontend/Parser.ml"
     in
    _menhir_goto_unary_op _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS_MINUS ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MULTIPLY ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_NOT ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS_PLUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RANDOM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_URANDOM ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_const _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState28 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState28 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_id _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_e_ = _endpos in
        let (e : (
# 32 "frontend/Parser.mly"
       (string)
# 3885 "frontend/Parser.ml"
        )) = _v in
        let _startpos_e_ = _startpos in
        let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_e_ in
        let _v : (IntermediateSyntax.exp) = 
# 222 "frontend/Parser.mly"
                                                ( I_id e )
# 3895 "frontend/Parser.ml"
         in
        _menhir_goto_primary_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run31 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS_MINUS ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MULTIPLY ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_NOT ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS_PLUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RANDOM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_URANDOM ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState31 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_const _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState31 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState31 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _v : (IntermediateSyntax.unaryOp) = 
# 208 "frontend/Parser.mly"
             ( I_MINUS )
# 3954 "frontend/Parser.ml"
     in
    _menhir_goto_unary_op _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS_MINUS ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MULTIPLY ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_NOT ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS_PLUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RANDOM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_URANDOM ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_const _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState33 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState33 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_const _ ->
        _menhir_reduce85 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run42 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (IntermediateSyntax.exp) = 
# 220 "frontend/Parser.mly"
                                      ( I_FALSE )
# 4026 "frontend/Parser.ml"
     in
    _menhir_goto_primary_exp _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_goto_function_decl : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (IntermediateSyntax.functionDecl) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_x_ = _endpos in
    let (x : (IntermediateSyntax.functionDecl)) = _v in
    let _startpos_x_ = _startpos in
    let _startpos = _startpos_x_ in
    let _endpos = _endpos_x_ in
    let _v : (IntermediateSyntax.functionDecl IntermediateSyntax.annotated) = let _endpos = _endpos_x_ in
    let _startpos = _startpos_x_ in
    
# 228 "frontend/Parser.mly"
          ( x, (_startpos, _endpos) )
# 4044 "frontend/Parser.ml"
     in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_d_ = _endpos in
    let (d : (IntermediateSyntax.functionDecl IntermediateSyntax.annotated)) = _v in
    let _startpos_d_ = _startpos in
    let _startpos = _startpos_d_ in
    let _endpos = _endpos_d_ in
    let _v : (IntermediateSyntax.decl) = 
# 81 "frontend/Parser.mly"
                               ( I_function d )
# 4056 "frontend/Parser.ml"
     in
    _menhir_goto_decl _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ASSERT ->
        _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_CPP_IF ->
        _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_FOR ->
        _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_IF ->
        _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_INT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LCURLY ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS_MINUS ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MULTIPLY ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_NOT ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS_PLUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RANDOM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RETURN ->
        _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_SEMICOLON ->
        _menhir_run120 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_UINT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_URANDOM ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_WHILE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_const _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RCURLY ->
        _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_goto_stmt : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (IntermediateSyntax.stmt) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_x_ = _endpos in
    let (x : (IntermediateSyntax.stmt)) = _v in
    let _startpos_x_ = _startpos in
    let _endpos = _endpos_x_ in
    let _v : (IntermediateSyntax.stmt IntermediateSyntax.annotated) = let _endpos = _endpos_x_ in
    let _startpos = _startpos_x_ in
    
# 228 "frontend/Parser.mly"
          ( x, (_startpos, _endpos) )
# 4134 "frontend/Parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_CPP_ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TOK_ASSERT ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_CPP_IF ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_FALSE ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_FOR ->
                _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_IF ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_INT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LBRACKET ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LCURLY ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MINUS_MINUS ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MULTIPLY ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_NOT ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_PLUS ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_PLUS_PLUS ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RANDOM ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RETURN ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_SEMICOLON ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_TRUE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_UINT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_URANDOM ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_WHILE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_const _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_id _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState170)
        | TOK_CPP_ENDIF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__6_ = _endpos in
            let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_e_, _, (e : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e_), _endpos__4_), _endpos_s_, _, (s : (IntermediateSyntax.stmt IntermediateSyntax.annotated))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__6_ in
            let _v : (IntermediateSyntax.stmt) = 
# 139 "frontend/Parser.mly"
                                                                                               ( I_ifdef (e,s,None))
# 4216 "frontend/Parser.ml"
             in
            _menhir_goto_conditional_stmt _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState170 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_CPP_ENDIF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__8_ = _endpos in
            let ((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_e_, _, (e : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e_), _endpos__4_), _endpos_s1_, _, (s1 : (IntermediateSyntax.stmt IntermediateSyntax.annotated))), _endpos_s2_, _, (s2 : (IntermediateSyntax.stmt IntermediateSyntax.annotated))) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__8_ in
            let _v : (IntermediateSyntax.stmt) = 
# 140 "frontend/Parser.mly"
                                                                                                                         ( I_ifdef (e,s1,Some s2) )
# 4247 "frontend/Parser.ml"
             in
            _menhir_goto_conditional_stmt _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_d_, _, (d : (IntermediateSyntax.globalDecl IntermediateSyntax.annotated)), _startpos_d_), _endpos_e1_, _, (e1 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e1_), _endpos__5_, _startpos__5_), _endpos_e2_, _, (e2 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e2_), _endpos__7_), _endpos_s_, _, (s : (IntermediateSyntax.stmt IntermediateSyntax.annotated))) = _menhir_stack in
        let _7 = () in
        let _5 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_s_ in
        let _v : (IntermediateSyntax.stmt) = 
# 145 "frontend/Parser.mly"
                                                                                                                                  ( I_for (d,e1,e2,s) )
# 4269 "frontend/Parser.ml"
         in
        _menhir_goto_iteration_stmt _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | MenhirState184 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_e1_, _, (e1 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e1_), _endpos__4_, _startpos__4_), _endpos_e2_, _, (e2 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e2_), _endpos__6_, _startpos__6_), _endpos_e3_, _, (e3 : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e3_), _endpos__8_), _endpos_s_, _, (s : (IntermediateSyntax.stmt IntermediateSyntax.annotated))) = _menhir_stack in
        let _8 = () in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_s_ in
        let _v : (IntermediateSyntax.stmt) = 
# 144 "frontend/Parser.mly"
                                                                                                                                           ( I_for_simple (e1,e2,e3,s) )
# 4286 "frontend/Parser.ml"
         in
        _menhir_goto_iteration_stmt _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TOK_ASSERT ->
                _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_CPP_IF ->
                _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_FALSE ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_FOR ->
                _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_IF ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_INT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LBRACKET ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LCURLY ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MINUS_MINUS ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_MULTIPLY ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_NOT ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_PLUS ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_PLUS_PLUS ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RANDOM ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RETURN ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_SEMICOLON ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_TRUE ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_UINT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_URANDOM ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_WHILE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_const _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_id _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState187)
        | TOK_ASSERT | TOK_CPP_ELSE | TOK_CPP_ENDIF | TOK_CPP_IF | TOK_FALSE | TOK_FOR | TOK_IF | TOK_INT | TOK_LBRACKET | TOK_LCURLY | TOK_LPAREN | TOK_MINUS | TOK_MINUS_MINUS | TOK_MULTIPLY | TOK_NOT | TOK_PLUS | TOK_PLUS_PLUS | TOK_RANDOM | TOK_RCURLY | TOK_RETURN | TOK_SEMICOLON | TOK_TRUE | TOK_UINT | TOK_URANDOM | TOK_WHILE | TOK_const _ | TOK_id _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_e_, _, (e : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e_), _endpos__4_), _endpos_s_, _, (s : (IntermediateSyntax.stmt IntermediateSyntax.annotated))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_s_ in
            let _v : (IntermediateSyntax.stmt) = 
# 135 "frontend/Parser.mly"
                                                                             ( I_if (e,s,None) )
# 4362 "frontend/Parser.ml"
             in
            _menhir_goto_selection_stmt _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState187 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_e_, _, (e : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e_), _endpos__4_), _endpos_s1_, _, (s1 : (IntermediateSyntax.stmt IntermediateSyntax.annotated))), _endpos_s2_, _, (s2 : (IntermediateSyntax.stmt IntermediateSyntax.annotated))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_s2_ in
        let _v : (IntermediateSyntax.stmt) = 
# 136 "frontend/Parser.mly"
                                                                                                   ( I_if (e,s1,Some s2) )
# 4384 "frontend/Parser.ml"
         in
        _menhir_goto_selection_stmt _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_e_, _, (e : (IntermediateSyntax.exp IntermediateSyntax.annotated)), _startpos_e_), _endpos__4_), _endpos_s_, _, (s : (IntermediateSyntax.stmt IntermediateSyntax.annotated))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_s_ in
        let _v : (IntermediateSyntax.stmt) = 
# 143 "frontend/Parser.mly"
                                                                                          ( I_while (e,s) )
# 4399 "frontend/Parser.ml"
         in
        _menhir_goto_iteration_stmt _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | MenhirState192 | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_ASSERT ->
            _menhir_run154 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_CPP_IF ->
            _menhir_run150 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_FALSE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_FOR ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_IF ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_INT ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LBRACKET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LCURLY ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LPAREN ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS_MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MULTIPLY ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS_PLUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RANDOM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RETURN ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_SEMICOLON ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_UINT ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_URANDOM ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_WHILE ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_const _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState192 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_id _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState192 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RCURLY ->
            _menhir_reduce65 _menhir_env (Obj.magic _menhir_stack) MenhirState192
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState192)
    | _ ->
        _menhir_fail ()

and _menhir_reduce35 : _menhir_env -> 'ttv_tail * _menhir_state * (string IntermediateSyntax.annotated) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (d : (string IntermediateSyntax.annotated)), _startpos_d_) = _menhir_stack in
    let _v : (IntermediateSyntax.declarator) = 
# 94 "frontend/Parser.mly"
                                 ( d, None )
# 4470 "frontend/Parser.ml"
     in
    _menhir_goto_declarator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run141 : _menhir_env -> 'ttv_tail * _menhir_state * (string IntermediateSyntax.annotated) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_FALSE ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LBRACKET ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MINUS_MINUS ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_MULTIPLY ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_NOT ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_PLUS_PLUS ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_RANDOM ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_TRUE ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_URANDOM ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_const _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_id _v ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState141 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141

and _menhir_goto_separated_nonempty_list_TOK_COMMA_parameter_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((IntermediateSyntax.typ IntermediateSyntax.annotated *
   string IntermediateSyntax.annotated)
  list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState218 | MenhirState196 | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((IntermediateSyntax.typ IntermediateSyntax.annotated *
   string IntermediateSyntax.annotated)
  list)) = _v in
        let _v : ((IntermediateSyntax.typ IntermediateSyntax.annotated *
   string IntermediateSyntax.annotated)
  list) = 
# 130 "/usr/share/menhir/standard.mly"
    ( x )
# 4528 "frontend/Parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_TOK_COMMA_parameter__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((IntermediateSyntax.typ IntermediateSyntax.annotated *
   string IntermediateSyntax.annotated)
  list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (IntermediateSyntax.typ IntermediateSyntax.annotated *
  string IntermediateSyntax.annotated))) = _menhir_stack in
        let _2 = () in
        let _v : ((IntermediateSyntax.typ IntermediateSyntax.annotated *
   string IntermediateSyntax.annotated)
  list) = 
# 217 "/usr/share/menhir/standard.mly"
    ( x :: xs )
# 4545 "frontend/Parser.ml"
         in
        _menhir_goto_separated_nonempty_list_TOK_COMMA_parameter_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_TOK_COMMA_parameter__ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((IntermediateSyntax.typ IntermediateSyntax.annotated *
   string IntermediateSyntax.annotated)
  list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TOK_LCURLY ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState196 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TOK_LCURLY ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState202 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _menhir_s = MenhirState202 in
                let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos__6_ = _endpos in
                let _startpos__6_ = _startpos in
                let (((((_menhir_stack, _menhir_s, _startpos__10_), _, (f : (string IntermediateSyntax.annotated)), _startpos_f_), _startpos__3_), _, (xs0 : ((IntermediateSyntax.typ IntermediateSyntax.annotated *
   string IntermediateSyntax.annotated)
  list))), _endpos__5_) = _menhir_stack in
                let _6 = () in
                let _5 = () in
                let _3 = () in
                let _10 = () in
                let _startpos = _startpos__10_ in
                let _endpos = _endpos__6_ in
                let _v : (IntermediateSyntax.functionDecl) = let p =
                  let xs = xs0 in
                  
# 206 "/usr/share/menhir/standard.mly"
    ( xs )
# 4618 "frontend/Parser.ml"
                  
                in
                let t =
                  let _1 = _10 in
                  
# 111 "frontend/Parser.mly"
                ( None )
# 4626 "frontend/Parser.ml"
                  
                in
                
# 106 "frontend/Parser.mly"
                                                                                                                      ( t, f, p, [] )
# 4632 "frontend/Parser.ml"
                 in
                _menhir_goto_function_decl _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState202)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState218 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TOK_LCURLY ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let _menhir_s = MenhirState224 in
                let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _endpos__6_ = _endpos in
                let _startpos__6_ = _startpos in
                let (((((_menhir_stack, _menhir_s, (t0 : (IntermediateSyntax.typ IntermediateSyntax.annotated)), _startpos_t0_), _, (f : (string IntermediateSyntax.annotated)), _startpos_f_), _startpos__3_), _, (xs0 : ((IntermediateSyntax.typ IntermediateSyntax.annotated *
   string IntermediateSyntax.annotated)
  list))), _endpos__5_) = _menhir_stack in
                let _6 = () in
                let _5 = () in
                let _3 = () in
                let _startpos = _startpos_t0_ in
                let _endpos = _endpos__6_ in
                let _v : (IntermediateSyntax.functionDecl) = let p =
                  let xs = xs0 in
                  
# 206 "/usr/share/menhir/standard.mly"
    ( xs )
# 4681 "frontend/Parser.ml"
                  
                in
                let t =
                  let t = t0 in
                  
# 110 "frontend/Parser.mly"
                            ( Some t )
# 4689 "frontend/Parser.ml"
                  
                in
                
# 106 "frontend/Parser.mly"
                                                                                                                      ( t, f, p, [] )
# 4695 "frontend/Parser.ml"
                 in
                _menhir_goto_function_decl _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState224)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce75 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (IntermediateSyntax.declarator list) = 
# 128 "/usr/share/menhir/standard.mly"
    ( [] )
# 4721 "frontend/Parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_TOK_COMMA_declarator__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run133 : _menhir_env -> 'ttv_tail * _menhir_state * (IntermediateSyntax.typ IntermediateSyntax.annotated) * Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_id _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState133 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_SEMICOLON ->
        _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState133
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133

and _menhir_reduce4 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 32 "frontend/Parser.mly"
       (string)
# 4743 "frontend/Parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _endpos_x_, _menhir_s, (x : (
# 32 "frontend/Parser.mly"
       (string)
# 4749 "frontend/Parser.ml"
    )), _startpos_x_) = _menhir_stack in
    let _startpos = _startpos_x_ in
    let _v : (string IntermediateSyntax.annotated) = let _endpos = _endpos_x_ in
    let _startpos = _startpos_x_ in
    
# 228 "frontend/Parser.mly"
          ( x, (_startpos, _endpos) )
# 4757 "frontend/Parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (t : (IntermediateSyntax.typ IntermediateSyntax.annotated)), _startpos_t_), _, (d : (string IntermediateSyntax.annotated)), _startpos_d_) = _menhir_stack in
        let _v : (IntermediateSyntax.typ IntermediateSyntax.annotated *
  string IntermediateSyntax.annotated) = 
# 114 "frontend/Parser.mly"
                                                          ( t, d )
# 4769 "frontend/Parser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TOK_INT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState10 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_UINT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10)
        | TOK_RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (IntermediateSyntax.typ IntermediateSyntax.annotated *
  string IntermediateSyntax.annotated))) = _menhir_stack in
            let _v : ((IntermediateSyntax.typ IntermediateSyntax.annotated *
   string IntermediateSyntax.annotated)
  list) = 
# 215 "/usr/share/menhir/standard.mly"
    ( [ x ] )
# 4798 "frontend/Parser.ml"
             in
            _menhir_goto_separated_nonempty_list_TOK_COMMA_parameter_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState132 | MenhirState133 | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_EQUAL ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack)
        | TOK_COMMA | TOK_SEMICOLON ->
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState17 | MenhirState192 | MenhirState119 | MenhirState128 | MenhirState187 | MenhirState184 | MenhirState149 | MenhirState153 | MenhirState170 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__2_ = _endpos in
            let (_menhir_stack, _menhir_s, (l : (string IntermediateSyntax.annotated)), _startpos_l_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_l_ in
            let _endpos = _endpos__2_ in
            let _v : (IntermediateSyntax.stmt) = 
# 127 "frontend/Parser.mly"
                                  ( I_label l )
# 4840 "frontend/Parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos_s_ = _endpos in
            let (s : (IntermediateSyntax.stmt)) = _v in
            let _startpos_s_ = _startpos in
            let _startpos = _startpos_s_ in
            let _endpos = _endpos_s_ in
            let _v : (IntermediateSyntax.stmt) = 
# 117 "frontend/Parser.mly"
                     ( s )
# 4852 "frontend/Parser.ml"
             in
            _menhir_goto_stmt _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TOK_INT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState196 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_UINT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState196 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_VOID ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState196 in
                let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | TOK_RPAREN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    let _menhir_stack = (_menhir_stack, _endpos) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | TOK_LCURLY ->
                        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState198 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TOK_SEMICOLON ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                        let _menhir_s = MenhirState198 in
                        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _endpos__6_ = _endpos in
                        let _startpos__6_ = _startpos in
                        let (((((_menhir_stack, _menhir_s, _startpos__10_), _, (f : (string IntermediateSyntax.annotated)), _startpos_f_), _startpos__3_), _, _startpos__4_), _endpos__5_) = _menhir_stack in
                        let _6 = () in
                        let _5 = () in
                        let _4 = () in
                        let _3 = () in
                        let _10 = () in
                        let _startpos = _startpos__10_ in
                        let _endpos = _endpos__6_ in
                        let _v : (IntermediateSyntax.functionDecl) = let t =
                          let _1 = _10 in
                          
# 111 "frontend/Parser.mly"
                ( None )
# 4916 "frontend/Parser.ml"
                          
                        in
                        
# 104 "frontend/Parser.mly"
                                                                                              ( t, f, [], [] )
# 4922 "frontend/Parser.ml"
                         in
                        _menhir_goto_function_decl _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState198)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | TOK_RPAREN ->
                _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState196
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState196)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState205 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_FALSE ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState206 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LBRACKET ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState206 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_LPAREN ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState206 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState206 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MINUS_MINUS ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState206 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_MULTIPLY ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState206 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_NOT ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState206 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState206 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_PLUS_PLUS ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState206 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_RANDOM ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState206 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_TRUE ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState206 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_URANDOM ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState206 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_const _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState206 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_id _v ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState206 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState206)
    | MenhirState216 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_EQUAL ->
            _menhir_run141 _menhir_env (Obj.magic _menhir_stack)
        | TOK_LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TOK_INT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState218 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_UINT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_VOID ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState218 in
                let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | TOK_RPAREN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    let _menhir_stack = (_menhir_stack, _endpos) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | TOK_LCURLY ->
                        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | TOK_SEMICOLON ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                        let _menhir_s = MenhirState220 in
                        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _endpos__6_ = _endpos in
                        let _startpos__6_ = _startpos in
                        let (((((_menhir_stack, _menhir_s, (t0 : (IntermediateSyntax.typ IntermediateSyntax.annotated)), _startpos_t0_), _, (f : (string IntermediateSyntax.annotated)), _startpos_f_), _startpos__3_), _, _startpos__4_), _endpos__5_) = _menhir_stack in
                        let _6 = () in
                        let _5 = () in
                        let _4 = () in
                        let _3 = () in
                        let _startpos = _startpos_t0_ in
                        let _endpos = _endpos__6_ in
                        let _v : (IntermediateSyntax.functionDecl) = let t =
                          let t = t0 in
                          
# 110 "frontend/Parser.mly"
                            ( Some t )
# 5040 "frontend/Parser.ml"
                          
                        in
                        
# 104 "frontend/Parser.mly"
                                                                                              ( t, f, [], [] )
# 5046 "frontend/Parser.ml"
                         in
                        _menhir_goto_function_decl _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState220)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | TOK_RPAREN ->
                _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState218
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState218)
        | TOK_COMMA | TOK_SEMICOLON ->
            _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_decl_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (IntermediateSyntax.decl list) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_x_ = _endpos in
        let (x : (IntermediateSyntax.decl list)) = _v in
        let _startpos_x_ = _startpos in
        let _v : (IntermediateSyntax.decl list IntermediateSyntax.annotated) = let _endpos = _endpos_x_ in
        let _startpos = _startpos_x_ in
        
# 228 "frontend/Parser.mly"
          ( x, (_startpos, _endpos) )
# 5090 "frontend/Parser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (t : (IntermediateSyntax.decl list IntermediateSyntax.annotated))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 72 "frontend/Parser.mly"
       (IntermediateSyntax.decl list IntermediateSyntax.annotated)
# 5105 "frontend/Parser.ml"
            ) = 
# 76 "frontend/Parser.mly"
                                       ( t )
# 5109 "frontend/Parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 72 "frontend/Parser.mly"
       (IntermediateSyntax.decl list IntermediateSyntax.annotated)
# 5116 "frontend/Parser.ml"
            )) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState213 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos_xs_ = _endpos in
        let (xs : (IntermediateSyntax.decl list)) = _v in
        let _startpos_xs_ = _startpos in
        let (_menhir_stack, _endpos_x_, _menhir_s, (x : (IntermediateSyntax.decl)), _startpos_x_) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : (IntermediateSyntax.decl list) = 
# 187 "/usr/share/menhir/standard.mly"
    ( x :: xs )
# 5137 "frontend/Parser.ml"
         in
        _menhir_goto_list_decl_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | _ ->
        _menhir_fail ()

and _menhir_reduce77 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((IntermediateSyntax.typ IntermediateSyntax.annotated *
   string IntermediateSyntax.annotated)
  list) = 
# 128 "/usr/share/menhir/standard.mly"
    ( [] )
# 5150 "frontend/Parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_TOK_COMMA_parameter__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_global_typ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (IntermediateSyntax.typ) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos_x_ = _endpos in
    let (x : (IntermediateSyntax.typ)) = _v in
    let _startpos_x_ = _startpos in
    let _startpos = _startpos_x_ in
    let _v : (IntermediateSyntax.typ IntermediateSyntax.annotated) = let _endpos = _endpos_x_ in
    let _startpos = _startpos_x_ in
    
# 228 "frontend/Parser.mly"
          ( x, (_startpos, _endpos) )
# 5167 "frontend/Parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState218 | MenhirState196 | MenhirState4 | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_id _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState13 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13)
    | MenhirState17 | MenhirState192 | MenhirState119 | MenhirState128 | MenhirState187 | MenhirState184 | MenhirState149 | MenhirState153 | MenhirState170 | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_MULTIPLY ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_id _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_SEMICOLON ->
            _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132)
    | MenhirState0 | MenhirState213 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_MULTIPLY ->
            _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_id _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState216 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TOK_SEMICOLON ->
            _menhir_reduce75 _menhir_env (Obj.magic _menhir_stack) MenhirState216
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState216)
    | _ ->
        _menhir_fail ()

and _menhir_run2 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 32 "frontend/Parser.mly"
       (string)
# 5218 "frontend/Parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState224 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState220 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState218 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState216 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState213 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState206 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState205 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState202 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState198 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState196 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState192 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState187 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState184 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState182 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState180 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState170 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _startpos) = Obj.magic _menhir_stack in
    let _endpos = _startpos in
    let _v : (IntermediateSyntax.decl list) = 
# 185 "/usr/share/menhir/standard.mly"
    ( [] )
# 5483 "frontend/Parser.ml"
     in
    _menhir_goto_list_decl_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_ASSERT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState1 in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TOK_LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TOK_INT ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_UINT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TOK_RPAREN ->
                _menhir_reduce77 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | TOK_id _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState1 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_INT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let _startpos = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _endpos__2_ = _endpos in
        let _startpos__2_ = _startpos in
        let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (IntermediateSyntax.typ) = 
# 91 "frontend/Parser.mly"
                    ( I_UINT )
# 5553 "frontend/Parser.ml"
         in
        _menhir_goto_global_typ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run7 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _endpos__1_ = _endpos in
    let _startpos__1_ = _startpos in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : (IntermediateSyntax.typ) = 
# 90 "frontend/Parser.mly"
           ( I_INT )
# 5575 "frontend/Parser.ml"
     in
    _menhir_goto_global_typ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos

and _menhir_run205 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_id _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState205 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState205

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and file : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 72 "frontend/Parser.mly"
       (IntermediateSyntax.decl list IntermediateSyntax.annotated)
# 5607 "frontend/Parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TOK_CPP_DEFINE ->
        _menhir_run205 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_INT ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_UINT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_VOID ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TOK_EOF ->
        _menhir_reduce67 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 230 "frontend/Parser.mly"
  

# 5639 "frontend/Parser.ml"

# 219 "/usr/share/menhir/standard.mly"
  


# 5645 "frontend/Parser.ml"
