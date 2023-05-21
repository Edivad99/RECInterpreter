// Signature file for parser generated by fsyacc
module Parser
type token = 
  | EOF
  | LPAREN
  | RPAREN
  | IF
  | THEN
  | ELSE
  | UNDEF
  | COLON
  | SEMICOLON
  | COMMA
  | EQ
  | PLUS
  | MINUS
  | MULT
  | ID of (string)
  | Number of (int)
type tokenId = 
    | TOKEN_EOF
    | TOKEN_LPAREN
    | TOKEN_RPAREN
    | TOKEN_IF
    | TOKEN_THEN
    | TOKEN_ELSE
    | TOKEN_UNDEF
    | TOKEN_COLON
    | TOKEN_SEMICOLON
    | TOKEN_COMMA
    | TOKEN_EQ
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_MULT
    | TOKEN_ID
    | TOKEN_Number
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startprog
    | NONTERM_prog
    | NONTERM_func_opt
    | NONTERM_funcn
    | NONTERM_params
    | NONTERM_expr
    | NONTERM_factor
    | NONTERM_paramExpr
    | NONTERM_func
    | NONTERM_dec_opt
    | NONTERM_decn
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val prog : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (Ast.Program) 