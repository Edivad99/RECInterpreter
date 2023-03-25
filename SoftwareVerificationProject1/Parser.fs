// Implementation file for parser generated by fsyacc
module Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open FSharp.Text.Lexing
open FSharp.Text.Parsing.ParseHelpers
# 1 "Parser.fsy"

open SVProject1.Ast

# 10 "Parser.fs"
// This type is the type of tokens accepted by the parser
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
// This type is used to give symbolic names to token indexes, useful for error messages
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
// This type is used to give symbolic names to token indexes, useful for error messages
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

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | LPAREN  -> 1 
  | RPAREN  -> 2 
  | IF  -> 3 
  | THEN  -> 4 
  | ELSE  -> 5 
  | UNDEF  -> 6 
  | COLON  -> 7 
  | SEMICOLON  -> 8 
  | COMMA  -> 9 
  | EQ  -> 10 
  | PLUS  -> 11 
  | MINUS  -> 12 
  | MULT  -> 13 
  | ID _ -> 14 
  | Number _ -> 15 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_LPAREN 
  | 2 -> TOKEN_RPAREN 
  | 3 -> TOKEN_IF 
  | 4 -> TOKEN_THEN 
  | 5 -> TOKEN_ELSE 
  | 6 -> TOKEN_UNDEF 
  | 7 -> TOKEN_COLON 
  | 8 -> TOKEN_SEMICOLON 
  | 9 -> TOKEN_COMMA 
  | 10 -> TOKEN_EQ 
  | 11 -> TOKEN_PLUS 
  | 12 -> TOKEN_MINUS 
  | 13 -> TOKEN_MULT 
  | 14 -> TOKEN_ID 
  | 15 -> TOKEN_Number 
  | 18 -> TOKEN_end_of_input
  | 16 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startprog 
    | 1 -> NONTERM_prog 
    | 2 -> NONTERM_func_opt 
    | 3 -> NONTERM_func_opt 
    | 4 -> NONTERM_funcn 
    | 5 -> NONTERM_funcn 
    | 6 -> NONTERM_funcn 
    | 7 -> NONTERM_params 
    | 8 -> NONTERM_params 
    | 9 -> NONTERM_expr 
    | 10 -> NONTERM_expr 
    | 11 -> NONTERM_expr 
    | 12 -> NONTERM_expr 
    | 13 -> NONTERM_factor 
    | 14 -> NONTERM_factor 
    | 15 -> NONTERM_factor 
    | 16 -> NONTERM_factor 
    | 17 -> NONTERM_factor 
    | 18 -> NONTERM_factor 
    | 19 -> NONTERM_factor 
    | 20 -> NONTERM_factor 
    | 21 -> NONTERM_paramExpr 
    | 22 -> NONTERM_paramExpr 
    | 23 -> NONTERM_func 
    | 24 -> NONTERM_func 
    | 25 -> NONTERM_dec_opt 
    | 26 -> NONTERM_dec_opt 
    | 27 -> NONTERM_decn 
    | 28 -> NONTERM_decn 
    | 29 -> NONTERM_decn 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 18 
let _fsyacc_tagOfErrorTerminal = 16

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | LPAREN  -> "LPAREN" 
  | RPAREN  -> "RPAREN" 
  | IF  -> "IF" 
  | THEN  -> "THEN" 
  | ELSE  -> "ELSE" 
  | UNDEF  -> "UNDEF" 
  | COLON  -> "COLON" 
  | SEMICOLON  -> "SEMICOLON" 
  | COMMA  -> "COMMA" 
  | EQ  -> "EQ" 
  | PLUS  -> "PLUS" 
  | MINUS  -> "MINUS" 
  | MULT  -> "MULT" 
  | ID _ -> "ID" 
  | Number _ -> "Number" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | LPAREN  -> (null : System.Object) 
  | RPAREN  -> (null : System.Object) 
  | IF  -> (null : System.Object) 
  | THEN  -> (null : System.Object) 
  | ELSE  -> (null : System.Object) 
  | UNDEF  -> (null : System.Object) 
  | COLON  -> (null : System.Object) 
  | SEMICOLON  -> (null : System.Object) 
  | COMMA  -> (null : System.Object) 
  | EQ  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | MULT  -> (null : System.Object) 
  | ID _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | Number _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us;65535us;1us;65535us;0us;1us;1us;65535us;0us;2us;2us;65535us;0us;9us;20us;19us;2us;65535us;11us;15us;22us;23us;12us;65535us;3us;4us;13us;14us;17us;18us;33us;25us;34us;26us;35us;27us;43us;28us;45us;29us;46us;30us;47us;31us;48us;32us;50us;32us;12us;65535us;3us;24us;13us;24us;17us;24us;33us;24us;34us;24us;35us;24us;43us;24us;45us;24us;46us;24us;47us;24us;48us;24us;50us;24us;2us;65535us;48us;49us;50us;52us;12us;65535us;3us;42us;13us;42us;17us;42us;33us;42us;34us;42us;35us;42us;43us;42us;45us;42us;46us;42us;47us;42us;48us;42us;50us;42us;1us;65535us;5us;6us;2us;65535us;5us;54us;60us;59us;|]
let _fsyacc_sparseGotoTableRowOffsets = [|0us;1us;3us;5us;8us;11us;24us;37us;40us;53us;55us;|]
let _fsyacc_stateToProdIdxsTableElements = [| 1us;0us;1us;0us;1us;1us;1us;1us;4us;1us;10us;11us;12us;1us;1us;1us;1us;1us;1us;1us;1us;2us;2us;6us;2us;4us;5us;2us;4us;5us;1us;4us;1us;4us;4us;4us;10us;11us;12us;1us;5us;1us;5us;1us;5us;4us;5us;10us;11us;12us;2us;6us;6us;1us;6us;2us;7us;8us;1us;8us;1us;8us;1us;9us;4us;10us;10us;11us;12us;4us;10us;11us;11us;12us;4us;10us;11us;12us;12us;4us;10us;11us;12us;19us;4us;10us;11us;12us;20us;4us;10us;11us;12us;20us;4us;10us;11us;12us;20us;5us;10us;11us;12us;21us;22us;1us;10us;1us;11us;1us;12us;1us;13us;1us;14us;2us;15us;16us;1us;15us;1us;16us;3us;17us;23us;24us;1us;18us;1us;19us;1us;19us;1us;20us;1us;20us;1us;20us;1us;22us;1us;22us;2us;23us;24us;1us;23us;1us;24us;1us;24us;2us;25us;29us;2us;27us;28us;2us;27us;28us;1us;27us;1us;28us;2us;29us;29us;1us;29us;|]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us;2us;4us;6us;8us;13us;15us;17us;19us;21us;24us;27us;30us;32us;34us;39us;41us;43us;45us;50us;53us;55us;58us;60us;62us;64us;69us;74us;79us;84us;89us;94us;99us;105us;107us;109us;111us;113us;115us;118us;120us;122us;126us;128us;130us;132us;134us;136us;138us;140us;142us;145us;147us;149us;151us;154us;157us;160us;162us;164us;167us;|]
let _fsyacc_action_rows = 61
let _fsyacc_actionTableElements = [|1us;16387us;14us;10us;0us;49152us;1us;32768us;8us;3us;6us;32768us;1us;43us;3us;45us;6us;37us;12us;38us;14us;41us;15us;36us;4us;32768us;8us;5us;11us;33us;12us;34us;13us;35us;1us;16410us;14us;55us;1us;32768us;8us;7us;1us;32768us;0us;8us;0us;16385us;1us;16386us;9us;20us;1us;32768us;1us;11us;2us;32768us;2us;12us;14us;21us;1us;32768us;10us;13us;6us;32768us;1us;43us;3us;45us;6us;37us;12us;38us;14us;41us;15us;36us;3us;16388us;11us;33us;12us;34us;13us;35us;1us;32768us;2us;16us;1us;32768us;10us;17us;6us;32768us;1us;43us;3us;45us;6us;37us;12us;38us;14us;41us;15us;36us;3us;16389us;11us;33us;12us;34us;13us;35us;1us;16390us;9us;20us;1us;32768us;14us;10us;1us;16391us;9us;22us;1us;32768us;14us;21us;0us;16392us;0us;16393us;1us;16394us;13us;35us;1us;16395us;13us;35us;0us;16396us;4us;32768us;2us;44us;11us;33us;12us;34us;13us;35us;4us;32768us;4us;46us;11us;33us;12us;34us;13us;35us;4us;32768us;5us;47us;11us;33us;12us;34us;13us;35us;3us;16404us;11us;33us;12us;34us;13us;35us;4us;16405us;9us;48us;11us;33us;12us;34us;13us;35us;6us;32768us;1us;43us;3us;45us;6us;37us;12us;38us;14us;41us;15us;36us;6us;32768us;1us;43us;3us;45us;6us;37us;12us;38us;14us;41us;15us;36us;6us;32768us;1us;43us;3us;45us;6us;37us;12us;38us;14us;41us;15us;36us;0us;16397us;0us;16398us;2us;32768us;6us;40us;15us;39us;0us;16399us;0us;16400us;1us;16401us;1us;50us;0us;16402us;6us;32768us;1us;43us;3us;45us;6us;37us;12us;38us;14us;41us;15us;36us;0us;16403us;6us;32768us;1us;43us;3us;45us;6us;37us;12us;38us;14us;41us;15us;36us;6us;32768us;1us;43us;3us;45us;6us;37us;12us;38us;14us;41us;15us;36us;6us;32768us;1us;43us;3us;45us;6us;37us;12us;38us;14us;41us;15us;36us;6us;32768us;1us;43us;3us;45us;6us;37us;12us;38us;14us;41us;15us;36us;0us;16406us;7us;32768us;1us;43us;2us;51us;3us;45us;6us;37us;12us;38us;14us;41us;15us;36us;0us;16407us;1us;32768us;2us;53us;0us;16408us;1us;16409us;9us;60us;1us;32768us;10us;56us;2us;32768us;6us;58us;15us;57us;0us;16411us;0us;16412us;1us;16413us;9us;60us;1us;32768us;14us;55us;|]
let _fsyacc_actionTableRowOffsets = [|0us;2us;3us;5us;12us;17us;19us;21us;23us;24us;26us;28us;31us;33us;40us;44us;46us;48us;55us;59us;61us;63us;65us;67us;68us;69us;71us;73us;74us;79us;84us;89us;93us;98us;105us;112us;119us;120us;121us;124us;125us;126us;128us;129us;136us;137us;144us;151us;158us;165us;166us;174us;175us;177us;178us;180us;182us;185us;186us;187us;189us;|]
let _fsyacc_reductionSymbolCounts = [|1us;7us;1us;0us;5us;6us;3us;1us;3us;1us;3us;3us;3us;1us;1us;2us;2us;1us;1us;3us;6us;1us;3us;3us;4us;1us;0us;3us;3us;3us;|]
let _fsyacc_productionToNonTerminalTable = [|0us;1us;2us;2us;3us;3us;3us;4us;4us;5us;5us;5us;5us;6us;6us;6us;6us;6us;6us;6us;6us;7us;7us;8us;8us;9us;9us;10us;10us;10us;|]
let _fsyacc_immediateActions = [|65535us;49152us;65535us;65535us;65535us;65535us;65535us;65535us;16385us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;16392us;16393us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;16397us;16398us;65535us;16399us;16400us;65535us;16402us;65535us;16403us;65535us;65535us;65535us;65535us;16406us;65535us;16407us;65535us;16408us;65535us;65535us;65535us;16411us;16412us;65535us;65535us;|]
let _fsyacc_reductions = lazy [|
# 194 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> SVProject1.Ast.Program in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : 'gentype__startprog));
# 203 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_func_opt in
            let _3 = parseState.GetInput(3) :?> 'gentype_expr in
            let _5 = parseState.GetInput(5) :?> 'gentype_dec_opt in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 20 "Parser.fsy"
                                                                                     Program(_1, _3, _5) 
                   )
# 20 "Parser.fsy"
                 : SVProject1.Ast.Program));
# 216 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_funcn in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 23 "Parser.fsy"
                                                                               _1 
                   )
# 23 "Parser.fsy"
                 : 'gentype_func_opt));
# 227 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 25 "Parser.fsy"
                                                                               [] 
                   )
# 25 "Parser.fsy"
                 : 'gentype_func_opt));
# 237 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            let _5 = parseState.GetInput(5) :?> 'gentype_expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 28 "Parser.fsy"
                                                                               [FuncDec(_1, [], _5)] 
                   )
# 28 "Parser.fsy"
                 : 'gentype_funcn));
# 249 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            let _3 = parseState.GetInput(3) :?> 'gentype_params in
            let _6 = parseState.GetInput(6) :?> 'gentype_expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 29 "Parser.fsy"
                                                                               [FuncDec(_1, _3, _6)] 
                   )
# 29 "Parser.fsy"
                 : 'gentype_funcn));
# 262 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_funcn in
            let _3 = parseState.GetInput(3) :?> 'gentype_funcn in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 30 "Parser.fsy"
                                                                               _1 @ _3 
                   )
# 30 "Parser.fsy"
                 : 'gentype_funcn));
# 274 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 33 "Parser.fsy"
                                                                               [_1] 
                   )
# 33 "Parser.fsy"
                 : 'gentype_params));
# 285 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            let _3 = parseState.GetInput(3) :?> 'gentype_params in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 34 "Parser.fsy"
                                                                               _1 :: _3 
                   )
# 34 "Parser.fsy"
                 : 'gentype_params));
# 297 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_factor in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 37 "Parser.fsy"
                                                                               _1 
                   )
# 37 "Parser.fsy"
                 : 'gentype_expr));
# 308 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_expr in
            let _3 = parseState.GetInput(3) :?> 'gentype_expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 38 "Parser.fsy"
                                                                               EOp(_1, Plus, _3) 
                   )
# 38 "Parser.fsy"
                 : 'gentype_expr));
# 320 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_expr in
            let _3 = parseState.GetInput(3) :?> 'gentype_expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 39 "Parser.fsy"
                                                                               EOp(_1, Minus, _3) 
                   )
# 39 "Parser.fsy"
                 : 'gentype_expr));
# 332 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_expr in
            let _3 = parseState.GetInput(3) :?> 'gentype_expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 40 "Parser.fsy"
                                                                               EOp(_1, Mult, _3) 
                   )
# 40 "Parser.fsy"
                 : 'gentype_expr));
# 344 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> int in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "Parser.fsy"
                                                                               ENum(Some _1) 
                   )
# 43 "Parser.fsy"
                 : 'gentype_factor));
# 355 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 44 "Parser.fsy"
                                                                               ENum(None) 
                   )
# 44 "Parser.fsy"
                 : 'gentype_factor));
# 365 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> int in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 45 "Parser.fsy"
                                                                               ENum(Some -_2) 
                   )
# 45 "Parser.fsy"
                 : 'gentype_factor));
# 376 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 46 "Parser.fsy"
                                                                               ENum(None) 
                   )
# 46 "Parser.fsy"
                 : 'gentype_factor));
# 386 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 47 "Parser.fsy"
                                                                               EVar _1 
                   )
# 47 "Parser.fsy"
                 : 'gentype_factor));
# 397 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_func in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 48 "Parser.fsy"
                                                                               _1 
                   )
# 48 "Parser.fsy"
                 : 'gentype_factor));
# 408 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> 'gentype_expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 49 "Parser.fsy"
                                                                               _2 
                   )
# 49 "Parser.fsy"
                 : 'gentype_factor));
# 419 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> 'gentype_expr in
            let _4 = parseState.GetInput(4) :?> 'gentype_expr in
            let _6 = parseState.GetInput(6) :?> 'gentype_expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 50 "Parser.fsy"
                                                                               ECond(_2, _4, _6) 
                   )
# 50 "Parser.fsy"
                 : 'gentype_factor));
# 432 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_expr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 53 "Parser.fsy"
                                                                               [_1] 
                   )
# 53 "Parser.fsy"
                 : 'gentype_paramExpr));
# 443 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_expr in
            let _3 = parseState.GetInput(3) :?> 'gentype_paramExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 54 "Parser.fsy"
                                                                               [_1] @ _3 
                   )
# 54 "Parser.fsy"
                 : 'gentype_paramExpr));
# 455 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 57 "Parser.fsy"
                                                                               EFunc(_1, []) 
                   )
# 57 "Parser.fsy"
                 : 'gentype_func));
# 466 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            let _3 = parseState.GetInput(3) :?> 'gentype_paramExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 58 "Parser.fsy"
                                                                               EFunc(_1, _3) 
                   )
# 58 "Parser.fsy"
                 : 'gentype_func));
# 478 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_decn in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 62 "Parser.fsy"
                                                                               _1 
                   )
# 62 "Parser.fsy"
                 : 'gentype_dec_opt));
# 489 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 64 "Parser.fsy"
                                                                               [] 
                   )
# 64 "Parser.fsy"
                 : 'gentype_dec_opt));
# 499 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            let _3 = parseState.GetInput(3) :?> int in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 67 "Parser.fsy"
                                                                               [Def(_1, Some _3)] 
                   )
# 67 "Parser.fsy"
                 : 'gentype_decn));
# 511 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 68 "Parser.fsy"
                                                                               [Def(_1, None)] 
                   )
# 68 "Parser.fsy"
                 : 'gentype_decn));
# 522 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_decn in
            let _3 = parseState.GetInput(3) :?> 'gentype_decn in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 69 "Parser.fsy"
                                                                               _1 @ _3 
                   )
# 69 "Parser.fsy"
                 : 'gentype_decn));
|]
# 535 "Parser.fs"
let tables : FSharp.Text.Parsing.Tables<_> = 
  { reductions = _fsyacc_reductions.Value;
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 19;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = tables.Interpret(lexer, lexbuf, startState)
let prog lexer lexbuf : SVProject1.Ast.Program =
    engine lexer lexbuf 0 :?> _
