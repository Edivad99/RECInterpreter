module SVProject1.Ast

type Op =
    | Plus
    | Minus
    | Mult
    override x.ToString() = sprintf "%A" x

type Expr =
    | EVar of string 
    | ENum of int option
    | ECond of Expr * Expr * Expr
    | EOp of Expr * Op * Expr
    | EFunc of string * Expr list
    override x.ToString() = sprintf "%A" x


type FuncDec = FuncDec of string * string list * Expr

type VEnv = Map<string, int option>

type Program = Program of FuncDec list * Expr * VEnv


// Interpreter types
type FEnv = Map<string, (int option list -> int option)>
type ProgramParsed = ProgramParsed of FEnv * Expr * VEnv
