module SVProject1.Ast

type Variable = string

type Op =
    | Plus
    | Minus
    | Mult
    override x.ToString() = sprintf "%A" x

type Expr =
    | EVar of Variable 
    | ENum of int option
    | ECond of Expr * Expr * Expr
    | EOp of Expr * Op * Expr
    | EFunc of Variable * Expr list
    override x.ToString() = sprintf "%A" x


type FuncDec = FuncDec of Variable * Variable list * Expr

type Def = Def of Variable * int option

type VEnv = Venv of Def list

type Program = Program of FuncDec list * Expr * VEnv


// Interpreter types

type Func = Func of Variable * (int option list -> int option)
type FEnv = Func list 
type ProgramParsed = ProgramParsed of FEnv * Expr * VEnv
