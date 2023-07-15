module Ast

type Op =
    | Plus
    | Minus
    | Mult
    override x.ToString() = $"%A{x}"

type Expr =
    | Var of string
    | Num of int option
    | Cond of Expr * Expr * Expr
    | Op of Expr * Op * Expr
    | Func of string * Expr list
    override x.ToString() = $"%A{x}"


type FuncDec = FuncDec of string * string list * Expr

type VEnv = Map<string, int option>

type Program = Program of FuncDec list * Expr * VEnv


// Interpreter types
type FEnv = Map<string, (int option list -> int option)>

type ProgramParsed = ProgramParsed of FEnv * Expr * VEnv
