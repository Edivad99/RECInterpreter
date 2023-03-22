﻿module SVProject1.Ast

type Variable = string

type Op =
    | Plus
    | Minus
    | Mult

type Expr =
    | EVar of Variable
    | ENum of int option
    | ECond of Expr * Expr * Expr
    | EOp of Expr * Op * Expr
    | EFunc of Variable * Expr list

type FuncDec = Variable * Variable list * Expr

type Def = Variable * int option

type VEnv = Def list

type Program = FuncDec list * Expr * VEnv
