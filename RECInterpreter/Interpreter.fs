module Interpreter

open Ast

let valueVar(venv: VEnv, name: string): int option = Map.tryFind name venv |> Option.flatten

let bottom = Map.empty<_, (int option list -> int option)>.Add("bottom", fun _ -> None)

let valueFunc(fenv: FEnv, name: string): (int option list -> int option) =
    match Map.tryFind name fenv with
    | Some value -> value
    | None -> fun _ -> None

let valueOp (l: int option, op: Op, r: int option): int option =
    let calculate op l r =
        match (l, r) with
        | Some l, Some r -> Some(op l r)
        | _ -> None
    match op with
    | Plus -> calculate ( + ) l r
    | Minus -> calculate ( - ) l r
    | Mult -> calculate ( * ) l r

let valueCond (guard, true_branch, false_branch) =
    match guard with
    | Some value -> if value = 0 then true_branch else false_branch
    | None -> None

let rec valueExpr (ProgramParsed(f_decs, expr, v_decs)): int option =
    match expr with
    | Var v -> valueVar(v_decs, v)
    | Num n -> n
    | Op (l, op, r) ->
        let left_expr = valueExpr(ProgramParsed(f_decs, l, v_decs))
        let right_expr = valueExpr(ProgramParsed(f_decs, r, v_decs))
        valueOp(left_expr, op, right_expr)
    | Cond (guard, true_branch, false_branch) ->
        let guard_expr = valueExpr(ProgramParsed(f_decs, guard, v_decs))
        let true_branch_expr = valueExpr(ProgramParsed(f_decs, true_branch, v_decs))
        let false_branch_expr = valueExpr(ProgramParsed(f_decs, false_branch, v_decs))
        valueCond(guard_expr, true_branch_expr, false_branch_expr)
    | Func (f_name, pars) ->
        let f = valueFunc (f_decs, f_name)
        f(valueParams (f_decs, v_decs, pars))

and valueParams (fs: FEnv, venv: VEnv, es: Expr list): int option list =
    List.map (fun e -> valueExpr(ProgramParsed(fs, e, venv))) es

//-------------------------------------------

let replaceVars (venv: VEnv, vars: string list, ns: int option list): VEnv =
    (venv, vars, ns)
    |||> List.fold2 (fun venv v n -> venv.Add(v, n))

let rec createFunctional(f_decs: FuncDec list, venv: VEnv): FEnv -> FEnv =
    match f_decs with
    | [] -> (fun _ -> Map.empty)
    | FuncDec(name, parms, exp) :: fs ->
        fun (fenv: FEnv) -> 
            createFunctional (fs, venv) fenv
            |> Map.add name (fun inp -> valueExpr (ProgramParsed(fenv, exp, replaceVars(venv, parms, inp)) ) )

let rec createFunctionalN (f: FEnv -> FEnv) (n: int): FEnv -> FEnv = // KKT iterations
    match n with
    | 0 -> id
    | k -> fun fenv -> (createFunctionalN f (k - 1)) (f fenv)

let findFix (Program(f_decs, t, v_decs), k: int): int option =
    let f = createFunctionalN (createFunctional(f_decs, v_decs)) k
    let fix = f bottom
    valueExpr(ProgramParsed(fix, t, v_decs))

let rec interpreter input iteration = 
    printfn "Iterazioni: %d" iteration
    match findFix (input, iteration) with
    | Some n -> n
    | None -> interpreter input (iteration + 1)
