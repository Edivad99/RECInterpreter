module SVProject1.Interpreter

open SVProject1.Ast

let rec valueVar(ds: VEnv, var: Variable): int option =
    match List.tryFind(fun (name, _) -> name = var) ds with
    | Some(_, value) -> value
    | None -> None

let rec valueFunc(fs: FEnv, var: Variable): Func =
    match List.tryFind(fun (name, _) -> name = var) fs with
    | Some(name, value) -> Func(name, value)
    | None -> Func("bottom", fun _ -> None)

let valueOp (op: Op, l: int option, r: int option): int option =
    match op with
    | Plus -> match (l, r) with
              | (Some(l1), Some(r1)) -> Some(l1 + r1)
              | _ -> None
    | Minus -> match (l, r) with
               | (Some(l1), Some(r1)) -> Some(l1 - r1)
               | _ -> None
    | Mult -> match (l, r) with
              | (Some(l1), Some(r1)) -> Some(l1 * r1)
              | _ -> None

let valueCond (guard: int option, true_branch: int option, false_branch): int option =
    match guard with
    | Some(value) -> if value = 0 then true_branch else false_branch
    | None -> None

let rec valueExpr (ProgramParsed(funcn, expr, decn)): int option =

    let valueParams(fs: FEnv, ds: VEnv, es: Expr list): int option list =
        List.map (fun e -> valueExpr(ProgramParsed(fs, e, ds))) es

    match expr with
    | EVar v -> valueVar(decn, v)
    | ENum n -> n
    | EOp (l, op, r) ->
        let left_expr = valueExpr(ProgramParsed(funcn, l, decn))
        let right_expr = valueExpr(ProgramParsed(funcn, r, decn))
        valueOp(op, left_expr, right_expr)
    | ECond (guard, true_branch, false_branch) ->
        let guard_expr = valueExpr(ProgramParsed(funcn, guard, decn))
        let true_branch_expr = valueExpr(ProgramParsed(funcn, true_branch, decn))
        let false_branch_expr = valueExpr(ProgramParsed(funcn, false_branch, decn))
        valueCond(guard_expr, true_branch_expr, false_branch_expr)
    | EFunc(f_name, pars) -> 
        let (_, f) = valueFunc (funcn, f_name)
        f(valueParams (funcn, decn, pars))

