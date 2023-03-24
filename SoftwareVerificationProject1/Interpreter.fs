module SVProject1.Interpreter

open SVProject1.Ast

let valueVar(ds: VEnv, var: Variable): int option =
    match List.tryFind(fun (name, _) -> name = var) ds with
    | Some(_, value) -> value
    | None -> None

let bottom = Func("bottom", fun _ -> None)

let valueFunc(fs: FEnv, var: Variable): Func =
    match List.tryFind(fun (name, _) -> name = var) fs with
    | Some(name, value) -> Func(name, value)
    | None -> bottom

let valueOp (op: Op, l: int option, r: int option): int option =
    let calculate op l r =
        match (l, r) with
        | (Some l, Some r) -> Some(op l r)
        | _ -> None
    match op with
    | Plus -> calculate ( + ) l r
    | Minus -> calculate ( - ) l r
    | Mult -> calculate ( * ) l r

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


//-------------------------------------------

(*let replaceVar (ds: VEnv, v: Variable, n: int option): VEnv =
    List.map(fun (Def(name, value)) -> if name = v then Def(name, n) else Def(name, value)) ds

let replaceVars (ds: VEnv, vars: Variable list, n: int option list): VEnv =
    List.map2(fun v n -> replaceVar(ds, v, n)) vars n*)

let rec replaceVar (ds: VEnv, v: Variable, n: int option): VEnv =
    match ds with
    | [] -> [Def(v, n)]
    | (name, value) :: xs ->
        if name = v then
            Def(name, n) :: replaceVar(xs, v, n)
        else
            Def(name, value) :: replaceVar(xs, v, n)

let rec replaceVars (ds: VEnv, vars: Variable list, n: int option list): VEnv =
    match (vars, n) with
    | [], [] -> ds
    | v::vs, n::ns -> replaceVars(replaceVar(ds, v, n), vs, ns)
    | _, _ -> failwithf "ERRORE replaceVars, due liste di diversa lunghezza"

let rec functional(funcs: FuncDec list, ds: VEnv): FEnv -> FEnv =
    match (funcs, ds) with
    | [], _ -> (fun _ -> [])
    | FuncDec(name, parms, exp) :: fs, venv -> fun (fenv: FEnv) ->
        let a = Func(name, (fun inp -> valueExpr(ProgramParsed(fenv, exp, (replaceVars(venv, parms, inp))))))
        let b = functional (fs, venv) fenv
        in a :: b

// https://stackoverflow.com/questions/1904049/in-f-what-does-the-operator-mean
let rec rho (boh: FEnv -> FEnv, n: int): FEnv -> FEnv =
    match (boh, n) with
    | _, 0 -> id
    | f, k -> rho (f, k - 1) >> f

let findFix (Program(funcn, t, decn), k: int): int option =
    let fix = rho (functional(funcn, decn), k) [bottom]
    in valueExpr(ProgramParsed(fix, t, decn))

let interpreter input =
    let rec sub_iterpreter (n: int, input: Program) =
        let findF = findFix (input, n)
        match findF with
        | Some n -> n
        | None -> sub_iterpreter (n + 1, input)
    sub_iterpreter (0, input)