module SVProject1.Interpreter

open SVProject1.Ast

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

let rec valueExpr (ProgramParsed(funcn, expr, decn)): int option =

    let valueParams (fs: FEnv, venv: VEnv, es: Expr list): int option list =
        List.map (fun e -> valueExpr(ProgramParsed(fs, e, venv))) es

    match expr with
    | EVar v -> valueVar(decn, v)
    | ENum n -> n
    | EOp (l, op, r) ->
        let left_expr = valueExpr(ProgramParsed(funcn, l, decn))
        let right_expr = valueExpr(ProgramParsed(funcn, r, decn))
        valueOp(left_expr, op, right_expr)
    | ECond (guard, true_branch, false_branch) ->
        let guard_expr = valueExpr(ProgramParsed(funcn, guard, decn))
        let true_branch_expr = valueExpr(ProgramParsed(funcn, true_branch, decn))
        let false_branch_expr = valueExpr(ProgramParsed(funcn, false_branch, decn))
        valueCond(guard_expr, true_branch_expr, false_branch_expr)
    | EFunc (f_name, pars) ->
        let f = valueFunc (funcn, f_name)
        f(valueParams (funcn, decn, pars))

//-------------------------------------------

let replaceVars (venv: VEnv, vars: string list, n: int option list): VEnv =
    (venv, vars, n)
    |||> List.fold2 (fun venv v n -> venv.Add(v, n))

(*
   FUNCTIONAL è una funzione che ritorna una funzione che aggiorna gli environment.
   La funzione ritornata aggiorna gli envs aggiungendo un set di altre funzioni, espandendoli.
   Le funzioni che vengono aggiunte sono quelle passate nella lista di definizioni che riceve functional.
   In pratica, è una fabbrica di funzioni che espandono gli environment che ricevono allo stesso modo,
   definito all'attivazione della fabbrica.
*)
let rec functional(funcs: FuncDec list, venv: VEnv): FEnv -> FEnv =
    match funcs with
    | [] -> (fun _ -> Map.empty)
    | FuncDec(name, parms, exp) :: fs ->
        fun (fenv: FEnv) -> 
            functional (fs, venv) fenv
            |> Map.add name (fun inp -> valueExpr (ProgramParsed(fenv, exp, (replaceVars(venv, parms, inp))) ) )

// https://stackoverflow.com/questions/1904049/in-f-what-does-the-operator-mean
let rec rho (f: FEnv -> FEnv) (n: int): FEnv -> FEnv =
    match n with
    | 0 -> id
    | k -> fun funcn -> (rho f (k - 1)) (f funcn) //fun x -> f(rho(f, k-1) x) //rho (f, k - 1) >> f //fun x -> (x |> rho (f, k - 1) |> f)

let findFix (Program(funcn, t, decn), k: int): int option =
    let r = rho (functional(funcn, decn)) k
    let fix = r bottom
    valueExpr(ProgramParsed(fix, t, decn))

let interpreter input = 
    let rec sub_iterpreter (n: int, input: Program) =
        printfn "Iterazioni: %d" n
        match findFix (input, n) with
        | Some n -> n
        | None -> sub_iterpreter (n + 1, input)
    sub_iterpreter (0, input)
