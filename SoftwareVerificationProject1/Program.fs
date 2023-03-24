module SVProject1.Program

open System
open FSharp.Text.Lexing
open SVProject1.Interpreter
open SVProject1.Ast

let evaluate input =
    let lexbuf = LexBuffer<char>.FromString input
    Parser.prog Lexer.tokenize lexbuf

[<EntryPoint>]
let main args =
    printfn "Press Ctrl+c to Exit"
    while true do
        printf "Evaluate > "
        //let input = Console.ReadLine()
        let input1 = "f1(x) = if x then 1 else 0; f1(x); x = 1;"
        let input2 = "f1(x1, x2) = x1 + 2, f2() = f2() + 1; 3 + f1(x1 + 2, f2()); x1 = 2;"
        let input3 = "f2(x1) = x1 + 2; f2(y); y = undef;"
        let input4 = "functional (n) = if n then 1 else functional(n-1) * n; functional (x); x = 3;"
        let input5 = "functional (n) = if n then 1 else functional(n-1) * n; functional (-2);;"
        let input6 = ";undef;;"
        try
            Console.WriteLine(input1)
            let result = evaluate input1
            Console.WriteLine(result)
            Console.WriteLine(interpreter result)
        with ex -> Console.WriteLine(ex.ToString())
        Console.ReadKey() |> ignore
    0
