module Program

open System
open FSharp.Text.Lexing
open Interpreter

let evaluate input =
    let lexbuf = LexBuffer<char>.FromString input
    Parser.prog Lexer.tokenize lexbuf

[<EntryPoint>]
let main args =
    printfn "Press Ctrl+c to Exit"
    while true do
        printf "Evaluate > "
        let input =
            match 4 with
            | 0 -> Console.ReadLine()
            | 1 -> "f(x) = x + 2; f(x); x = 2;" // CASO BASE
            | 2 -> "f1(a, b) = a + 2, f2() = f2(); f1(x * 3, f2()); x = 2;" // CASO CALL-BY-NAME
            | 3 -> "f(x) = x + 2; f(y); y = undef;" // CASO INFINITE LOOP
            | 4 -> "fact(n) = if n then 1 else fact(n - 1) * n; fact(x); x = 3;" // CASO FACTORIAL
            | 5 -> "fact(n) = if n then 1 else fact(n - 1) * n; fact(-2);;"
            | 6 -> ";undef;;"
            | 7 -> "f(x) = x + 1; f(x) + f(y);x = 3, y = 2;"
            | _ -> ""
        try
            Console.WriteLine(input)
            let program = evaluate input
            Console.WriteLine(program)
            Console.WriteLine(interpreter program 0)
        with ex -> Console.WriteLine(ex.ToString())
        Console.ReadKey() |> ignore
    0
