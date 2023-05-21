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
            match 7 with
            | 0 -> Console.ReadLine()
            | 1 -> "f1(x) = x+2; f1(x); x = 2;"
            | 2 -> "f1(a, b) = a + 2, f2() = f2(); f1(x1 * 3, f2()); x1 = 2;"
            | 3 -> "f2(x1) = x1 + 2; f2(y); y = undef;"
            | 4 -> "functional (n) = if n then 1 else functional(n-1) * n; functional (x); x = 3;"
            | 5 -> "functional (n) = if n then 1 else functional(n-1) * n; functional (-2);;"
            | 6 -> ";undef;;"
            | 7 -> "f(x) = x + 1; f(x) + f(y);x = 3, y = 2;"
            | _ -> ""
        try
            Console.WriteLine(input)
            let program = evaluate input
            Console.WriteLine(program)
            Console.WriteLine(interpreter program)
        with ex -> Console.WriteLine(ex.ToString())
        Console.ReadKey() |> ignore
    0
