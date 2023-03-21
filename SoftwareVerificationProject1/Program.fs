open System
open FSharp.Text.Lexing

let evaluate (input:string) =
    let lexbuf = LexBuffer<char>.FromString input
    let output = Parser.parse Lexer.tokenize lexbuf
    string output

[<EntryPoint>]
let main args =
    printfn "Press Ctrl+c to Exit"
    while true do
        printf "Evaluate > "
        let input = Console.ReadLine()
        try
            let result = evaluate input
            printfn "%s" result
        with ex -> printfn "%s" (ex.ToString())
    0
