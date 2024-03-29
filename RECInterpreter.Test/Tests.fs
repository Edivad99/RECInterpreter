﻿namespace SVProject1.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open Program
open Interpreter

[<TestClass>]
type TestInterpreter () =

    [<TestMethod>]
    [<DataRow("f1(x1, x2) = x1 + 2, f2() = f2() + 1; 3 + f1(x1 + 2, f2()); x1 = 2;", 9)>]
    [<DataRow("f1(x) = if x then 1 else 0; f1(x); x = 1;", 0)>]
    [<DataRow("f1(x) = 2 - x * 2; f1(x); x = 2;", -2)>]
    [<DataRow("f1(x) = x * 2 - 2; f1(x); x = 2;", 2)>]
    [<DataRow("f1(x) = 2 - x; f1(x); x = 2;", 0)>]
    [<DataRow("f1(x) = x - 2; f1(x); x = 2;", 0)>]
    [<DataRow("f1(x) = 2 - x; f1(x); x = 2;", 0)>]
    [<DataRow("f1(x) = x - 2; f1(x); x = 2;", 0)>]
    [<DataRow("f1(x) = 2 - x; f1(x); x = 2;", 0)>]
    [<DataRow("functional (n) = if n then 1 else functional(n - 1) * n; functional (x); x = 3;", 6)>]
    member this.TestProgram (program, expected_result: int) =
        let result = interpreter(evaluate program) 0
        Assert.AreEqual(expected_result, result);
