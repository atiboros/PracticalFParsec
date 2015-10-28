module CvsTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open FsUnit.MsTest
open Csv
open System

[<TestClass>]
type CsvTests() = 

    let run p s = 
        let unit = ()
        let result = runParserOnString p unit "" s
        match result with
            | Success(result, _, _) -> result
            | Failure(errorMessage, _, _) -> failwith errorMessage

    [<TestMethod>]
    member x.``Test a single simple csv line``() = 
        let result = run csvFile "1,2,3" 
        result |>  should equal [["1";"2";"3"]]

    [<TestMethod>]
    member x.``Testcomplex csv lines``() = 
        let input = @"A1,B1,""C1,+comma"",D1
,B2,""line 1
line 2"",D2
,,C3,""D3,+comma""
,,,D4 space"

        let result = run csvFile input
        let text = sprintf "%A" result
        let expected =
            [
                ["A1"; "B1"; "C1,+comma"; "D1"]
                [""; "B2"; "line 1\nline 2"; "D2"]
                [""; ""; "C3"; "D3,+comma"]
                [""; ""; ""; "D4 space"]]
        result |>  should equal expected
