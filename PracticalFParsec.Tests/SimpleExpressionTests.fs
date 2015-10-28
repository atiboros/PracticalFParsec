module SimpleExpressionTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open FsUnit.MsTest
open SimpleExpression
open System

[<TestClass>]
type SimpleExoressionTests() = 

    let run p s = 
        let unit = ()
        let result = runParserOnString p unit "" s
        match result with
            | Success(result, _, _) -> result
            | Failure(errorMessage, _, _) -> failwith errorMessage

    [<TestMethod>]
    member x.``Test parsing an integer number without brakets around it``() = 
        run pExpression "1" |>  should equal (Num(1.0))

    [<TestMethod>]
    member x.``Test parsing a negative integer number without brakets around it``() = 
        run pExpression "-12" |>  should equal (Num(-12.0))

    [<TestMethod>]
    member x.``Test parsing an double number without brakets around it``() = 
        run pExpression "5.67" |>  should equal (Num(5.67))

    [<TestMethod>]
    member x.``Test parsing anegative double number without brakets around it``() = 
        run pExpression "-5.67" |>  should equal (Num(-5.67))

    [<TestMethod>]
    member x.``Test parsing an integer number with brakets around it``() = 
        run pExpression "( 1 )" |>  should equal (Num(1.0))

    [<TestMethod>]
    member x.``Test parsing a negative integer number with brakets around it``() = 
        run pExpression "(-12)" |>  should equal (Num(-12.0))

    [<TestMethod>]
    member x.``Test parsing an double number with brakets around it``() = 
        run pExpression "( 5.67)" |>  should equal (Num(5.67))

    [<TestMethod>]
    member x.``Test parsing anegative double number with brakets around it``() = 
        run pExpression "(   -5.67)" |>  should equal (Num(-5.67))

    [<TestMethod>]
    member x.``Test parsing a number with many brakets around it``() = 
        run pExpression "(((( (( (( 5.67) ) )     )   )) )) " |>  should equal (Num(5.67))


    [<TestMethod>]
    [<ExpectedException(typeof<Exception>)>]
    member x.``Test parsing a number with unmatched brakets``() = 
        run pExpression "((((5.67) ) " |>  should equal (Num(5.67))

    [<TestMethod>]
    member x.``Test parsing an identifier``() = 
        run pExpression "x1" |>  should equal (Var("x1"))

    [<TestMethod>]
    member x.``Test parsing 1 + 1``() = 
        let result = run pExpression "1  + 1" 
        result |> should equal (Add(Num(1.0), Num(1.0)))

    [<TestMethod>]
    member x.``Test parsing x + 1``() = 
        run pExpression "x  + 1" |> should equal (Add(Var("x"), Num(1.0)))

    [<TestMethod>]
    member x.``Test parsing x + y``() = 
        run pExpression "x  +y" |> should equal (Add(Var("x"), Var("y")))

    [<TestMethod>]
    member x.``Test parsing x + ( y )``() = 
        run pExpression "x  + ( y )" |> should equal (Add(Var("x"), Var("y")))

    [<TestMethod>]
    member x.``Test parsing x + ( y + z)``() = 
        run pExpression "x  + ( y + z )" |> should equal (Add(Var("x"), Add(Var("y"),Var("z"))))

    [<TestMethod>]
    member x.``Test parsing 1 * 1``() = 
        let result = run pExpression "1  * 1" 
        result |> should equal (Mul(Num(1.0), Num(1.0)))

    [<TestMethod>]
    member x.``Test parsing x * 1``() = 
        run pExpression "x  * 1" |> should equal (Mul(Var("x"), Num(1.0)))

    [<TestMethod>]
    member x.``Test parsing x * y``() = 
        run pExpression "x * y" |> should equal (Mul(Var("x"), Var("y")))

    [<TestMethod>]
    member x.``Test parsing x * ( y )``() = 
        let result = run pExpression "x  * ( y )" 
        result |> should equal (Mul(Var("x"), Var("y")))

    [<TestMethod>]
    member x.``Test parsing x * ( y * z)``() = 
        let result = run pExpression "x  * ( y * z )" 
        result |> should equal (Mul(Var("x"), Mul(Var("y"),Var("z"))))

    [<TestMethod>]
    member x.``Test parsing x + ( y * z)``() = 
        run pExpression "x  + ( y * z )" |> should equal (Add(Var("x"), Mul(Var("y"),Var("z"))))

    [<TestMethod>]
    member x.``Test parsing x +  y * z``() = 
        run pExpression "x  +  y * z " |> should equal (Add(Var("x"), Mul(Var("y"),Var("z"))))

    [<TestMethod>]
    member x.``Test parsing x *  y + z``() = 
        run pExpression "x  *  (y + z) " |> should equal (Mul(Var("x"), Add(Var("y"),Var("z"))))

    [<TestMethod>]
    member x.``Test parsing (x *  y) + (5 + z)``() = 
        run pExpression "(x *  y) + (5 + z) " |> should equal (Add(Mul(Var "x", Var "y"), Add(Num 5.0, Var "z")))

    
    [<TestMethod>]
    member x.``Test parsing (x *  y / (z - y))^2 ``() = 
        let result = run pExpression "(x *  y / (z - y))^2" 
        result |> should equal
             (Pow(
                Mul(Var "x", 
                    Div(Var "y", 
                        Sub(Var "z", Var "y")
                   )
                ), 
                Num 2.0))

