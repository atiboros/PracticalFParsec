module Chaining

open FParsec


type P<'t> = Parser<'t, Unit>


let p1:P<_> = pstringCI "Jan"

let p2:P<_> = pint8

let pMonth:P<_> = p1 .>> (spaces >>. p2)

let pDay:P<_> = (p1 >>. spaces) >>.p2

let pMonthAndDay:P<_> = p1 .>>. (spaces >>. p2)

let run p s = 
    let unit = ()
    let result = runParserOnString p unit "" s
    match result with
    | Success(result, _, _) -> result
    | Failure(errorMessage, _, _) -> failwith errorMessage
     

run pMonth "Jan 1"

run pDay "Jan 2"

run pMonthAndDay "jan 3"