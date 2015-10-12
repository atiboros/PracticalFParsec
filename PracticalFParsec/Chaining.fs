module Chaining

open FParsec


type Parser<'t> = Parser<'t, unit>


let p1:Parser<_> = pstringCI "Jan"

let p2:Parser<_> = pint8

let pMonth:Parser<_> = p1 .>> p2

let pDay:Parser<_> = p1 >>. p2

let pMonthAndDay:Parser<_> = p1 .>>. p2

let run1 p s = 
    match runParserOnString p () "" s with
    | Success(result, _, _) -> result
    | Failure(errorMessage, _, _) -> fail errorMessage
     

run1 pMonth "Jan 1"