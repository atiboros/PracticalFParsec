module Chaining

open FParsec


//type P<'t> = Parser<'t, Unit>

let pJanuary = pstringCI "Jan"
let pNumber = pint8

let pMonthJanuary = pJanuary .>> (spaces >>. pNumber)

let pDay = (pJanuary >>. spaces) >>.pNumber

let pJanuaryAndDay = pJanuary .>>. (spaces >>. pNumber)

let run p s = 
    let unit = ()
    let result = runParserOnString p unit "" s
    match result with
    | Success(result, _, _) -> result
    | Failure(errorMessage, _, _) -> failwith errorMessage
     

run pMonthJanuary "Jan 1" 

run pDay "Jan 2"

run pJanuaryAndDay "jan 3"