﻿module Choice

open FParsec



let pMonth =
    pstringCI "JAN"  <|> pstringCI "FEB"  <|> pstringCI "MAR"  <|>
    pstringCI "APR"  <|> pstringCI "MAY"  <|> pstringCI "JUN"  <|>
    pstringCI "JUL"  <|> pstringCI "AUG"  <|> pstringCI "SEP"  <|>
    pstringCI "OCT"  <|> pstringCI "NOV"  <|> pstringCI "DEC"

let (<||>) p1 p2 =
    p1 <|> (attempt p2)

let run p s = 
    let unit = ()
    let result = runParserOnString p unit "" s
    match result with
    | Success(result, _, _) -> result
    | Failure(errorMessage, _, _) -> failwith errorMessage


run pMonth "Feb" 

type P<'t> = Parser<'t, Unit>

let pMonthAndDay:P<_> =
    let md monthname = pstringCI monthname .>>. (spaces >>. pint8)
    md "JAN"  <||> md "FEB"  <||> md "MAR"  <||>
    md "APR"  <||> md "MAY"  <||> md "JUN"  <||>
    md "JUL"  <||> md "AUG"  <||> md "SEP"  <||>
    md "OCT"  <||> md "NOV"  <||> md "DEC"

     



run pMonthAndDay "Mar 21"

