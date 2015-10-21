module Choice

open FParsec


//type P<'t> = Parser<'t, Unit>

let pMonth =
    pstringCI "JAN"  <|> pstringCI "FEB"  <|> pstringCI "MAR"  <|>
    pstringCI "APR"  <|> pstringCI "MAY"  <|> pstringCI "JUN"  <|>
    pstringCI "JUL"  <|> pstringCI "AUG"  <|> pstringCI "SEP"  <|>
    pstringCI "OCT"  <|> pstringCI "NOV"  <|> pstringCI "DEC"

let run p s = 
    let unit = ()
    let result = runParserOnString p unit "" s
    match result with
    | Success(result, _, _) -> result
    | Failure(errorMessage, _, _) -> failwith errorMessage
     

run pMonth "Feb" 

