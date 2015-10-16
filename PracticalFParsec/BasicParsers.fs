module BasicParsers

open FParsec


let run p s = 
    let unit = ()
    let result = runParserOnString p unit "" s
    match result with
    | Success(result, _, _) -> result
    | Failure(errorMessage, _, _) -> failwith errorMessage
     

run (pstring "Jan") "Jan"


