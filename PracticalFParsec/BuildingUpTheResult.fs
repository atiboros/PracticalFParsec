module BuildingUpTheResult

open FParsec


let (<||>) p1 p2 =
    (attempt p1) <|> p2

let run p s = 
    let unit = ()
    let result = runParserOnString p unit "" s
    match result with
    | Success(result, _, _) -> result
    | Failure(errorMessage, _, _) -> failwith errorMessage

type P<'t> = Parser<'t, Unit>

type Date =
    {
        Day: int8
        Month: string
        Year: int32
    }

let pDayAndMonth:P<_> =
    let md monthname =
        pipe3
            pint8
            (spaces >>. pstringCI monthname)
            pint32
            (fun day month year -> 
                { Day = day; Month = month; Year = year})
    md "JAN"  <||> md "FEB"  <||> md "MAR"  <||>
    md "APR"  <||> md "MAY"  <||> md "JUN"  <||>
    md "JUL"  <||> md "AUG"  <||> md "SEP"  <||>
    md "OCT"  <||> md "NOV"  <||> md "DEC"

let pMonth     

run pDayAndMonth "21 march"
