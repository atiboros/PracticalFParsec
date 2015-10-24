module SimpleExpression

open FParsec

// value restriction
type P<'t> = Parser<'t, Unit>

// general helpers
let (<||>) p1 p2 =
    (attempt p1) <|> p2

type Expression =
    | Num of double
    | Var of string
    | Add of Expression * Expression
    | Sub of Expression * Expression
    | Mul of Expression * Expression
    | Neg of Expression


let bra:P<_> = pchar '(' .>> spaces
let ket:P<_> = spaces .>> pchar ')'

//let optBraketed p1 = between bra ket p1 <||> p1

let pNumber:P<_> = pfloat |>> Num

let pExpression, pExpressionImpl = createParserForwardedToRef<Expression, unit>()

let pExpressionInBrackets = between bra ket pExpression

let pVar = 
    (pipe2
        letter
        (many (letter <|> digit))
        (fun l rest -> l::rest |> System.String.Concat))
    |>> Var

let pAdd =
    pipe3
        (pNumber <||> pVar <||> pExpressionInBrackets)
        (spaces >>. pchar '+' .>> spaces)
        pExpression
        (fun exp1 _ exp2 -> Add(exp1, exp2))

let pMul =
    pipe3
        (pNumber <||> pVar <||> pExpressionInBrackets)
        (spaces >>. pchar '*' .>> spaces)
        pExpression
        (fun exp1 _ exp2 -> Mul(exp1, exp2))

do pExpressionImpl := pExpressionInBrackets <||> pAdd <||> pMul  <||> pNumber <||> pVar

//let rec pExpression:P<_> = pNumber <||> between bra ket pExpression

