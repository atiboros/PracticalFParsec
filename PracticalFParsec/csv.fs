module Csv

open FParsec

//csvFile ::= (csvRecord)* 'EOF'
//csvRecord ::= csvStringList ('\n' | 'EOF')
//csvStringList ::= rawString [',' csvStringList]
//rawString := optionalSpaces [rawField optionalSpaces)]
//optionalSpaces ::= whitespace*
//whitespace ::= ' ' | '\t'
//rawField ::= simpleField | quotedField 
//simpleField ::= (any char except \n, EOF, \t, space, comma or double quote)+
//quotedField ::= '"' escapedField '"'
//escapedField ::= subField ['"' '"' escapedField]
//subField ::= (any char except double quote or EOF)+
//
//http://www.boyet.com/articles/csvparser.html

open FParsec

// value restriction
type P<'t> = Parser<'t, Unit>

// general helpers
let (<||>) p1 p2 =
    (attempt p1) <|> p2

let debug (p: P<_>) stream =
    p stream // set a breakpoint here


let subField:P<_> = many1 (noneOf "\"") |>> System.String.Concat

let simpleField:P<_> = many1 (noneOf "\n\t,\"") |>> System.String.Concat <?> "Expected simple field"

let ws:P<_> = skipAnyOf " \t"

let optionalSpaces:P<_> = (attempt (skipMany ws))

let escapedField, escapedFieldImpl = createParserForwardedToRef<string, unit>()

let quotedField = between (pchar '"') (pchar '"') escapedField <?> "Expected quoted field"

let rawField = simpleField <||> quotedField

let rawString = 
        optionalSpaces >>. (opt (rawField .>> optionalSpaces)) |>>
        (fun field ->
            match field with
                | Some f -> f
                | None -> "")

let cvsRecord = (sepBy rawString (skipChar ',')) .>> (skipNewline <|> eof)

let csvFile = manyTill cvsRecord eof

do escapedFieldImpl := 
    (pipe2
    subField (opt (pstring "\"\"" >>. escapedField))
    (fun subf escF ->
        match escF with
            | Some f -> subf + f
            | None -> subf))