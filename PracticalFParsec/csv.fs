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

//http://www.boyet.com/articles/csvparser.html

open FParsec

// value restriction
type P<'t> = Parser<'t, Unit>

// general helpers
let (<||>) p1 p2 =
    (attempt p1) <|> p2

let subField:P<_> = many (noneOf "\"") |>> System.String.Concat

let escapedField, escapedFieldImpl = createParserForwardedToRef<string, unit>()

let simpleField:P<_> = many (noneOf "\n\t,\"") |>> System.String.Concat

let quotedField = between (pchar '"') (pchar '"') escapedField

let ws:P<_> = skipAnyOf " \t"

let optionalSpaces:P<_> = skipMany ws

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