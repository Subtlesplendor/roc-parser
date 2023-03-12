app "example"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.2.0/8tCohJeXMBUnjo_zdMq0jSaqdYoCWJkWazBd4wa8cQU.tar.br",
        #parser: "../package/main.roc",
    }
    imports [
        cli.Stdout,
        cli.Stderr,
        Parser.Minimal.{ skip, succeed, run, next, oneOf, many },
        ExampleParsers.Minimal.MinimalStr.{ Parser, char }
        #parser.ParserCore.{ Parser, buildPrimitiveParser, many },
        #parser.ParserStr.{ parseStr },
    ]
    provides [main] to cli

main =
    lettersInput = "AAAiBByAABBwBtCCCiAyArBBx" |> Str.toUtf8
    ifLetterA = \l -> l == A
    when run (many letterParser) lettersInput is
        Ok letters ->
            letters
            |> List.keepIf ifLetterA
            |> List.map \_ -> 1
            |> List.sum
            |> Num.toStr
            |> \countLetterA -> Stdout.line "I counted \(countLetterA) letter A's!"

        Err _ -> Stderr.line "Ooops, something went wrong parsing letters"




succeed Str.toNat
    


digs = 
    ["0","1","2","3","4","5","6","7","8","9"]
    |> List.map Str.toUtf8

isDigit = \c ->
    digs |> List.contains c 

digit =
    oneIf isDigit



nat: Parser Nat
nat = 
    succeed Str.toNat 




getChompedSource
