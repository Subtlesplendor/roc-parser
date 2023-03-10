app "minimal-letter-count"
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

Letter : [A, B, C, Other]

parseA : Parser Letter
parseA = 
    succeed A
    |> skip (char 'A')

parseB : Parser Letter
parseB = 
    succeed B
    |> skip (char 'B')

parseC : Parser Letter
parseC = 
    succeed C
    |> skip (char 'C')

parseOther : Parser Letter
parseOther = 
    succeed Other
    |> skip next        

letterParser : Parser Letter
letterParser =
    oneOf [parseA, parseB, parseC, parseOther]


expect
    input = Str.toUtf8 "B"
    parser = letterParser
    result = run parser input
    result == Ok B

expect
    input = Str.toUtf8 "BCXA"
    parser = many letterParser
    result = run parser input
    result == Ok [B, C, Other, A]
