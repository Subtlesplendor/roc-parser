app "example"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        parser: "../package/main.roc",
    }
    imports [
        cli.Stdout,
        #cli.Stderr,
        #parser.Parser.Advanced.Generic,
        parser.Parser.Advanced.Utf8,
        #parser.Parser.Utf8.{ Parser, buildPrimitiveParser, fromState, RawStr, RawChar, string, map, keep, chompIf, chompWhile, skip, const, getChompedRawStr },
        parser.Parser.Utf8.{ Parser } 
    ]
    provides [main] to cli


# Letter : [A, B, C, Other]

# toLetter : RawChar -> Letter
# toLetter = \c ->
#     when c is
#         65 -> A
#         66 -> B
#         67 -> C
#         _ -> Other

# toLetters : RawStr -> List Letter
# toLetters = \s -> s |> List.map toLetter
# #Problem : [ParsingFailure Str]

# isNotStar: RawChar -> Bool
# isNotStar = \c -> c != 42

# always: RawChar -> Bool
# always = \_ -> Bool.true

# everything = chompWhile always

# chomped = getChompedRawStr


# letter : Parser (List Letter)
# letter = 
#     const toLetter
#     |> skip everything
#     |> keep chomped
#     |> map toLetters
    

# letterParser : Parser Letter
# letterParser =
#     buildPrimitiveParser \state ->
#         input = state.src
#         when input is
#             [] -> Bad Bool.false (fromState state (ParsingFailure "Nothing to parse"))
#             ['A', ..] -> Good Bool.false A {state & src: List.dropFirst input}
#             ['B', ..] -> Good Bool.false B {state & src: List.dropFirst input}
#             ['C', ..] -> Good Bool.false C {state & src: List.dropFirst input}
#             _ -> Good Bool.false Other {state & src: List.dropFirst input}

main =
    Stdout.line "hello"
# main =
#     lettersInput = "AAAiBByAABBwBtCCCiAyArBBx"
#     ifLetterA = \l -> l == A
#     when parseStr (many letterParser) lettersInput is
#         Ok letters ->
#             letters
#             |> List.keepIf ifLetterA
#             |> List.map \_ -> 1
#             |> List.sum
#             |> Num.toStr
#             |> \countLetterA -> Stdout.line "I counted \(countLetterA) letter A's!"

#         Err _ -> Stderr.line "Ooops, something went wrong parsing letters"

# Letter : [A, B, C, Other]

# letterParser : Parser (List U8) Letter
# letterParser =
#     input <- buildPrimitiveParser

#     valResult =
#         when input is
#             [] -> Err (ParsingFailure "Nothing to parse")
#             ['A', ..] -> Ok A
#             ['B', ..] -> Ok B
#             ['C', ..] -> Ok C
#             _ -> Ok Other

#     valResult
#     |> Result.map \val -> { val, input: List.dropFirst input }

# expect
#     input = "B"
#     parser = letterParser
#     result = parseStr parser input
#     result == Ok B

# expect
#     input = "BCXA"
#     parser = many letterParser
#     result = parseStr parser input
#     result == Ok [B, C, Other, A]
