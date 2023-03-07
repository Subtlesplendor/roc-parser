app "example"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.2.0/8tCohJeXMBUnjo_zdMq0jSaqdYoCWJkWazBd4wa8cQU.tar.br",
        parser: "../package/main.roc"
    }
    imports [
        cli.Stdout,
        cli.Stderr,
        parser.ParserAdvanced.{ Parser, PStep, State, fromState, buildPrimitiveParser }
    ]
    provides [main] to cli

Letter : [A, B, C, Other]

Problem : [ParsingFailure Str]

U8Parser a : Parser {} (List U8) Problem a

letterParser : U8Parser Letter
letterParser =
    buildPrimitiveParser \state ->
        input = state.src
        when input is
            [] -> Bad Bool.false (fromState state (ParsingFailure "Nothing to parse"))
            ['A', ..] -> Good Bool.false A {state & src: List.dropFirst input}
            ['B', ..] -> Good Bool.false B {state & src: List.dropFirst input}
            ['C', ..] -> Good Bool.false C {state & src: List.dropFirst input}
            _ -> Good Bool.false Other {state & src: List.dropFirst input}

main =
    Stdout.line "hello"
