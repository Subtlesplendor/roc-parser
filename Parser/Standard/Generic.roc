interface Parser.Standard.Generic 
    exposes []
    imports [Parser.Advanced.Generic.{}]


# -- PARSERS ------------------

Parser input value : Parser.Advanced.Generic.Parser {} input Problem value

DeadEnd i: Parser.Advanced.Generic.DeadEnd {} (Problem i)

buildPrimitiveParser = Parser.Advanced.Generic.buildPrimitiveParser

# -- RUN ------------------

run : Parser i a, List i -> Result a (List (DeadEnd i))
run = \parser, input ->
    when Parser.Advanced.Generic.run parser input is
        Ok value ->
            Ok value

        Err problems ->
            Err (problems |> List.map problemToDeadEnd)

problemToDeadEnd : Parser.Advanced.Generic.DeadEnd {} _ -> DeadEnd i
problemToDeadEnd = \d ->
    { offset: d.offset, problem: d.problem, contextStack: d.contextStack }

# -- PROBLEMS ------------------

Problem i : [
    Expecting (List i),
    ExpectingSymbol (List i),
    Problem Str,
]
