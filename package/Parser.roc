interface Parser 
    exposes []
    imports [Parser.Advanced.{}]


# -- PARSERS ------------------

Parser input value : Parser.Advanced.Parser {} input Problem value

DeadEnd i: Parser.Advanced.DeadEnd {} (Problem i)

buildPrimitiveParser = Parser.Advanced.buildPrimitiveParser

# -- RUN ------------------

run : Parser i a, List i -> Result a (List (DeadEnd i))
run = \parser, input ->
    when Parser.Advanced.run parser input is
        Ok value ->
            Ok value

        Err problems ->
            Err (problems |> List.map problemToDeadEnd)

problemToDeadEnd : Parser.Advanced.DeadEnd {} _ -> DeadEnd i
problemToDeadEnd = \d ->
    { offset: d.offset, problem: d.problem, contextStack: d.contextStack }

# -- PROBLEMS ------------------

Problem i : [
    Expecting (List i),
    ExpectingSymbol (List i),
    Problem Str,
]
