interface Parser 
    exposes [Parser, DeadEnd, #Types
             buildPrimitiveParser,
             run, #Operating
             #const, fail, problem, end, symbol, #Primitives
             #map, map2, keep, skip, andThen, #Combinators
             #lazy, many, oneOrMore, alt, oneOf, between, sepBy, #ignore, #Combinators
             #chompIf, chompWhile, chompUntil, chompUntilEndOr, #getChompedSource, #Chompers
             #getOffset, getSource, # Info
             #inContext, # Context
             #backtrackable, commit, # Backtracking
             #loop, # Looping
             ]
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
