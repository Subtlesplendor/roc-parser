interface Parser
    exposes [
        Parser,
        DeadEnd,
        run,
        succeed,
        problem,
        map,
        map2,
        fail,
        keep,
        skip,
        andThen,
        lazy,
        alt,
        oneOf,
        loop,
        commit,
        backtrackable,
        buildPrimitiveParser,
        symbol,
        keyword,
        end,
        getChompedString,
        mapChompedString,
        chompIf,
        chompWhile,
        chompUntil,
        chompUntilEndOr,
        getIndent,
        withIndent,
        token,
        Step,
        getPosition,
        getCol,
        getRow,
        getSource,
        getOffset,
        spaces
    ]
    imports [ParserAdvanced]

# -- PARSERS ------------------

Parser value : ParserAdvanced.Parser {} Problem value

buildPrimitiveParser = ParserAdvanced.buildPrimitiveParser

# -- RUN ------------------

run : Parser a, List U8 -> Result a (List DeadEnd)
run = \parser, input ->
    when ParserAdvanced.run parser input is
        Ok value ->
            Ok value

        Err problems ->
            Err (problems |> List.map problemToDeadEnd)

problemToDeadEnd : ParserAdvanced.DeadEnd {} Problem -> DeadEnd
problemToDeadEnd = \p ->
    { row: p.row, col: p.col, problem: p.problem }

# -- PROBLEMS ------------------

DeadEnd : {
    row : Nat,
    col : Nat,
    problem : Problem,
}

Problem : [
    Expecting (List U8),
    ExpectingInt,
    ExpectingHex,
    ExpectingOctal,
    ExpectingBinary,
    ExpectingFloat,
    ExpectingNumber,
    ExpectingVariable,
    ExpectingSymbol (List U8),
    ExpectingKeyword (List U8),
    ExpectingEnd,
    UnexpectedChar,
    Problem Str,
    BadRepeat,
]

# -- PRIMITIVES ------------------

succeed : a -> Parser a
succeed = ParserAdvanced.succeed

problem : Str -> Parser a
problem = \msg ->
    ParserAdvanced.problem (Problem msg)

# -- MAPPING ------------------

map : Parser a, (a -> b) -> Parser b
map = ParserAdvanced.map

map2 : Parser a, Parser b, (a, b -> value) -> Parser value
map2 = ParserAdvanced.map2

keep : Parser (a -> b), Parser a -> Parser b
keep = ParserAdvanced.keep

skip : Parser keep, Parser ignore -> Parser keep
skip = ParserAdvanced.skip

# -- AND THEN ------------------

andThen : Parser a, (a -> Parser b) -> Parser b
andThen = ParserAdvanced.andThen

# -- LAZY ------------------

lazy : ({} -> Parser a) -> Parser a
lazy = ParserAdvanced.lazy

# -- ONE OF ------------------

alt : Parser a, Parser a -> Parser a
alt = ParserAdvanced.alt

fail : Parser a
fail = ParserAdvanced.fail

oneOf : List (Parser a) -> Parser a
oneOf = ParserAdvanced.oneOf

# -- LOOP ------------------

Step state a : ParserAdvanced.Step state a

loop : state, (state -> Parser (Step state a)) -> Parser a
loop = ParserAdvanced.loop

# -- BACKTRACKABLE ------------------

backtrackable : Parser a -> Parser a
backtrackable = ParserAdvanced.backtrackable

commit : a -> Parser a
commit = ParserAdvanced.commit

# -- SYMBOL ------------------

symbol : List U8 -> Parser {}
symbol = \lst ->
    ParserAdvanced.symbol { str: lst, prob: ExpectingSymbol lst }

# -- KEYWORD ------------------

keyword : List U8 -> Parser {}
keyword = \kwd ->
    ParserAdvanced.symbol { str: kwd, prob: ExpectingKeyword kwd }

# -- TOKEN ------------------

token : List U8 -> Parser {}
token = \lst ->
    ParserAdvanced.token (toToken lst)

toToken : List U8 -> ParserAdvanced.Token Problem
toToken = \lst ->
    { str: lst, prob: Expecting lst }
# -- INT ------------------

# -- FLOAT ------------------

# -- NUMBER ------------------

# -- END ------------------

end : Parser {}
end = ParserAdvanced.end ExpectingEnd

# # -- CHOMPED STRINGS -----------

getChompedString : Parser a -> Parser (List U8)
getChompedString = ParserAdvanced.getChompedString

mapChompedString : (List U8, a -> b), Parser a -> Parser b
mapChompedString = ParserAdvanced.mapChompedString

# -- CHOMP IF -----------

chompIf : (U8 -> Bool) -> Parser {}
chompIf = \isGood ->
    ParserAdvanced.chompIf isGood UnexpectedChar

# -- CHOMP WHILE -----------

chompWhile : (U8 -> Bool) -> Parser {}
chompWhile = ParserAdvanced.chompWhile

# -- CHOMP UNTIL -----------

chompUntil : List U8 -> Parser {}
chompUntil = \str ->
    ParserAdvanced.chompUntil (toToken str)

chompUntilEndOr : List U8 -> Parser {}
chompUntilEndOr =
    ParserAdvanced.chompUntilEndOr


# -- INDENTATION -----------

getIndent : Parser Nat
getIndent =
    ParserAdvanced.getIndent

withIndent : Nat, Parser a -> Parser a
withIndent =
    ParserAdvanced.withIndent

# -- POSITION -----------

# This name is confusing due to my definition of Position.
getPosition : Parser { row : Nat, col : Nat }
getPosition =
    ParserAdvanced.getPosition

getRow : Parser Nat
getRow =
    ParserAdvanced.getRow

getCol : Parser Nat
getCol =
    ParserAdvanced.getCol

getOffset : Parser Nat
getOffset =
    ParserAdvanced.getOffset

getSource : Parser (List U8)
getSource =
    ParserAdvanced.getSource

# -- VARIABLES -----------

# -- SEQUENCES -----------

# -- WHITESPACE -----------

spaces : Parser {}
spaces =
    ParserAdvanced.spaces
