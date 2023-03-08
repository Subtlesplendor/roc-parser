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
    ]
    imports [ParserAdvanced]

# -- PARSERS ------------------

Parser value : ParserAdvanced.Parser {} Problem value

# buildPrimitiveParser : (State c -> PStep c x a) -> Parser c x a
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

# # -- PROBLEMS ------------------

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

# deadEndsToString : List DeadEnd -> Str
# deadEndsToString = \deadEnds ->
#  "TODO deadEndsToString"

# # -- PRIMITIVES ------------------

succeed : a -> Parser a
succeed = ParserAdvanced.succeed

problem : Str -> Parser a
problem = \msg ->
    ParserAdvanced.problem (Problem msg)

# # -- MAPPING ------------------

map : Parser a, (a -> b) -> Parser b
map = ParserAdvanced.map

# #According to Semantics.md, the booleans should actually compose with &&. But in the code it uses ||. What gives?
map2 : Parser a, Parser b, (a, b -> value) -> Parser value
map2 = ParserAdvanced.map2

keep : Parser (a -> b), Parser a -> Parser b
keep = ParserAdvanced.keep

skip : Parser keep, Parser ignore -> Parser keep
skip = ParserAdvanced.skip

# # -- AND THEN ------------------

andThen : Parser a, (a -> Parser b) -> Parser b
andThen = ParserAdvanced.andThen

# # -- LAZY ------------------

lazy : ({} -> Parser a) -> Parser a
lazy = ParserAdvanced.lazy

# # -- ONE OF ------------------

alt : Parser a, Parser a -> Parser a
alt = ParserAdvanced.alt

fail : Parser a
fail = ParserAdvanced.fail

## Try a list of parsers in turn, until one of them succeeds
oneOf : List (Parser a) -> Parser a
oneOf = ParserAdvanced.oneOf

# # -- LOOP ------------------

Step state a : ParserAdvanced.Step state a

loop : state, (state -> Parser (Step state a)) -> Parser a
loop = ParserAdvanced.loop

# # -- BACKTRACKABLE ------------------

backtrackable : Parser a -> Parser a
backtrackable = ParserAdvanced.backtrackable

commit : a -> Parser a
commit = ParserAdvanced.commit

# # -- SYMBOL ------------------

symbol : List U8 -> Parser {}
symbol = \lst ->
    ParserAdvanced.symbol { str: lst, prob: ExpectingSymbol lst }

# # -- KEYWORD ------------------

keyword : List U8 -> Parser {}
keyword = \kwd ->
    ParserAdvanced.symbol { str: kwd, prob: ExpectingKeyword kwd }

# # -- TOKEN ------------------

token : List U8 -> Parser {}
token = \lst ->
    ParserAdvanced.token (toToken lst)

toToken : List U8 -> ParserAdvanced.Token Problem
toToken = \lst ->
    { str: lst, prob: Expecting lst }
# # -- INT ------------------

# # -- FLOAT ------------------

# # -- NUMBER ------------------

# # -- END ------------------

end : Parser {}
end = ParserAdvanced.end ExpectingEnd

# # -- CHOMPED STRINGS -----------

getChompedString : Parser a -> Parser (List U8)
getChompedString = ParserAdvanced.getChompedString

mapChompedString : (List U8, a -> b), Parser a -> Parser b
mapChompedString = ParserAdvanced.mapChompedString

# # -- CHOMP IF -----------

# only consumes one thing. remove newOffset from issubchar?
chompIf : (U8 -> Bool) -> Parser {}
chompIf = \isGood ->
    ParserAdvanced.chompIf isGood UnexpectedChar

# # -- CHOMP WHILE -----------

# I tried to keep this faithful to the Elm library. But I think this would be better without using isSubChar, because I am kind of passing around another src without need.
chompWhile : (U8 -> Bool) -> Parser {}
chompWhile = ParserAdvanced.chompWhile

# # -- CHOMP UNTIL -----------

chompUntil : List U8 -> Parser {}
chompUntil = \str ->
    ParserAdvanced.chompUntil (toToken str)

chompUntilEndOr : List U8 -> Parser {}
chompUntilEndOr =
    ParserAdvanced.chompUntilEndOr

# # -- CONTEXT -----------

# inContext : context, Parser context x a -> Parser context x a
# inContext = \con, @Parser parse ->
#     @Parser \s0 ->
#         stateInContext =
#             s0.context
#             |> List.prepend {row: s0.row, col: s0.col, context: con}
#             |> changeContext s0

#         when parse stateInContext is
#             Good p a s1 ->
#                 Good p a (changeContext s0.context s1)

#             Bad _ _ as step ->
#                 step

# changeContext : List (Located c), State c -> State c
# changeContext = \newContext, s ->
#     { s & context: newContext }

# # -- INDENTATION -----------

getIndent : Parser Nat
getIndent =
    ParserAdvanced.getIndent

withIndent : Nat, Parser a -> Parser a
withIndent =
    ParserAdvanced.withIndent

# # -- POSITION -----------

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

# # -- LOW LEVEL HELPERS -----------

# #These are most likely to be optimizable

# Position : {offset: Nat, row: Nat, col: Nat}

# newLine: U8
# newLine = 10

# posUpdate: Position, U8 -> Position
# posUpdate = \pos, c ->
#     if c == newLine then
#         {offset: pos.offset + 1, row: pos.row + 1, col: 1}
#     else
#         {pos & offset: pos.offset + 1, col: pos.col + 1}

# #This is missnamed.
# isSubString : List U8, Position, List U8 -> Result Position [NotFound, OutOfBounds]
# isSubString =\smallLst, pos, bigLst ->
#     if pos.offset + List.len smallLst <= List.len bigLst then

#         smallLst |> List.walkTry pos \p, c ->
#             char <- Result.try (List.get bigLst p.offset)

#             if c == char then
#                 Ok (pos |> posUpdate c)
#             else
#                 Err NotFound

#     else
#         Err NotFound

# expect
#     p = {offset: 0, row: 1, col: 1}
#     smallLst = Str.toUtf8 "Hell"
#     bigLst = Str.toUtf8 "Hello there"
#     pos = Result.withDefault (isSubString smallLst p bigLst) p
#     pos == {offset: 4, row: 1, col: 5}

# expect
#     p = {offset: 0, row: 2, col: 4}
#     smallLst = Str.toUtf8 "Hello\n there"
#     bigLst = Str.toUtf8 "Hello\n there neighbour"
#     pos = Result.withDefault (isSubString smallLst p bigLst) p
#     pos == {offset: 12, row: 3, col: 7}

# expect
#     p = {offset: 4, row: 2, col: 5}
#     smallLst = Str.toUtf8 "now"
#     bigLst = Str.toUtf8 "Hi\n now"
#     pos = Result.withDefault (isSubString smallLst p bigLst) p
#     pos == {offset: 7, row: 2, col: 8}

# expect
#     p = {offset: 4, row: 2, col: 5}
#     smallLst = Str.toUtf8 "now and again"
#     bigLst = Str.toUtf8 "Hi\n now"
#     res = (isSubString smallLst p bigLst)
#     when res is
#         Err NotFound -> Bool.true
#         _ -> Bool.false

# expect
#     p = {offset: 0, row: 1, col: 1}
#     smallLst = Str.toUtf8 "ice"
#     bigLst = Str.toUtf8 "Hello\n there neighbour"
#     res = (isSubString smallLst p bigLst)
#     when res is
#         Err NotFound -> Bool.true
#         _ -> Bool.false

# isSubChar: (U8 -> Bool), Nat, List U8 -> Result Nat [NotFound, NewLine, OutOfBounds]
# isSubChar = \predicate, offset, lst ->
#     char <- Result.try (List.get lst offset)
#     if predicate char then
#         Ok (offset + 1)
#     else if char==newLine then
#         Err NewLine
#     else
#         Err NotFound

# #TODO
# isAsciiCode: Nat, Nat, List U8 -> Bool

# findSubString : List U8, Position, List U8 -> Result Position [EndOfList Position]
# findSubString = \smallLst, pos, bigLst ->
#     smallLen = List.len smallLst

#     finalPos =
#         bigLst |> List.walkFromUntil pos.offset pos \p,c ->
#             sbList = List.sublist bigLst {start: p.offset, len: smallLen}

#             newPos = pos |> posUpdate c
#             if smallLst == sbList then
#                 Break newPos
#             else
#                 Continue newPos

#     if finalPos.offset == List.len bigLst then
#         Err (EndOfList finalPos)
#     else
#         Ok finalPos

# # -- VARIABLES -----------

# # -- SEQUENCES -----------

# # -- WHITESPACE -----------
