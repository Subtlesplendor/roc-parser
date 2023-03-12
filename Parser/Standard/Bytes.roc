interface Parser.Standard.Bytes
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
    imports [Parser.Advanced.Bytes]

# -- PARSERS ------------------

Parser value : Parser.Advanced.Bytes.Parser {} Problem value

# buildPrimitiveParser : (State c -> PStep c x a) -> Parser c x a
buildPrimitiveParser = Parser.Advanced.Bytes.buildPrimitiveParser

# -- RUN ------------------

run : Parser a, List U8 -> Result a (List DeadEnd)
run = \parser, input ->
    when Parser.Advanced.Bytes.run parser input is
        Ok value ->
            Ok value

        Err problems ->
            Err (problems |> List.map problemToDeadEnd)

problemToDeadEnd : Parser.Advanced.Bytes.DeadEnd {} Problem -> DeadEnd
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
succeed = Parser.Advanced.Bytes.succeed

problem : Str -> Parser a
problem = \msg ->
    Parser.Advanced.Bytes.problem (Problem msg)

# # -- MAPPING ------------------

map : Parser a, (a -> b) -> Parser b
map = Parser.Advanced.Bytes.map

# #According to Semantics.md, the booleans should actually compose with &&. But in the code it uses ||. What gives?
map2 : Parser a, Parser b, (a, b -> value) -> Parser value
map2 = Parser.Advanced.Bytes.map2

keep : Parser (a -> b), Parser a -> Parser b
keep = Parser.Advanced.Bytes.keep

skip : Parser keep, Parser ignore -> Parser keep
skip = Parser.Advanced.Bytes.skip

# # -- AND THEN ------------------

andThen : Parser a, (a -> Parser b) -> Parser b
andThen = Parser.Advanced.Bytes.andThen

# # -- LAZY ------------------

lazy : ({} -> Parser a) -> Parser a
lazy = Parser.Advanced.Bytes.lazy

# # -- ONE OF ------------------

alt : Parser a, Parser a -> Parser a
alt = Parser.Advanced.Bytes.alt

fail : Parser a
fail = Parser.Advanced.Bytes.fail

## Try a list of parsers in turn, until one of them succeeds
oneOf : List (Parser a) -> Parser a
oneOf = Parser.Advanced.Bytes.oneOf

# # -- LOOP ------------------

Step state a : Parser.Advanced.Bytes.Step state a

loop : state, (state -> Parser (Step state a)) -> Parser a
loop = Parser.Advanced.Bytes.loop

# # -- BACKTRACKABLE ------------------

backtrackable : Parser a -> Parser a
backtrackable = Parser.Advanced.Bytes.backtrackable

commit : a -> Parser a
commit = Parser.Advanced.Bytes.commit

# # -- SYMBOL ------------------

symbol : List U8 -> Parser {}
symbol = \lst ->
    Parser.Advanced.Bytes.symbol { str: lst, prob: ExpectingSymbol lst }

# # -- KEYWORD ------------------

keyword : List U8 -> Parser {}
keyword = \kwd ->
    Parser.Advanced.Bytes.symbol { str: kwd, prob: ExpectingKeyword kwd }

# # -- TOKEN ------------------

token : List U8 -> Parser {}
token = \lst ->
    Parser.Advanced.Bytes.token (toToken lst)

toToken : List U8 -> Parser.Advanced.Bytes.Token Problem
toToken = \lst ->
    { str: lst, prob: Expecting lst }
# # -- INT ------------------

# # -- FLOAT ------------------

# # -- NUMBER ------------------

# # -- END ------------------

end : Parser {}
end = Parser.Advanced.Bytes.end ExpectingEnd

# # -- CHOMPED STRINGS -----------

getChompedString : Parser a -> Parser (List U8)
getChompedString = Parser.Advanced.Bytes.getChompedString

mapChompedString : (List U8, a -> b), Parser a -> Parser b
mapChompedString = Parser.Advanced.Bytes.mapChompedString

# # -- CHOMP IF -----------

# only consumes one thing. remove newOffset from issubchar?
chompIf : (U8 -> Bool) -> Parser {}
chompIf = \isGood ->
    Parser.Advanced.Bytes.chompIf isGood UnexpectedChar

# # -- CHOMP WHILE -----------

# I tried to keep this faithful to the Elm library. But I think this would be better without using isSubChar, because I am kind of passing around another src without need.
chompWhile : (U8 -> Bool) -> Parser {}
chompWhile = Parser.Advanced.Bytes.chompWhile

# # -- CHOMP UNTIL -----------

chompUntil : List U8 -> Parser {}
chompUntil = \str ->
    Parser.Advanced.Bytes.chompUntil (toToken str)

chompUntilEndOr : List U8 -> Parser {}
chompUntilEndOr =
    Parser.Advanced.Bytes.chompUntilEndOr

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
    Parser.Advanced.Bytes.getIndent

withIndent : Nat, Parser a -> Parser a
withIndent =
    Parser.Advanced.Bytes.withIndent

# # -- POSITION -----------

# This name is confusing due to my definition of Position.
getPosition : Parser { row : Nat, col : Nat }
getPosition =
    Parser.Advanced.Bytes.getPosition

getRow : Parser Nat
getRow =
    Parser.Advanced.Bytes.getRow

getCol : Parser Nat
getCol =
    Parser.Advanced.Bytes.getCol

getOffset : Parser Nat
getOffset =
    Parser.Advanced.Bytes.getOffset

getSource : Parser (List U8)
getSource =
    Parser.Advanced.Bytes.getSource

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
