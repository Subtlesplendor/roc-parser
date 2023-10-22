interface Parser.Bytes
    exposes [Parser, DeadEnd, #Types
             buildPrimitiveParser,
             run, #Operating
             const, fail, problem, end, token, #Primitives
             map, map2, keep, skip, andThen, flatten, #Combinators
             lazy, many, oneOrMore, alt, oneOf, between, sepBy, ignore, #Combinators
             chompIf, chompWhile, chompUntil, chompUntilEndOr, getChompedBytes, mapChompedBytes, #Chompers
             getOffset, getSource, # Info
             backtrackable, commit, # Backtracking
             loop, # Looping
             chompBytes, chompByte
             ]
    imports [Parser.Advanced.Bytes]


# -- PARSERS ------------------

Byte: Parser.Advanced.Bytes.Byte
ByteList: Parser.Advanced.Bytes.ByteList

Parser value : Parser.Advanced.Bytes.Parser Context Problem value

DeadEnd: Parser.Advanced.Bytes.DeadEnd Context Problem

Context : {}

# -- PROBLEMS ------------------

Problem : [
    UnexpectedByte, 
    Expecting ByteList,
    ParsingFailure Str,
    ExpectingEnd,
]

# -- RUN ------------------
# To be refactored: do not reference internal types.
buildPrimitiveParser = Parser.Advanced.Bytes.buildPrimitiveParser

run : Parser a, ByteList -> Result a (List DeadEnd)
run = \parser, input ->
    when Parser.Advanced.Bytes.run parser input is
        Ok value ->
            Ok value

        Err problems ->
            Err (problems |> List.map problemToDeadEnd)

problemToDeadEnd : Parser.Advanced.Bytes.DeadEnd Context _ -> DeadEnd
problemToDeadEnd = \d ->
    { offset: d.offset, problem: d.problem, contextStack: [] }

# -- PRIMITIVES -----------

const : v -> Parser v
const =\value ->
    Parser.Advanced.Bytes.const value

problem : Str -> Parser *
problem = \msg -> 
     Parser.Advanced.Bytes.problem (ParsingFailure msg)

fail : Parser *
fail = 
    Parser.Advanced.Bytes.fail
    
end: Parser {}
end = 
    Parser.Advanced.Bytes.end ExpectingEnd  


# -- COMBINATORS ----------

map: Parser a, (a -> b) -> Parser b
map = \parser, mapper ->
    Parser.Advanced.Bytes.map parser mapper

map2: Parser a, Parser b, (a, b -> d) -> Parser d
map2 = \first, second, mapper ->
    Parser.Advanced.Bytes.map2 first second mapper

keep: Parser (a -> b), Parser a -> Parser b        
keep = \parserFunc, parser ->
    Parser.Advanced.Bytes.keep parserFunc parser

skip: Parser keep, Parser ignore -> Parser keep
skip = \parserKeep, parserSkip ->
    Parser.Advanced.Bytes.skip parserKeep parserSkip

andThen: Parser a, (a -> Parser b) -> Parser b
andThen = \parser, parserBuilder ->
    Parser.Advanced.Bytes.andThen parser parserBuilder

alt: Parser v, Parser v -> Parser v
alt = \first, second ->
    Parser.Advanced.Bytes.alt first second          

oneOf : List (Parser v) -> Parser v
oneOf = \parsers ->
    Parser.Advanced.Bytes.oneOf parsers

lazy : ({} -> Parser v) -> Parser v
lazy = \thunk -> 
    Parser.Advanced.Bytes.lazy thunk

many : Parser v -> Parser (List v)
many = \parser ->
    Parser.Advanced.Bytes.many parser

oneOrMore : Parser v -> Parser (List v)
oneOrMore = \parser ->
    Parser.Advanced.Bytes.oneOrMore parser   

between : Parser v, Parser *, Parser * -> Parser v
between = \parser, open, close ->
    Parser.Advanced.Bytes.between parser open close      

sepBy : Parser v, Parser * -> Parser (List v)
sepBy = \parser, separator ->
    Parser.Advanced.Bytes.sepBy parser separator


ignore : Parser v -> Parser {}
ignore = \parser ->
    Parser.Advanced.Bytes.ignore parser

flatten : Parser (Result v Problem) -> Parser v
flatten = \parser ->
    Parser.Advanced.Bytes.flatten parser

# ---- CHOMPERS -------

chompIf: (Byte -> Bool) -> Parser {}
chompIf = \isGood ->
    Parser.Advanced.Bytes.chompIf isGood UnexpectedByte     


getChompedBytes : Parser * -> Parser ByteList
getChompedBytes = \parser ->
    Parser.Advanced.Bytes.getChompedBytes parser

mapChompedBytes : Parser a, (ByteList, a -> b) -> Parser b
mapChompedBytes = \parser, mapper ->
    Parser.Advanced.Bytes.mapChompedBytes parser mapper
       

chompWhile: (Byte -> Bool) -> Parser {}
chompWhile = \isGood ->
    Parser.Advanced.Bytes.chompWhile isGood


chompUntil : ByteList -> Parser {}
chompUntil = \bytelist -> 
    Parser.Advanced.Bytes.chompUntil (toToken bytelist)


chompUntilEndOr : ByteList -> Parser {}
chompUntilEndOr = \bytelist ->
    Parser.Advanced.Bytes.chompUntilEndOr bytelist


# -- LOOP ---------

Step state a : Parser.Advanced.Bytes.Step state a

loop : state, (state -> Parser (Step state a)) -> Parser a
loop =  \state, callback ->
    Parser.Advanced.Bytes.loop state callback


# -- BACKTRACKABLE ---------

backtrackable : Parser a -> Parser a
backtrackable = \parser ->
    Parser.Advanced.Bytes.backtrackable parser

commit : a -> Parser a
commit = \value ->
    Parser.Advanced.Bytes.commit value

# -- POSITION

getOffset: Parser Nat
getOffset =
    Parser.Advanced.Bytes.getOffset

getSource: Parser ByteList
getSource =
    Parser.Advanced.Bytes.getSource

# -- TOKEN

token : ByteList -> Parser {}
token = \bytelist ->
    Parser.Advanced.Bytes.token (bytelist |> toToken)

toToken: ByteList -> Parser.Advanced.Bytes.Token Problem
toToken = \tok ->
    {tok, expecting: Expecting tok}


# -- Byte specific

chompBytes :  ByteList -> Parser {}
chompBytes = \bytelist ->
    token bytelist

chompByte : Byte -> Parser {}
chompByte = \b ->
    chompIf (\x -> x == b)
