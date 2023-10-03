interface Parser.Utf8
    exposes [Parser, DeadEnd, RawStr, RawChar, #Types
             buildPrimitiveParser,
             run, #Operating
             const, fail, problem, end, token, #Primitives
             map, map2, keep, skip, andThen, flatten, #Combinators
             lazy, many, oneOrMore, alt, oneOf, between, sepBy, ignore, #Combinators
             chompIf, chompWhile, chompUntil, chompUntilEndOr, getChompedRawStr, mapChompedRawStr, #Chompers
             getOffset, getSource, # Info
             backtrackable, commit, # Backtracking
             loop, # Looping
             chompString, chompChar, keyword, string
             ]
    imports [Parser.Advanced.Utf8.{}]

# -- PARSERS ------------------

RawChar: Parser.Advanced.Utf8.RawChar
RawStr: Parser.Advanced.Utf8.RawStr

Parser value : Parser.Advanced.Utf8.Parser Context Problem value

DeadEnd: Parser.Advanced.Utf8.DeadEnd Context Problem

Context : {}

# -- PROBLEMS ------------------

Problem : [
    UnexpectedRawChar, 
    Expecting RawStr,
    ExpectingKeyword RawStr,
    ParsingFailure Str,
    ExpectingEnd,
]

# -- RUN ------------------

# To be refactored: do not reference internal types.
buildPrimitiveParser = Parser.Advanced.Utf8.buildPrimitiveParser

run : Parser a, RawStr -> Result a (List DeadEnd)
run = \parser, input ->
    when Parser.Advanced.Utf8.run parser input is
        Ok value ->
            Ok value

        Err problems ->
            Err (problems |> List.map problemToDeadEnd)

problemToDeadEnd : Parser.Advanced.Utf8.DeadEnd Context _ -> DeadEnd
problemToDeadEnd = \d ->
    { offset: d.offset, problem: d.problem, contextStack: [] }

# -- PRIMITIVES -----------

const : v -> Parser v
const = \value ->
    Parser.Advanced.Utf8.const value

problem : Str -> Parser *
problem = \msg -> 
     Parser.Advanced.Utf8.problem (ParsingFailure msg)

fail : Parser *
fail = 
    Parser.Advanced.Utf8.fail
    
end: Parser {}
end = 
    Parser.Advanced.Utf8.end ExpectingEnd  


# -- COMBINATORS ----------

map: Parser a, (a -> b) -> Parser b
map = \parser, mapper ->
    Parser.Advanced.Utf8.map parser mapper 

map2: Parser a, Parser b, (a, b -> d) -> Parser d
map2 = \first, second, mapper ->
    Parser.Advanced.Utf8.map2 first second mapper

keep: Parser (a -> b), Parser a -> Parser b        
keep = \parserFunc, parser ->
    Parser.Advanced.Utf8.keep parserFunc parser

skip: Parser keep, Parser ignore -> Parser keep
skip = \parserKeep, parserSkip ->
    Parser.Advanced.Utf8.skip parserKeep parserSkip

andThen: Parser a, (a -> Parser b) -> Parser b
andThen = \parser, parserBuilder ->
    Parser.Advanced.Utf8.andThen parser parserBuilder

alt: Parser v, Parser v -> Parser v
alt = \first, second  ->
    Parser.Advanced.Utf8.alt first second       

oneOf : List (Parser v) -> Parser v
oneOf = \parsers ->
    Parser.Advanced.Utf8.oneOf parsers

lazy : ({} -> Parser v) -> Parser v
lazy = \thunk ->
    Parser.Advanced.Utf8.lazy thunk 


many : Parser v -> Parser (List v)
many = \parser ->
    Parser.Advanced.Utf8.many parser

oneOrMore : Parser v -> Parser (List v)
oneOrMore = \parser ->
    Parser.Advanced.Utf8.oneOrMore parser  

between : Parser v, Parser *, Parser * -> Parser v
between = \parser, open, close ->
    Parser.Advanced.Utf8.between parser open close          

sepBy : Parser v, Parser * -> Parser (List v)
sepBy = \parser, separator ->
    Parser.Advanced.Utf8.sepBy parser separator


ignore : Parser v -> Parser {}
ignore = \parser ->
    Parser.Advanced.Utf8.ignore parser    


flatten : Parser (Result v Problem) -> Parser v
flatten = \parser ->
    Parser.Advanced.Utf8.flatten parser

# ---- CHOMPERS -------

chompIf: (RawChar -> Bool) -> Parser {}
chompIf = \isGood ->
    Parser.Advanced.Utf8.chompIf isGood UnexpectedRawChar     


getChompedRawStr : Parser * -> Parser RawStr
getChompedRawStr = \parser ->
    Parser.Advanced.Utf8.getChompedRawStr parser

mapChompedRawStr : Parser a, (RawStr, a -> b) -> Parser b
mapChompedRawStr = \parser, mapper ->
    Parser.Advanced.Utf8.mapChompedRawStr parser mapper
       

chompWhile: (RawChar -> Bool) -> Parser {}
chompWhile = \isGood ->
    Parser.Advanced.Utf8.chompWhile isGood


chompUntil : RawStr -> Parser {}
chompUntil = \tok ->  
    Parser.Advanced.Utf8.chompUntil (toToken tok)


chompUntilEndOr : RawStr -> Parser {}
chompUntilEndOr = \raw ->
    Parser.Advanced.Utf8.chompUntilEndOr raw


# -- LOOP ---------

Step state a : Parser.Advanced.Utf8.Step state a

loop : state, (state -> Parser (Step state a)) -> Parser a
loop = \state, callback ->
    Parser.Advanced.Utf8.loop state callback


# -- BACKTRACKABLE ---------

backtrackable : Parser a -> Parser a
backtrackable = \parser -> 
    Parser.Advanced.Utf8.backtrackable parser

commit : a -> Parser a
commit = \value ->
    Parser.Advanced.Utf8.commit value

# -- POSITION

getOffset: Parser Nat
getOffset =
    Parser.Advanced.Utf8.getOffset

getSource: Parser RawStr
getSource =
    Parser.Advanced.Utf8.getSource

# -- TOKEN

token : RawStr -> Parser {}
token = \tok ->
    Parser.Advanced.Utf8.token (tok |> toToken)

toToken: RawStr -> Parser.Advanced.Utf8.Token Problem
toToken = \tok ->
    {tok, expecting: Expecting tok}


# Utf8 specific

separators = [' ', '\n']

keyword: RawStr -> Parser {}
keyword = \rawstr ->
    Parser.Advanced.Utf8.keyword separators {tok: rawstr, expecting: ExpectingKeyword rawstr}

chompString :  RawStr -> Parser {}
chompString = \raw ->
    token raw

chompChar : RawChar -> Parser {}
chompChar = \b ->
    chompIf (\x -> x == b)



rwstr : RawStr -> Parser RawStr
rwstr = \raw ->
    chompString raw
    |> getChompedRawStr

string: Str -> Parser Str
string = \str ->
    strToRawStr str
    |> rwstr
    |> map rawStrToStr
    |> flatten



# --- Internal -------

rawStrToStr : RawStr -> Result Str Problem
rawStrToStr = \raw ->
    _ <- Result.onErr (Str.fromUtf8 raw)
    Err (ParsingFailure "Failed to create Str from raw string (List U8).")

strToRawStr : Str -> RawStr 
strToRawStr = \str ->
    Str.toUtf8 str  

       