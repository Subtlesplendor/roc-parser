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

#const : v -> Parser * v
const = Parser.Advanced.Utf8.const

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
map2 = 
    Parser.Advanced.Utf8.map2

keep: Parser (a -> b), Parser a -> Parser b        
keep = 
    Parser.Advanced.Utf8.keep

skip: Parser keep, Parser ignore -> Parser keep
skip = 
    Parser.Advanced.Utf8.skip

andThen: Parser a, (a -> Parser b) -> Parser b
andThen = 
    Parser.Advanced.Utf8.andThen

alt: Parser v, Parser v -> Parser v
alt = 
    Parser.Advanced.Utf8.alt          

oneOf : List (Parser v) -> Parser v
oneOf = 
    Parser.Advanced.Utf8.oneOf    

lazy : ({} -> Parser v) -> Parser v
lazy = 
    Parser.Advanced.Utf8.lazy     


many : Parser v -> Parser (List v)
many = 
    Parser.Advanced.Utf8.many

oneOrMore : Parser v -> Parser (List v)
oneOrMore = 
    Parser.Advanced.Utf8.oneOrMore    

between : Parser v, Parser *, Parser * -> Parser v
between = 
    Parser.Advanced.Utf8.between           

sepBy : Parser v, Parser * -> Parser (List v)
sepBy = 
    Parser.Advanced.Utf8.sepBy


ignore : Parser v -> Parser {}
ignore = 
    Parser.Advanced.Utf8.ignore     


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
chompWhile = 
    Parser.Advanced.Utf8.chompWhile


chompUntil : RawStr -> Parser {}
chompUntil = \tok ->  
    Parser.Advanced.Utf8.chompUntil (toToken tok)


chompUntilEndOr : RawStr -> Parser {}
chompUntilEndOr = 
    Parser.Advanced.Utf8.chompUntilEndOr


# -- LOOP ---------

Step state a : Parser.Advanced.Utf8.Step state a

loop : state, (state -> Parser (Step state a)) -> Parser a
loop = 
    Parser.Advanced.Utf8.loop


# -- BACKTRACKABLE ---------

backtrackable : Parser a -> Parser a
backtrackable = 
    Parser.Advanced.Utf8.backtrackable

commit : a -> Parser a
commit = 
    Parser.Advanced.Utf8.commit

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
chompString =
    token

chompChar : RawChar -> Parser {}
chompChar = \b ->
    chompIf (\x -> x == b)



rwstr : RawStr -> Parser RawStr
rwstr = \raw ->
    chompString raw
    |> getChompedRawStr

string: Str -> Parser Str
string = \str ->
    raw <- Result.try (strToRawStr str)
    rwstr raw
    |> map rawStrToStr
    |> flatten



# --- Internal -------

rawStrToStr : RawStr -> Result Str Problem
rawStrToStr = \raw ->
    _ <- Result.onErr (Str.fromUtf8 raw)
    Err (ParsingFailure "Failed to create Str from raw string (List U8).")

strToRawStr : Str -> Result RawStr Problem
strToRawStr = \str ->
    _ <- Result.onErr (Str.toUtf8 str)
    Err (ParsingFailure "Failed to create raw string (List U8) from Str.")    

       