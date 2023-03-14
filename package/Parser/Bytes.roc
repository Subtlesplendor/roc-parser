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
    imports [Parser.Advanced.Bytes.{}]


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

#const : v -> Parser * v
const = Parser.Advanced.Bytes.const

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
map = 
    Parser.Advanced.Bytes.map    

map2: Parser a, Parser b, (a, b -> d) -> Parser d
map2 = 
    Parser.Advanced.Bytes.map2

keep: Parser (a -> b), Parser a -> Parser b        
keep = 
    Parser.Advanced.Bytes.keep

skip: Parser keep, Parser ignore -> Parser keep
skip = 
    Parser.Advanced.Bytes.skip

andThen: Parser a, (a -> Parser b) -> Parser b
andThen = 
    Parser.Advanced.Bytes.andThen

alt: Parser v, Parser v -> Parser v
alt = 
    Parser.Advanced.Bytes.alt          

oneOf : List (Parser v) -> Parser v
oneOf = 
    Parser.Advanced.Bytes.oneOf    

lazy : ({} -> Parser v) -> Parser v
lazy = 
    Parser.Advanced.Bytes.lazy     


many : Parser v -> Parser (List v)
many = 
    Parser.Advanced.Bytes.many

oneOrMore : Parser v -> Parser (List v)
oneOrMore = 
    Parser.Advanced.Bytes.oneOrMore    

between : Parser v, Parser *, Parser * -> Parser v
between = 
    Parser.Advanced.Bytes.between           

sepBy : Parser v, Parser * -> Parser (List v)
sepBy = 
    Parser.Advanced.Bytes.sepBy


ignore : Parser v -> Parser {}
ignore = 
    Parser.Advanced.Bytes.ignore     


flatten : Parser (Result v Problem) -> Parser v
flatten = 
    Parser.Advanced.Bytes.flatten

# ---- CHOMPERS -------

chompIf: (Byte -> Bool) -> Parser {}
chompIf = \isGood ->
    Parser.Advanced.Bytes.chompIf isGood UnexpectedByte     


getChompedBytes : Parser * -> Parser ByteList
getChompedBytes = 
    Parser.Advanced.Bytes.getChompedBytes

mapChompedBytes : Parser a, (ByteList, a -> b) -> Parser b
mapChompedBytes = 
    Parser.Advanced.Bytes.mapChompedBytes
       

chompWhile: (Byte -> Bool) -> Parser {}
chompWhile = 
    Parser.Advanced.Bytes.chompWhile


chompUntil : ByteList -> Parser {}
chompUntil = \tok ->  
    Parser.Advanced.Bytes.chompUntil (toToken tok)


chompUntilEndOr : ByteList -> Parser {}
chompUntilEndOr = 
    Parser.Advanced.Bytes.chompUntilEndOr


# -- LOOP ---------

Step state a : Parser.Advanced.Bytes.Step state a

loop : state, (state -> Parser (Step state a)) -> Parser a
loop = 
    Parser.Advanced.Bytes.loop


# -- BACKTRACKABLE ---------

backtrackable : Parser a -> Parser a
backtrackable = 
    Parser.Advanced.Bytes.backtrackable

commit : a -> Parser a
commit = 
    Parser.Advanced.Bytes.commit

# -- POSITION

getOffset: Parser Nat
getOffset =
    Parser.Advanced.Bytes.getOffset

getSource: Parser ByteList
getSource =
    Parser.Advanced.Bytes.getSource

# -- TOKEN

token : ByteList -> Parser {}
token = \tok ->
    Parser.Advanced.Bytes.token (tok |> toToken)

toToken: ByteList -> Parser.Advanced.Bytes.Token Problem
toToken = \tok ->
    {tok, expecting: Expecting tok}


# -- Byte specific

chompBytes :  ByteList -> Parser {}
chompBytes =
    token

chompByte : Byte -> Parser {}
chompByte = \b ->
    chompIf (\x -> x == b)
