interface Parser.Bytes
    exposes [Parser, DeadEnd, #Types
             buildPrimitiveParser,
             run, #Operating
             const, fail, problem, end, symbol, token, #Primitives
             map, map2, keep, skip, andThen, #Combinators
             lazy, many, oneOrMore, alt, oneOf, between, sepBy, ignore, #Combinators
             chompIf, chompWhile, chompUntil, chompUntilEndOr, getChompedSource, mapChompedSource, #Chompers
             getOffset, getSource, # Info
             backtrackable, commit, # Backtracking
             loop, # Looping
             ]
    imports [Parser.Advanced.{}]


# -- PARSERS ------------------

Byte: U8
ByteList: List U8

Parser value : Parser.Advanced.Parser Context Byte Problem value

DeadEnd: Parser.Advanced.DeadEnd Context Problem

Context : {}

# -- PROBLEMS ------------------

Problem : [
    UnexpectedByte, 
    Expecting ByteList,
    ExpectingKeyWord ByteList,
    ParsingFailure Str,
]

# -- RUN ------------------

buildPrimitiveParser = Parser.Advanced.buildPrimitiveParser

run : Parser a, ByteList -> Result a (List DeadEnd)
run = \parser, input ->
    when Parser.Advanced.run parser input is
        Ok value ->
            Ok value

        Err problems ->
            Err (problems |> List.map problemToDeadEnd)

problemToDeadEnd : Parser.Advanced.DeadEnd Context _ -> DeadEnd
problemToDeadEnd = \d ->
    { offset: d.offset, problem: d.problem, contextStack: [] }

# -- PRIMITIVES -----------

#const : v -> Parser * v
const = Parser.Advanced.const

problem : Str -> Parser *
problem = \msg -> 
     Parser.Advanced.problem (ParsingFailure msg)

fail : Parser *
fail = 
    Parser.Advanced.fail
    
end: Parser {}
end = 
    Parser.Advanced.end    


# -- COMBINATORS ----------

map: Parser a, (a -> b) -> Parser b
map = 
    Parser.Advanced.map    

map2: Parser a, Parser b, (a, b -> d) -> Parser d
map2 = 
    Parser.Advanced.map2

keep: Parser (a -> b), Parser a -> Parser b        
keep = 
    Parser.Advanced.keep

skip: Parser keep, Parser ignore -> Parser keep
skip = 
    Parser.Advanced.skip

andThen: Parser a, (a -> Parser b) -> Parser b
andThen = 
    Parser.Advanced.andThen

alt: Parser v, Parser v -> Parser v
alt = 
    Parser.Advanced.alt          

oneOf : List (Parser v) -> Parser v
oneOf = 
    Parser.Advanced.oneOf    

lazy : ({} -> Parser v) -> Parser v
lazy = 
    Parser.Advanced.lazy     


many : Parser v -> Parser (List v)
many = 
    Parser.Advanced.many

oneOrMore : Parser v -> Parser (List v)
oneOrMore = 
    Parser.Advanced.oneOrMore    

between : Parser v, Parser *, Parser * -> Parser v
between = 
    Parser.Advanced.between           

sepBy : Parser v, Parser * -> Parser (List v)
sepBy = 
    Parser.Advanced.sepBy


ignore : Parser v -> Parser {}
ignore = 
    Parser.Advanced.ignore     


# flatten : Parser (Result v _) -> Parser v
# flatten = 
#     Parser.Advanced.flatten

# ---- CHOMPERS -------

chompIf: (Byte -> Bool) -> Parser {}
chompIf = \isGood ->
    Parser.Advanced.chompIf isGood UnexpectedByte     


getChompedSource : Parser * -> Parser ByteList
getChompedSource = 
    Parser.Advanced.getChompedSource

mapChompedSource : Parser a, (ByteList, a -> b) -> Parser b
mapChompedSource = 
    Parser.Advanced.mapChompedSource
       

chompWhile: (Byte -> Bool) -> Parser {}
chompWhile = 
    Parser.Advanced.chompWhile


chompUntil : ByteList -> Parser {}
chompUntil = \tok ->  
    Parser.Advanced.chompUntil (toToken tok)


chompUntilEndOr : ByteList -> Parser {}
chompUntilEndOr = 
    Parser.Advanced.chompUntilEndOr


# -- LOOP ---------

Step state a : Parser.Advanced.Step state a

loop : state, (state -> Parser (Step state a)) -> Parser a
loop = 
    Parser.Advanced.loop


# -- BACKTRACKABLE ---------

backtrackable : Parser a -> Parser a
backtrackable = 
    Parser.Advanced.backtrackable

commit : a -> Parser a
commit = 
    Parser.Advanced.commit

# -- POSITION

getOffset: Parser Nat
getOffset =
    Parser.Advanced.getOffset

getSource: Parser ByteList
getSource =
    Parser.Advanced.getSource

# -- TOKEN & SYMBOL

token : ByteList -> Parser {}
token = \tok ->
    Parser.Advanced.token (tok |> toToken)

toToken: ByteList -> Parser.Advanced.Token Byte Problem
toToken = \tok ->
    {tok, expecting: Expecting tok}

symbol :  ByteList -> Parser {}
symbol =
  token
