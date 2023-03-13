interface Parser.Utf8
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

RawChar: U8
RawStr: List U8

Parser value : Parser.Advanced.Parser {} RawChar Problem value

DeadEnd: Parser.Advanced.DeadEnd {} Problem

# -- PROBLEMS ------------------

Problem : [
    UnexpectedChar, 
    Expecting RawStr,
    ExpectingKeyWord RawStr,
    ParsingFailure Str,
]

# -- RUN ------------------

buildPrimitiveParser = Parser.Advanced.buildPrimitiveParser

run : Parser a, RawStr -> Result a (List DeadEnd)
run = \parser, input ->
    when Parser.Advanced.run parser input is
        Ok value ->
            Ok value

        Err problems ->
            Err (problems |> List.map problemToDeadEnd)

problemToDeadEnd : Parser.Advanced.DeadEnd {} _ -> DeadEnd
problemToDeadEnd = \d ->
    { offset: d.offset, problem: d.problem, contextStack: [] }


Position: {row: Nat, col: Nat}

newLine: RawChar
newLine = '\n'

isNewLine: RawChar -> Bool
isNewLine = \c ->
    c == newLine

errorPosition: Nat, RawStr -> Position
errorPosition = \offset, src ->
    chomped = src |> List.takeFirst (offset + 1)
    when chomped |> List.splitLast newLine is
        Err _ ->
            {row: 1, col: 1 + offset }
        Ok {before, after} ->
            {row: 2 + (before |> List.countIf isNewLine), 
            col: 1 + (after |> List.len)}

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


## A parser which runs the element parser *zero* or more times on the input,
## returning a list containing all the parsed elements.
##
## Also see `oneOrMore`.
many : Parser v -> Parser (List v)
many = 
    Parser.Advanced.many

## A parser which runs the element parser *one* or more times on the input,
## returning a list containing all the parsed elements.
##
## Also see `many`.
oneOrMore : Parser v -> Parser (List v)
oneOrMore = 
    Parser.Advanced.oneOrMore    

## Runs a parser for an 'opening' delimiter, then your main parser, then the 'closing' delimiter,
## and only returns the result of your main parser.
##
## Useful to recognize structures surrounded by delimiters (like braces, parentheses, quotes, etc.)
##
## >>> betweenBraces  = \parser -> parser |> between (scalar '[') (scalar ']')
between : Parser v, Parser *, Parser * -> Parser v
between = 
    Parser.Advanced.between           

sepBy : Parser v, Parser * -> Parser (List v)
sepBy = 
    Parser.Advanced.sepBy


## Creates a new parser that ignores the result of the input parser, but propagates the state.
ignore : Parser v -> Parser {}
ignore = 
    Parser.Advanced.ignore     

# ---- CHOMPERS -------

chompIf: (RawChar -> Bool) -> Parser {}
chompIf = \isGood ->
    Parser.Advanced.chompIf isGood UnexpectedChar     


# Might be able to write a more efficient version than this?
# Bad name?
getChompedSource : Parser * -> Parser RawStr
getChompedSource = 
    Parser.Advanced.getChompedSource

mapChompedSource : Parser a, (RawStr, a -> b) -> Parser b
mapChompedSource = 
    Parser.Advanced.mapChompedSource
       

chompWhile: (RawChar -> Bool) -> Parser {}
chompWhile = 
    Parser.Advanced.chompWhile


chompUntil : RawStr -> Parser {}
chompUntil = \tok ->  
    Parser.Advanced.chompUntil (toToken tok)


chompUntilEndOr : RawStr -> Parser {}
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

getSource: Parser RawStr
getSource =
    Parser.Advanced.getSource

# -- TOKEN & SYMBOL

token : RawStr -> Parser {}
token = \tok ->
    Parser.Advanced.token (tok |> toToken)

toToken: RawStr -> Parser.Advanced.Token RawChar Problem
toToken = \tok ->
    {tok, expecting: Expecting tok}

symbol :  RawStr -> Parser {}
symbol =
  token


# -- UTF8 specific

separators: List RawChar
separators = [' ', '\n']

spaces: Parser {}
spaces =
    chompWhile (\c -> c == ' ' || c == '\n' || c == '\r')

keyword: RawStr -> Parser {}
keyword = \kwd ->
    Parser.Advanced.key separators {tok: kwd, expecting: ExpectingKeyWord kwd}