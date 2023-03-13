interface Parser 
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

Parser input value : Parser.Advanced.Parser {} input (Problem input) value

DeadEnd i: Parser.Advanced.DeadEnd {} (Problem i)

# -- PROBLEMS ------------------

Problem i : [
    UnexpectedChar, 
    Expecting (List i),
    ParsingFailure Str,
]

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
    { offset: d.offset, problem: d.problem, contextStack: [] }




# -- PRIMITIVES -----------

#const : v -> Parser * v
const = Parser.Advanced.const

problem : Str -> Parser * *
problem = \msg -> 
     Parser.Advanced.problem (ParsingFailure msg)

fail : Parser * *
fail = 
    Parser.Advanced.fail
    
end: Parser * {}
end = 
    Parser.Advanced.end    


# -- COMBINATORS ----------

map: Parser i a, (a -> b) -> Parser i b
map = 
    Parser.Advanced.map    

map2: Parser i a, Parser i b, (a, b -> d) -> Parser i d
map2 = 
    Parser.Advanced.map2

keep: Parser i (a -> b), Parser i a -> Parser i b        
keep = 
    Parser.Advanced.keep

skip: Parser i keep, Parser i ignore -> Parser i keep
skip = 
    Parser.Advanced.skip
andThen: Parser i a, (a -> Parser i b) -> Parser i b
andThen = 
    Parser.Advanced.andThen

alt: Parser i v, Parser i v -> Parser i v
alt = 
    Parser.Advanced.alt          

oneOf : List (Parser i v) -> Parser i v
oneOf = 
    Parser.Advanced.oneOf    

lazy : ({} -> Parser i v) -> Parser i v
lazy = 
    Parser.Advanced.lazy     


## A parser which runs the element parser *zero* or more times on the input,
## returning a list containing all the parsed elements.
##
## Also see `oneOrMore`.
many : Parser i v -> Parser i (List v)
many = 
    Parser.Advanced.many

## A parser which runs the element parser *one* or more times on the input,
## returning a list containing all the parsed elements.
##
## Also see `many`.
oneOrMore : Parser i v -> Parser i (List v)
oneOrMore = 
    Parser.Advanced.oneOrMore    

## Runs a parser for an 'opening' delimiter, then your main parser, then the 'closing' delimiter,
## and only returns the result of your main parser.
##
## Useful to recognize structures surrounded by delimiters (like braces, parentheses, quotes, etc.)
##
## >>> betweenBraces  = \parser -> parser |> between (scalar '[') (scalar ']')
between : Parser i v, Parser i *, Parser i * -> Parser i v
between = 
    Parser.Advanced.between           

sepBy : Parser i v, Parser i * -> Parser i (List v)
sepBy = 
    Parser.Advanced.sepBy


## Creates a new parser that ignores the result of the input parser, but propagates the state.
ignore : Parser i v -> Parser i {}
ignore = 
    Parser.Advanced.ignore     

# ---- CHOMPERS -------

chompIf: (i -> Bool) -> Parser i {}
chompIf = \isGood ->
    Parser.Advanced.chompIf isGood UnexpectedChar     


# Might be able to write a more efficient version than this?
# Bad name?
getChompedSource : Parser i * -> Parser i (List i)
getChompedSource = 
    Parser.Advanced.getChompedSource

mapChompedSource : Parser i a, (List i, a -> b) -> Parser i b
mapChompedSource = 
    Parser.Advanced.mapChompedSource
       

chompWhile: (i -> Bool) -> Parser i {}
chompWhile = 
    Parser.Advanced.chompWhile


chompUntil : List i -> Parser i {}
            | i has Eq
chompUntil = \tok ->  
    Parser.Advanced.chompUntil (toToken tok)


chompUntilEndOr : List i -> Parser i {}
                    | i has Eq 
chompUntilEndOr = 
    Parser.Advanced.chompUntilEndOr


# -- LOOP ---------

Step state a : Parser.Advanced.Step state a

loop : state, (state -> Parser i (Step state a)) -> Parser i a
loop = 
    Parser.Advanced.loop


# -- BACKTRACKABLE ---------

backtrackable : Parser i a -> Parser i a
backtrackable = 
    Parser.Advanced.backtrackable

commit : a -> Parser * a
commit = 
    Parser.Advanced.commit

# -- POSITION

getOffset: Parser * Nat
getOffset =
    Parser.Advanced.getOffset

getSource: Parser i (List i)
getSource =
    Parser.Advanced.getSource

# -- TOKEN & SYMBOL

token : List i -> Parser i {} | i has Eq
token = \tok ->
    Parser.Advanced.token (tok |> toToken)

toToken: List i -> Parser.Advanced.Token i (Problem i)
toToken = \tok ->
    {tok, expecting: Expecting tok}

symbol :  List i -> Parser i {} | i has Eq
symbol =
  token
