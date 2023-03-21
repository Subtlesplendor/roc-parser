interface Parser.Advanced.Utf8
    exposes [Parser, DeadEnd, Step, Token, RawChar, RawStr, #Types
             buildPrimitiveParser,
             run, #Operating
             const, fail, problem, end, token, #Primitives
             map, map2, keep, skip, andThen, flatten, #Combinators
             lazy, many, oneOrMore, alt, oneOf, between, sepBy, ignore, #Combinators
             chompIf, chompWhile, chompUntil, chompUntilEndOr, getChompedRawStr, mapChompedRawStr, #Chompers
             getOffset, getSource, # Info
             backtrackable, commit, # Backtracking
             loop, # Looping
             keyword
             ]
    imports [Parser.Advanced.Generic.{}]


# -- PARSERS ------------------

RawChar: U8
RawStr: List U8

Parser context problem value : Parser.Advanced.Generic.Parser context RawChar problem value

DeadEnd context problem : Parser.Advanced.Generic.DeadEnd context problem


Token p : Parser.Advanced.Generic.Token RawChar p

# -- RUN ------------------

buildPrimitiveParser = Parser.Advanced.Generic.buildPrimitiveParser

run : Parser c p a, RawStr -> Result a (List (DeadEnd c p))
run = 
    Parser.Advanced.Generic.run
        
# -- PRIMITIVES -----------

const : v -> Parser * * v
const = Parser.Advanced.Generic.const

problem : p -> Parser * p *
problem = 
     Parser.Advanced.Generic.problem

fail : Parser * * *
fail = 
    Parser.Advanced.Generic.fail
    
end: p -> Parser * p {}
end = 
    Parser.Advanced.Generic.end    


# # -- COMBINATORS ----------

map: Parser c p a, (a -> b) -> Parser c p b
map = 
    Parser.Advanced.Generic.map    

map2: Parser c p a, Parser c p b, (a, b -> d) -> Parser c p d
map2 = 
    Parser.Advanced.Generic.map2

keep: Parser c p (a -> b), Parser c p a -> Parser c p b        
keep = 
    Parser.Advanced.Generic.keep

skip: Parser c p keep, Parser c p ignore -> Parser c p keep
skip = 
    Parser.Advanced.Generic.skip

andThen: Parser c p a, (a -> Parser c p b) -> Parser c p b
andThen = 
    Parser.Advanced.Generic.andThen

alt: Parser c p v, Parser c p v -> Parser c p v
alt = 
    Parser.Advanced.Generic.alt          

oneOf : List (Parser c p v) -> Parser c p v
oneOf = 
    Parser.Advanced.Generic.oneOf    

lazy : ({} -> Parser c p v) -> Parser c p v
lazy = 
    Parser.Advanced.Generic.lazy     


many : Parser c p v -> Parser c p (List v)
many = 
    Parser.Advanced.Generic.many

oneOrMore : Parser c p v -> Parser c p (List v)
oneOrMore = 
    Parser.Advanced.Generic.oneOrMore    

between : Parser c p v, Parser c p *, Parser c p * -> Parser c p v
between = 
    Parser.Advanced.Generic.between           

sepBy : Parser c p v, Parser c p * -> Parser c p (List v)
sepBy = 
    Parser.Advanced.Generic.sepBy


ignore : Parser c p v -> Parser c p {}
ignore = 
    Parser.Advanced.Generic.ignore     


flatten : Parser c p (Result v p) -> Parser c p v
flatten = 
    Parser.Advanced.Generic.flatten

# # ---- CHOMPERS -------

chompIf: (RawChar -> Bool), p -> Parser * p {}
chompIf =
    Parser.Advanced.Generic.chompIf     


getChompedRawStr : Parser c p * -> Parser c p RawStr
getChompedRawStr = 
    Parser.Advanced.Generic.getChompedSource

mapChompedRawStr : Parser c p a, (RawStr, a -> b) -> Parser c p b
mapChompedRawStr = 
    Parser.Advanced.Generic.mapChompedSource
       

chompWhile: (RawChar -> Bool) -> Parser c p {}
chompWhile = 
    Parser.Advanced.Generic.chompWhile


chompUntil : Token p -> Parser * p {}
chompUntil = 
    Parser.Advanced.Generic.chompUntil 


chompUntilEndOr : RawStr -> Parser c p {}
chompUntilEndOr = 
    Parser.Advanced.Generic.chompUntilEndOr


# # -- LOOP ---------

Step state a : Parser.Advanced.Generic.Step state a

loop : state, (state -> Parser c p (Step state a)) -> Parser c p a
loop = 
    Parser.Advanced.Generic.loop


# # -- BACKTRACKABLE ---------

backtrackable : Parser c p a -> Parser c p a
backtrackable = 
    Parser.Advanced.Generic.backtrackable

commit : a -> Parser * * a
commit = 
    Parser.Advanced.Generic.commit

# # -- POSITION

getOffset: Parser * * Nat
getOffset =
    Parser.Advanced.Generic.getOffset

getSource: Parser * * RawStr
getSource =
    Parser.Advanced.Generic.getSource

# -- TOKEN

token : Token p -> Parser * p {}
token = 
    Parser.Advanced.Generic.token

keyword: List RawChar, Token p -> Parser * p {}
keyword = 
    Parser.Advanced.Generic.key
