interface Parser.Minimal 
    exposes [Parser, #Types
             #buildPrimitiveParser,
             run, #Operating
             succeed, problem, end, #Primitives
             map, map2, map3, keep, skip, andThen, #Combinators
             lazy, many, oneOrMore, alt, oneOf, between, sepBy, ignore, #Combinators
             one, oneIf, # Parsers
             next, nextIf, nextWhile, getChompedSource #Low level
             ]
    imports []

## A minimal parser library. 

## The parser parses lists of `input` and returns a `value`.

## The parser operates by moving a cursor along the list. The input is never consumed.


# -- TYPES ------------------ 

Parser input value := 
    State input -> PResult input value

State input : { src: List input, offset: Nat }

PResult input value : Result {val: value, state: State input} [OutOfBounds, Fail, Str]


# -- OPERATING ------------

## Construct a parser from a parser-function.
# buildPrimitiveParser: (State i -> PResult i v) -> Parser i v
# buildPrimitiveParser = \f ->
#     @Parser f


## Run a parser and get a Result.
run: Parser i v, List i -> Result v [OutOfBounds, Fail, Str]
run = \@Parser parse, src ->
    parse {src, offset: 0} 
    |> Result.map .val


# -- PRIMITIVES -----------

#const
succeed : v -> Parser * v
succeed = \val ->
    @Parser \state -> Ok {val, state}

#fail
problem : Parser * *
problem = 
     @Parser \_ -> Err Fail

end: Parser i {}
end = 
    @Parser \state ->
        if state.offset == List.len state.src then
            Ok {val: {}, state}
        else
            Err Fail     

# -- COMBINATORS ----------

map: Parser i a, (a -> b) -> Parser i b
map = \@Parser parse, f ->
    @Parser \s ->
        {val, state} <- Result.try (parse s)
        Ok {val: f val, state}

map2: Parser i a, Parser i b, (a, b -> c) -> Parser i c
map2 = \@Parser first, @Parser second, f ->
    @Parser \s0 ->
        {val: val1, state: s1} <- Result.try (first s0)
        {val: val2, state: s2} <- Result.try (second s1)
        Ok {val: f val1 val2, state: s2}

map3: Parser i a, Parser i b, Parser i c, (a, b, c -> d) -> Parser i d
map3 = \@Parser first, @Parser second, @Parser third, f ->
    @Parser \s0 ->
        {val: val1, state: s1} <- Result.try (first s0)
        {val: val2, state: s2} <- Result.try (second s1)
        {val: val3, state: s3} <- Result.try (third s2)
        Ok {val: f val1 val2 val3, state: s3}


keep: Parser i (a -> b), Parser i a -> Parser i b        
keep = \parserFunc, parserArg ->
     map2 parserFunc parserArg (\f, x -> f x)

skip: Parser i keep, Parser i ignore -> Parser i keep
skip = \parserKeep, parserSkip ->
    map2 parserKeep parserSkip (\k, _ -> k)

andThen: Parser i a, (a -> Parser i b) -> Parser i b
andThen = \@Parser firstParser, parserBuilder ->
    @Parser \s0 ->
        {val: a, state: s1} <- Result.try (firstParser s0)
        @Parser nextParser = parserBuilder a
        nextParser s1

alt : Parser i v, Parser i v -> Parser i v
alt = \@Parser first, @Parser second ->
    @Parser \state ->
        _ <- Result.onErr (first state)
        secondErr <- Result.onErr (second state)
        Err secondErr

oneOf : List (Parser i v) -> Parser i v
oneOf = \parsers ->
    List.walkBackwards parsers problem (\laterParser, earlierParser -> alt earlierParser laterParser)
      

lazy : ({} -> Parser i v) -> Parser i v
lazy = \thunk ->
    succeed {}
    |> andThen thunk        


## A parser which runs the element parser *zero* or more times on the input,
## returning a list containing all the parsed elements.
##
## Also see `oneOrMore`.
many : Parser i v -> Parser i (List v)
many = \parser ->
    @Parser \state ->
        manyImpl parser [] state

## A parser which runs the element parser *one* or more times on the input,
## returning a list containing all the parsed elements.
##
## Also see `many`.
oneOrMore : Parser i v -> Parser i (List v)
oneOrMore = \@Parser parser ->
    @Parser \s ->
        {val, state} <- Result.try (parser s) 
        manyImpl (@Parser parser) [val] state       

## Runs a parser for an 'opening' delimiter, then your main parser, then the 'closing' delimiter,
## and only returns the result of your main parser.
##
## Useful to recognize structures surrounded by delimiters (like braces, parentheses, quotes, etc.)
##
## >>> betweenBraces  = \parser -> parser |> between (scalar '[') (scalar ']')
between : Parser i v, Parser i open, Parser i close -> Parser i v
between = \parser, open, close ->
    map3 open parser close (\_, val, _ -> val)            

sepBy : Parser i v, Parser i sep -> Parser i (List v)
sepBy = \parser, separator ->
    alt (sepBy1 parser separator) (succeed [])


## Creates a new parser that ignores the result of the input parser, but propagates the state.
ignore : Parser i v -> Parser i {}
ignore = \parser ->
    map parser (\_ -> {})        

# ---- LOW LEVEL FUNCTIONS -------

next: Parser i {}
next =
    @Parser \s ->
        _ <- Result.try (s.src |> List.get s.offset)
        Ok {val: {}, state: {s & offset: s.offset + 1}}

nextIf: (i -> Bool) -> Parser i {}
nextIf = \isGood ->
    @Parser \s ->
        c <- Result.try (s.src |> List.get s.offset)
        if isGood c then
            Ok {val: {}, state: {s & offset: s.offset + 1}}
        else 
            Err Fail

one: Parser i i 
one =
    @Parser \s ->
        c <- Result.try (s.src |> List.get s.offset)
        Ok {val: c, state: {s & offset: s.offset + 1}}

oneIf: (i -> Bool) -> Parser i i 
oneIf = \isGood ->
    @Parser \s ->
        c <- Result.try (s.src |> List.get s.offset)
        if isGood c then
            Ok {val: c, state: {s & offset: s.offset + 1}}
        else 
            Err Fail      

# Might be able to write a more efficient version than this?
# Bad name?
getChompedSource : Parser i * -> Parser i (List i)
getChompedSource = \@Parser parse ->
    @Parser \s0 ->
        {val: _, state: s1} <- Result.try (parse s0) 
        when s1.offset - s0.offset is
            x if x>= 0 ->
                length = x |> Num.toNat
                Ok {val: s0.src |> List.sublist {start: s0.offset, len: length}, 
                    state: s1}
            _ ->
                Err Fail

#future ref: 
#nextWhile: (State i, i -> State i), (i -> Bool) -> Parser i {}
nextWhile: (i -> Bool) -> Parser i {}
nextWhile = \isGood ->
    @Parser \s ->
        pos = s.offset
        finalPos = s.src |> List.walkFromUntil s.offset pos \p, c ->
            if isGood c then
                Continue (p + 1)
            else
                Break p
        Ok {val: {}, state: {s & offset: finalPos}}


# ---- INTERNAL HELPER FUNCTIONS -------

#Helper function for many and oneOrMore
manyImpl : Parser i a, List a, State i -> PResult i (List a)
manyImpl = \@Parser parser, vals, s ->
    when parser s is
        Err _ ->
            Ok { val: vals, state: s }

        Ok { val: val, state: newState } ->
            manyImpl (@Parser parser) (List.append vals val) newState  

# Helper function for sepBy
sepBy1 : Parser i v, Parser i sep -> Parser i (List v)
sepBy1 = \parser, separator ->
    parserFollowedBySep =
        succeed (\_ -> \val -> val)
        |> keep separator
        |> keep parser

    succeed (\val -> \vals -> List.prepend vals val)
    |> keep parser
    |> keep (many parserFollowedBySep)
