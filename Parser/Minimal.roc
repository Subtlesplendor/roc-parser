interface Parser.Minimal 
    exposes [Parser, #Types
             buildPrimitiveParser, run, #Operating
             succeed, problem, #Primitives
             map, map2, map3, keep, skip, andThen, #Combinators
             lazy, many, oneOrMore, alt, oneOf, between, sepBy, ignore #Combinators
             ]
    imports []


# -- PARSERS ------------------ 

State input : { src: List input, offset: Nat }

PResult input value : Result {val: value, state: State input} Str

Parser input value := 
    State input -> PResult input value

buildPrimitiveParser: (State i -> PResult i v) -> Parser i v
buildPrimitiveParser = \f ->
    @Parser f



# -- OPERATING ------------

run: Parser i v, List i -> Result v Str
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
     @Parser \_ -> Err "Problem"

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
        firstErr <- Result.onErr (first state)
        secondErr <- Result.onErr (second state)
        Err "\(firstErr) or \(secondErr)"

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





# ---- INTERNAL HELPER FUNCTIONS -------

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
