interface Parser.Full 
    exposes [Parser, #Types
             buildPrimitiveParser,
             #run, #Operating
             #const, fail, problem, end, #Primitives
             #map, map2, map3, keep, skip, andThen, #Combinators
             #lazy, many, oneOrMore, alt, oneOf, between, sepBy, ignore, #Combinators
             #one, oneIf, # Parsers
             #next, nextIf, nextWhile, getChompedSource #Low level
             ]
    imports []

## A medium parser library. 

## The parser parses lists of `input` and returns a `value`.

## The parser operates by moving a cursor along the list. The input is never consumed.


# -- TYPES ------------------ 

Parser context input problem value := 
    State context input -> PResult context input problem value

State context input : { src: List input, offset: Nat, context: context }

Success context input value : 
    {val: value, backtrackable: Backtrackable, state: State context input}
Failure context problem : { backtrackable: Backtrackable,
                            parsingError: 
                                [OutOfBounds,
                                ExpectingEnd,
                                Neither,
                                #Tree (Failure context problem),
                                FailAt context, 
                                Problem problem] }



Backtrackable : [Yes, No]

and = \b1, b2 -> 
    when (b1, b2) is
        (No, _) -> No
        (_, No) -> No
        _ -> Yes

or = \b1, b2 -> 
    when (b1, b2) is
        (Yes, _) -> Yes
        (_, Yes) -> Yes
        _ -> No        
            


PResult context input problem value : 
    Result (Success context input value) (Failure context problem)


# -- OPERATING ------------

# Construct a parser from a parser-function.
buildPrimitiveParser: (State c i -> PResult c i p v) -> Parser c i p v
buildPrimitiveParser = \f ->
        @Parser f

# Run a parser and get a Result.
run: Parser c i p v, c, List i-> Result v (Failure c p)
run = \@Parser parse, con, src ->
    parse {src, offset: 0, context: con} 
    |> Result.map .val


# -- PRIMITIVES -----------

const : v -> Parser * * * v
const = \val ->
    @Parser \state -> Ok {val, state, backtrackable: Yes}

problem : p -> Parser * * p *
problem = \p -> 
     @Parser \_ -> Err {parsingError: Problem p,
                        backtrackable: Yes}

fail : Parser * * p *
fail = 
    @Parser \s -> Err { parsingError: FailAt s.context,
                        backtrackable: Yes}

end: Parser * * * {}
end = 
    @Parser \state ->
        if state.offset == List.len state.src then
            Ok {val: {}, state, backtrackable: Yes}
        else
            Err {parsingError: ExpectingEnd, backtrackable: Yes}     

# # -- COMBINATORS ----------

# map: Parser c i p a, (a -> b) -> Parser c i p b
# map = \@Parser parse, f ->
#     @Parser \s ->
#         {val, state, backtrackable} <- Result.try (parse s)
#         Ok {val: f val, state, backtrackable}        

# # map2: Parser c i p a, Parser c i p b, (a, b -> d) -> Parser c i p d
# map2 = \@Parser first, @Parser second, f ->
#     @Parser \s0 ->
#         {val: val1, state: s1} <- Result.try (first s0)
#         {val: val2, state: s2} <- Result.try (second s1)
#         Ok {val: f val1 val2, state: s2}

# map3: Parser c i p a, Parser c i p b, Parser c i p d, (a, b, d -> e) -> Parser c i p e
# map3 = \@Parser first, @Parser second, @Parser third, f ->
#     @Parser \s0 ->
#         {val: val1, state: s1} <- Result.try (first s0)
#         {val: val2, state: s2} <- Result.try (second s1)
#         {val: val3, state: s3} <- Result.try (third s2)
#         Ok {val: f val1 val2 val3, state: s3}


# keep: Parser c i p (a -> b), Parser c i p a -> Parser c i p b        
# keep = \parserFunc, parserArg ->
#      map2 parserFunc parserArg (\f, x -> f x)

# skip: Parser c i p keep, Parser c i p ignore -> Parser c i p keep
# skip = \parserKeep, parserSkip ->
#     map2 parserKeep parserSkip (\k, _ -> k)

# andThen: Parser c i p a, (a -> Parser c i p b) -> Parser c i p b
# andThen = \@Parser firstParser, parserBuilder ->
#     @Parser \s0 ->
#         {val: a, state: s1} <- Result.try (firstParser s0)
#         @Parser nextParser = parserBuilder a
#         nextParser s1



# alt : Parser c i p v, Parser c i p v -> Parser c i p v
# alt = \@Parser first, @Parser second ->
#     @Parser \state ->
#         _ <- Result.onErr (first state)
#         _ <- Result.onErr (second state)
#         Err (FailAt state.context)

# oneOf : List (Parser c i p v) -> Parser c i p v
# oneOf = \parsers ->
#     List.walkBackwards parsers fail (\laterParser, earlierParser -> alt earlierParser laterParser)
      

# lazy : ({} -> Parser c i p v) -> Parser c i p v
# lazy = \thunk ->
#     const {}
#     |> andThen thunk        


# ## A parser which runs the element parser *zero* or more times on the input,
# ## returning a list containing all the parsed elements.
# ##
# ## Also see `oneOrMore`.
# many : Parser c i p v -> Parser c i p (List v)
# many = \parser ->
#     @Parser \state ->
#         manyImpl parser [] state

# ## A parser which runs the element parser *one* or more times on the input,
# ## returning a list containing all the parsed elements.
# ##
# ## Also see `many`.
# oneOrMore : Parser c i p v -> Parser c i p (List v)
# oneOrMore = \@Parser parser ->
#     @Parser \s ->
#         {val, state} <- Result.try (parser s) 
#         manyImpl (@Parser parser) [val] state       

# ## Runs a parser for an 'opening' delimiter, then your main parser, then the 'closing' delimiter,
# ## and only returns the result of your main parser.
# ##
# ## Useful to recognize structures surrounded by delimiters (like braces, parentheses, quotes, etc.)
# ##
# ## >>> betweenBraces  = \parser -> parser |> between (scalar '[') (scalar ']')
# between : Parser c i p v, Parser c i p *, Parser c i p * -> Parser c i p v
# between = \parser, open, close ->
#     map3 open parser close (\_, val, _ -> val)            

# sepBy : Parser c i p v, Parser c i p * -> Parser c i p (List v)
# sepBy = \parser, separator ->
#     alt (sepBy1 parser separator) (const [])


# # ## Creates a new parser that ignores the result of the input parser, but propagates the state.
# ignore : Parser c i p v -> Parser c i p {}
# ignore = \parser ->
#     map parser (\_ -> {})        

# # ---- LOW LEVEL FUNCTIONS -------

# next: (c, i -> c) -> Parser c i * {}
# next = \updateWith ->
#     @Parser \s ->
#         char <- Result.try (s.src |> List.get s.offset)
#         Ok {val: {}, state: {s & offset: s.offset + 1, 
#                                  context: s.context |> updateWith char } }

# nextIf: (c, i -> c), (i -> Bool) -> Parser c i * {}
# nextIf = \updateWith, isGood ->
#     @Parser \s ->
#         char <- Result.try (s.src |> List.get s.offset)
#         if isGood char then
#             Ok {val: {}, state: {s & offset: s.offset + 1,
#                                      context: s.context |> updateWith char}}
#         else 
#             Err (FailAt s.context)

# one: (c, i -> c) -> Parser c i * i 
# one = \updateWith ->
#     @Parser \s ->
#         char <- Result.try (s.src |> List.get s.offset)
#         Ok {val: char, state: {s & offset: s.offset + 1,
#                                    context: s.context |> updateWith char} }

# oneIf: (c, i -> c), (i -> Bool) -> Parser c i * i 
# oneIf = \updateWith, isGood ->
#     @Parser \s ->
#         char <- Result.try (s.src |> List.get s.offset)
#         if isGood char then
#             Ok {val: char, state: {s & offset: s.offset + 1,
#                                        context: s.context |> updateWith char} }
#         else 
#             Err (FailAt s.context)  

# # Might be able to write a more efficient version than this?
# # Bad name?
# getChompedSource : Parser c i p * -> Parser c i p (List i)
# getChompedSource = \@Parser parse ->
#     @Parser \s0 ->
#         {val: _, state: s1} <- Result.try (parse s0) 
#         when s1.offset - s0.offset is
#             x if x >= 0 ->
#                 length = x |> Num.toNat
#                 Ok {val: s0.src |> List.sublist {start: s0.offset, len: length}, 
#                     state: s1}
#             _ ->
#                 Err OutOfBounds # Only happens if the parser moves backwards

# #future ref: 
# #nextWhile: (State i, i -> State i), (i -> Bool) -> Parser i {}
# nextWhile: (c, i -> c), (i -> Bool) -> Parser c i * {}
# nextWhile = \updateWith, isGood ->
#     @Parser \s0 ->
#         finalState = s0.src |> List.walkFromUntil s0.offset s0 \s, c ->
#             if isGood c then
#                 Continue {s & offset: s.offset + 1,
#                               context: s.context |> updateWith c}
#             else
#                 Break s
#         Ok {val: {}, state: finalState}


# # ---- INTERNAL HELPER FUNCTIONS -------

# #Helper function for many and oneOrMore
# manyImpl : Parser c i p a, List a, State c i -> PResult c i p (List a)
# manyImpl = \@Parser parser, vals, s ->
#     when parser s is
#         Err _ ->
#             Ok { val: vals, state: s }

#         Ok { val: val, state: newState } ->
#             manyImpl (@Parser parser) (List.append vals val) newState  

# # Helper function for sepBy
# sepBy1 : Parser c i p v, Parser c i p * -> Parser c i p (List v)
# sepBy1 = \parser, separator ->
#     parserFollowedBySep =
#         const (\_ -> \val -> val)
#         |> keep separator
#         |> keep parser

#     const (\val -> \vals -> List.prepend vals val)
#     |> keep parser
#     |> keep (many parserFollowedBySep)