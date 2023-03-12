interface Parser.FullBag 
    exposes [Parser, #Types
             #buildPrimitiveParser,
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
    State context input -> PStep context input problem value

State context input : { src: List input, offset: Nat, context: List (Located context) }

Located context: {offset: Nat, context: context}


Good context input value : {val: value, state: State context input, backtrackable: Backtrackable}
Bad context problem : {bag: Bag context problem, backtrackable: Backtrackable}



Bag c p : [Empty, 
           AddRight (Bag c p) (DeadEnd c p), 
           Append (Bag c p) (Bag c p) ]

Problem p : [OutOfBounds,
             ExpectingEnd,
             FailAt Nat]p
             

DeadEnd c p : {offset: Nat, problem: Problem p, contextStack: List (Located c)}

PStep context input problem value : 
    Result (Good context input value) (Bad context problem)


Backtrackable : [Yes, No]

and: Backtrackable, Backtrackable -> Backtrackable
and = \b1, b2 -> 
    when (b1, b2) is
        (No, _) -> No
        (_, No) -> No
        _ -> Yes

or: Backtrackable, Backtrackable -> Backtrackable
or = \b1, b2 -> 
    when (b1, b2) is
        (Yes, _) -> Yes
        (_, Yes) -> Yes
        _ -> No        
            


try: PStep c i p a, (Good c i a -> PStep c i p b) -> PStep c i p b
try = \res, callback ->
    step <- Result.try res
    when callback step is
        Ok {val: b, state: s2, backtrackable: b2} ->
            Ok {val: b, state: s2, backtrackable: step.backtrackable |> and b2}
        Err {bag: bag2, backtrackable: b2} ->
            Err {bag: bag2, backtrackable: step.backtrackable |> and b2}

onFail: PStep c i p a, (Bad c p -> PStep c i p a) -> PStep c i p a
onFail = \res, callback ->
    err <- Result.onErr res
    when callback err is
        Ok {val: b, state: s2, backtrackable: b2} ->
            Ok {val: b, state: s2, backtrackable: err.backtrackable |> and b2}
        Err {bag: bag2, backtrackable: b2} ->
            Err {bag: Append err.bag bag2, backtrackable: err.backtrackable |> and b2}

# -- OPERATING ------------

# Construct a parser from a parser-function.
buildPrimitiveParser: (State c i -> PStep c i p v) -> Parser c i p v
buildPrimitiveParser = \f ->
        @Parser f        

# Run a parser and get a Result.
run: Parser c i p v, c, List i-> Result v (List (DeadEnd c p))
run = \@Parser parse, con, src ->
    when parse {src, offset: 0, context: []} is
        Ok good ->
            Ok good.val
        Err bad ->
            Err (bad.bag |> bagToList [])

# -- PRIMITIVES -----------

const : v -> Parser * * * v
const = \val ->
    @Parser \state -> Ok {val, state, backtrackable: Yes}

problem : Problem p -> Parser c i p v
problem = \p -> 
     @Parser \s -> Err {bag:(fromState s p), backtrackable: Yes}

fail : Parser * * p *
fail = 
    @Parser \s -> Err {bag: Empty, backtrackable: Yes}

end: Parser * * * {}
end = 
    @Parser \state ->
        if state.offset == List.len state.src then
            Ok {val: {}, state, backtrackable: Yes}
        else
            Err {bag: fromState state ExpectingEnd, backtrackable: Yes}     


# -- COMBINATORS ----------

map: Parser c i p a, (a -> b) -> Parser c i p b
map = \@Parser parser, f ->
    @Parser \s0 ->
        {val: a, backtrackable: b1, state: s1} <- try (parser s0)
        Ok {val: f a, backtrackable: b1, state: s1}        

map2: Parser c i p a, Parser c i p b, (a, b -> d) -> Parser c i p d
map2 = \@Parser first, @Parser second, f ->
    @Parser \s0 ->
        {val: val1, state: s1, backtrackable: b1} <- try (first s0)
        {val: val2, state: s2, backtrackable: b2} <- try (second s1)
        Ok {val: f val1 val2, state: s2, backtrackable: b1 |> and b2}

map3: Parser c i p a, Parser c i p b, Parser c i p d, (a, b, d -> e) -> Parser c i p e
map3 = \@Parser first, @Parser second, @Parser third, f ->
    @Parser \s0 ->
        {val: val1, state: s1, backtrackable: b1} <- try (first s0)
        {val: val2, state: s2, backtrackable: b2} <- try (second s1)
        {val: val3, state: s3, backtrackable: b3} <- try (third s2)
        Ok {val: f val1 val2 val3, state: s3, backtrackable: b1 |> and b2 |> and b3}


keep: Parser c i p (a -> b), Parser c i p a -> Parser c i p b        
keep = \parserFunc, parserArg ->
     map2 parserFunc parserArg (\f, x -> f x)

skip: Parser c i p keep, Parser c i p ignore -> Parser c i p keep
skip = \parserKeep, parserSkip ->
    map2 parserKeep parserSkip (\k, _ -> k)

andThen: Parser c i p a, (a -> Parser c i p b) -> Parser c i p b
andThen = \@Parser firstParser, parserBuilder ->
    @Parser \s0 ->
        {val: a, state: s1, backtrackable: b1} <- try (firstParser s0)
        @Parser nextParser = parserBuilder a
        step2 <- try (nextParser s1)
        Ok {step2 & backtrackable: b1 |> and step2.backtrackable}



alt : Parser c i p v, Parser c i p v -> Parser c i p v
alt = \@Parser first, @Parser second ->
    @Parser \state ->
        {bag: bag1, backtrackable: b1} <- onFail (first state)
        if b1 == No then 
            Err {bag: bag1, backtrackable: b1}
        else
            {bag: bag2, backtrackable: b2} <- onFail (second state)
            Err {bag: Append bag1 bag2, backtrackable: b2}

oneOf : List (Parser c i p v) -> Parser c i p v
oneOf = \parsers ->
    List.walkBackwards parsers fail (\laterParser, earlierParser -> alt earlierParser laterParser)      

lazy : ({} -> Parser c i p v) -> Parser c i p v
lazy = \thunk ->
    const {}
    |> andThen thunk        


# How should this work with backtrackable?

## A parser which runs the element parser *zero* or more times on the input,
## returning a list containing all the parsed elements.
##
## Also see `oneOrMore`.
many : Parser c i p v -> Parser c i p (List v)
many = \parser ->
    @Parser \state ->
        manyImpl parser [] state

## A parser which runs the element parser *one* or more times on the input,
## returning a list containing all the parsed elements.
##
## Also see `many`.
oneOrMore : Parser c i p v -> Parser c i p (List v)
oneOrMore = \@Parser parser ->
    @Parser \s ->
        {val, state, backtrackable: _} <- try (parser s)
        manyImpl (@Parser parser) [val] state       

## Runs a parser for an 'opening' delimiter, then your main parser, then the 'closing' delimiter,
## and only returns the result of your main parser.
##
## Useful to recognize structures surrounded by delimiters (like braces, parentheses, quotes, etc.)
##
## >>> betweenBraces  = \parser -> parser |> between (scalar '[') (scalar ']')
between : Parser c i p v, Parser c i p *, Parser c i p * -> Parser c i p v
between = \parser, open, close ->
    map3 open parser close (\_, val, _ -> val)            

sepBy : Parser c i p v, Parser c i p * -> Parser c i p (List v)
sepBy = \parser, separator ->
    alt (sepBy1 parser separator) (const [])


# ## Creates a new parser that ignores the result of the input parser, but propagates the state.
ignore : Parser c i p v -> Parser c i p {}
ignore = \parser ->
    map parser (\_ -> {})        

# ---- LOW LEVEL FUNCTIONS -------

# next: Parser c i * {}
# next =
#     @Parser \s ->
#         char <- Result.try (s.src |> List.get s.offset)
#         Ok {val: {}, state: {s & offset: s.offset + 1} }




chompIf: (i -> Bool), Problem p -> Parser c i p {}
chompIf = \isGood, expecting ->
    @Parser \s ->
        when s.src |> List.get s.offset is
            Ok i ->
                if i |> isGood then
                    Ok {val: {}, state: {s & offset: s.offset + 1}, backtrackable: No}
                else
                    Err {bag: fromState s expecting, backtrackable:Yes}
            
            Err OutOfBounds ->
                Err {bag: fromState s OutOfBounds, backtrackable: Yes}        

# # one: (c, i -> c) -> Parser c i * i 
# # one = \updateWith ->
# #     @Parser \s ->
# #         char <- Result.try (s.src |> List.get s.offset)
# #         Ok {val: char, state: {s & offset: s.offset + 1,
# #                                    context: s.context |> updateWith char} }

# # oneIf: (c, i -> c), (i -> Bool) -> Parser c i * i 
# # oneIf = \updateWith, isGood ->
# #     @Parser \s ->
# #         char <- Result.try (s.src |> List.get s.offset)
# #         if isGood char then
# #             Ok {val: char, state: {s & offset: s.offset + 1,
# #                                        context: s.context |> updateWith char} }
# #         else 
# #             Err (FailAt s.context)  

# Might be able to write a more efficient version than this?
# Bad name?
getChompedSource : Parser c i p * -> Parser c i p (List i)
getChompedSource = \parser ->
    parser |> mapChompedSource (\l, _ -> l)

mapChompedSource : Parser c i p a, (List i, a -> b) -> Parser c i p b
mapChompedSource = \@Parser parse, f ->
        @Parser \s0 ->
            {val: a, state: s1, backtrackable: b1} <- try (parse s0)

            when s1.offset - s0.offset is
                x if x >= 0 ->
                    chomped = s0.src |> List.sublist {start: s0.offset, len: x |> Num.toNat}
                    Ok {val: f chomped a, 
                        state: s1, backtrackable: b1}

                _ ->
                    Err {bag: fromState s1 OutOfBounds, backtrackable: b1}
       

#future ref: 
#nextWhile: (State i, i -> State i), (i -> Bool) -> Parser i {}
chompWhile: (i -> Bool) -> Parser * i * {}
chompWhile = \isGood ->
    @Parser \s ->
        initialPos = s.offset
        finalPos = s.src |> List.walkFromUntil s.offset initialPos \p, c ->
            if isGood c then
                Continue (p + 1)
            else
                Break p

        Ok {val: {}, state: {s & offset: finalPos}, 
            backtrackable: if finalPos > initialPos then No else Yes}


chompUntil : Token i p -> Parser * i p {}
            | i has Eq
chompUntil = \{tok, expecting} ->
    @Parser \s ->
        when findSubSource tok s.offset s.src is
            Ok newOffset ->
                Ok {val: {}, state: {s & offset: newOffset},
                    backtrackable: if List.isEmpty tok then Yes else No}
            Err _ ->
                Err {bag: Append Empty (fromState s expecting), backtrackable: Yes}


chompUntilEndOr : List i -> Parser * i * {}
                    | i has Eq 
chompUntilEndOr = \lst ->
    @Parser \s ->
        initialOffset = s.offset
        when findSubSource lst initialOffset s.src is
            Ok newOffset ->
                Ok {val: {}, state: {s & offset: newOffset},
                    backtrackable: if newOffset == initialOffset then Yes else No}
            Err _ ->
                adjustedOffset = List.len s.src
                Ok {val: {}, state: {s & offset: adjustedOffset},
                    backtrackable: if adjustedOffset == initialOffset then Yes else No}

# -- CONTEXT ---------

inContext : Parser context i p v, context -> Parser context i p v
inContext = \@Parser parse, context ->
    @Parser \s0 ->
        contextStack = s0.context |> List.prepend {offset: s0.offset, context: context}
        step <- try (parse (s0 |> changeContext contextStack))
        
        Ok { step & state: step.state |> changeContext s0.context}


changeContext : State c i, List (Located c) -> State c i
changeContext = \s, newContext ->
    {s & context: newContext}


# -- LOOP ---------

Step state a : [Loop state, Done a]

loop : state, (state -> Parser c i p (Step state a)) -> Parser c i p a
loop = \state, callback ->
    @Parser \s ->
        loopImpl Yes state callback s


# -- BACKTRACKABLE ---------

backtrackable : Parser c i p a -> Parser c i p a
backtrackable = \@Parser parse ->
    @Parser \s0 ->
        when parse s0 is
            Err err ->
                Err {err & backtrackable: Yes}
            Ok step ->
                Ok {step & backtrackable: Yes}

commit : a -> Parser * * * a
commit = \a ->
    @Parser \s ->
        Ok {val: a, state: s, backtrackable: No}

# -- POSITION

getOffset: Parser c i p Nat
getOffset =
    @Parser \s ->
        Ok {val: s.offset, state: s, backtrackable: Yes}

getSource: Parser c i p (List i)
getSource =
    @Parser \s ->
        Ok {val: s.src, state: s, backtrackable: Yes}        
# -- TOKEN & SYMBOL

Token i p : { tok: List i, expecting: Problem p}

token : Token i p -> Parser c i p {} | i has Eq
token = \{tok, expecting} ->
    @Parser \s ->
        when isSubSource tok s.offset s.src is
            Ok newOffset ->
                Ok {val: {}, state: {s & offset: newOffset},
                    backtrackable: if List.isEmpty tok then Yes else No}
            Err _ ->
                Err {bag: Append Empty (fromState s expecting), backtrackable: Yes}

symbol : Token i p -> Parser c i p {} | i has Eq
symbol =
  token

# -- LOW LEVEL ---------

isSubSource : List i, Nat, List i -> Result Nat [OutOfBounds]
            | i has Eq
isSubSource = \smallSrc, offset, bigSrc ->
    if offset + List.len smallSrc <= List.len bigSrc then

        smallSrc |> List.walkTry offset \p, c ->
            char <- Result.try (List.get bigSrc p)

            if c == char then
                Ok (p + 1)
            else
                Err OutOfBounds
                
    else
        Err OutOfBounds


findSubSource : List i, Nat, List i -> Result Nat [OutOfBounds]
                | i has Eq
findSubSource = \smallSrc, offset, bigSrc -> 
   smallLen = List.len smallSrc

   if offset + smallLen <= List.len bigSrc then

        finalPos = 
            bigSrc |> List.walkFromUntil offset offset \p,c ->
                subSrc = List.sublist bigSrc {start: p, len: smallLen}

                if smallSrc == subSrc then
                    Break (p + 1)
                else 
                    Continue (p + 1)


        if finalPos == List.len bigSrc then
            Err OutOfBounds
        else 
            Ok finalPos
    
    else Err OutOfBounds

# ---- INTERNAL HELPER FUNCTIONS -------

#Could this list be reversed? I.e. could one use append instead of prepend? That would be more performant.
bagToList : Bag c p, List (DeadEnd c p) -> List (DeadEnd c p)
bagToList = \bag, list ->
    when bag is
        Empty ->
            list
        
        AddRight bag1 x ->
            bag1 |> bagToList (list |> List.prepend x)
        
        Append bag1 bag2 ->
            bag1 |> bagToList (bag2 |> bagToList list)

fromState : State c *, Problem p -> Bag c p
fromState = \s, p ->
  AddRight Empty {offset: s.offset, problem: p, contextStack: s.context}

fromInfo : Nat, Problem p, List (Located c) -> Bag c p
fromInfo = \offset, p, cs ->
    AddRight Empty {offset: offset, problem: p, contextStack: cs}              

#Helper function for many and oneOrMore
manyImpl : Parser c i p a, List a, State c i -> PStep c i p (List a)
manyImpl = \@Parser parser, vals, s ->
    when parser s is
        Err {backtrackable: b, bag: _} ->
            Ok { val: vals, state: s, backtrackable: b }

        Ok { val: val, state: newState, backtrackable: _ } ->
            manyImpl (@Parser parser) (List.append vals val) newState  

# Helper function for sepBy
sepBy1 : Parser c i p v, Parser c i p * -> Parser c i p (List v)
sepBy1 = \parser, separator ->
    parserFollowedBySep =
        const (\val -> val)
        |> skip separator
        |> keep parser

    const (\val -> \vals -> List.prepend vals val)
    |> keep parser
    |> keep (many parserFollowedBySep)


#Helper function for loop
loopImpl : Backtrackable, state, (state -> Parser c i p (Step state a)), State c i -> PStep c i p a
loopImpl = \b, state, callback, s0 ->
    @Parser parse = callback state
    {val: step, state: s1, backtrackable: b1} <- try (parse s0)
    when step is
        Loop newState ->
            loopImpl (b |> and b1) newState callback s1

        Done result ->
            Ok {val: result, state: s1,backtrackable: (b |> and b1)}    