interface ParserAdvanced 
    exposes [Parser,
             DeadEnd,
             run,
             succeed,
             problem,
             map,
             map2,
             fail,
             fromInfo,
             keep,
             skip,
             andThen,
             lazy,
             alt,
             oneOf,
             loop,
             commit,
             backtrackable,
             buildPrimitiveParser,
             fromState,
             symbol,
             keyword,
             end,
             getChompedString,
             mapChompedString,
             chompIf,
             chompWhile,
             chompUntil,
             chompUntilEndOr,
             getIndent,
             withIndent,
             token,
             Token,
             Step,
             getPosition,
             getCol,
             getRow,
             getSource,
             getOffset,
             inContext,
             spaces]
    imports []


# -- PARSERS ------------------ 


Parser context problem value := 
    State context -> PStep context problem value

#Backtrackable : [Backtrack, NoBacktrack]

#ParseResult context input problem value :
#    Result {backtrackable: Backtrackable, value: value, state: State context input} {backtrackable: Backtrackable, errors: List [ParserFailure context problem] }

RawStr: List U8 
RawChar: U8

PStep context problem value : 
    [Good Bool value (State context),
     Bad Bool (Bag context problem)]

State context :
  { src : RawStr,
    offset : Nat,
    indent : Nat,
    context : List (Located context),
    row : Nat,
    col : Nat }

Located context :
  { row : Nat,
    col : Nat,
    context : context }

Bag c x : [Empty, AddRight (Bag c x) (DeadEnd c x), Append (Bag c x) (Bag c x)]

DeadEnd context problem :
  { row : Nat,
    col : Nat,
    problem : problem,
    contextStack : List { row : Nat, col : Nat, context : context }
  }

buildPrimitiveParser : (State c -> PStep c x a) -> Parser c x a
buildPrimitiveParser = \fun ->
    @Parser fun


#-- RUN ------------------ 

run : Parser c x a, RawStr -> Result a (List (DeadEnd c x))
run = \(@Parser parse), str ->
    state = { src: str,
              offset: 1,
              indent: 1,
              context: [],
              row: 1,
              col: 1 }
    when parse state is 
        Good _ value _ -> Ok value
        Bad _ bag -> Err (bagToList bag [])

# -- PROBLEMS ------------------ 

bagToList : Bag c x, List (DeadEnd c x) -> List (DeadEnd c x)
bagToList = \bag, list ->
    when bag is
        Empty -> list
        AddRight b x -> bagToList b (List.prepend list x)
        Append b1 b2 -> bagToList b1 (bagToList b2 list)


fromState : State c, x -> Bag c x
fromState = \s, x ->
  AddRight Empty { row: s.row, col: s.col, problem: x, contextStack: s.context }


fromInfo : Nat, Nat, x, List (Located c) -> Bag c x
fromInfo = \row, col, x, context ->
  AddRight Empty { row: row, col: col, problem: x, contextStack: context }


# -- PRIMITIVES ------------------      

succeed : a -> Parser c x a
succeed = \a ->
    @Parser \s -> Good Bool.false a s

problem : x -> Parser c x a
problem = \x ->
    @Parser \s -> Bad Bool.false (fromState s x)    


# -- MAPPING ------------------ 

map : Parser c x a, (a -> b) -> Parser c x b
map = \@Parser parse, func ->
    @Parser \s0 ->
        when parse s0 is
            Good p a s1 -> Good p (func a) s1
            Bad p x -> Bad p x


#According to Semantics.md, the booleans should actually compose with &&. But in the code it uses ||. What gives?
map2 :  Parser c x a, Parser c x b, (a, b -> value) -> Parser c x value
map2 = \@Parser parseA, @Parser parseB, func ->
  @Parser \s0 ->
    when parseA s0 is
      Bad p x -> Bad p x
      Good p1 a s1 -> 
        when parseB s1 is
            Bad p2 x -> Bad (p1 || p2) x
            Good p2 b s2 -> Good (p1 || p2) (func a b) s2

keep : Parser c x (a -> b), Parser c x a -> Parser c x b
keep = \parseFunc, parseArg ->
    map2 parseFunc parseArg (\f, x -> f x)

skip : Parser c x keep, Parser c x ignore -> Parser c x keep
skip = \keepParser, ignoreParser ->
  map2 keepParser ignoreParser (\x, _ -> x)

# -- AND THEN ------------------ 

andThen : Parser c x a, (a -> Parser c x b) -> Parser c x b
andThen = \@Parser parseA, callback ->
  @Parser \s0 ->
    when parseA s0 is
      Bad p x ->
        Bad p x
      Good p1 a s1 ->
            @Parser parseB = callback a
            when parseB s1 is
                Bad p2 x ->
                    Bad (p1 || p2) x
                Good p2 b s2 ->
                    Good (p1 || p2) b s2

# -- LAZY ------------------ 

lazy : ({} -> Parser c x a) -> Parser c x a
lazy = \thunk ->
  @Parser \s ->
        @Parser parse = thunk {}
        parse s

# -- ONE OF ------------------ 

alt : Parser c x a, Parser c x a -> Parser c x a
alt = \@Parser first, @Parser second ->
    @Parser \s0 ->
        when first s0 is
            Good p1 b1 s1 -> 
                Good p1 b1 s1
            Bad p1 x1 ->
                when second s0 is
                    Good p2 b2 s2 -> 
                        Good p2 b2 s2
                    Bad p2 x2 ->
                        if p2 then
                            Bad p2 x2 
                        else
                            Bad (p1 || p2) (Append x1 x2) 

fail : Parser c x a
fail = @Parser \_ -> Bad Bool.false Empty

## Try a list of parsers in turn, until one of them succeeds
oneOf : List (Parser c x a) -> Parser c x a
oneOf = \parsers ->
    List.walkBackwards parsers fail (\laterParser, earlierParser -> alt earlierParser laterParser)


# -- LOOP ------------------ 

Step state a : [Loop state, Done a]

loop : state, (state -> Parser c x (Step state a)) -> Parser c x a
loop = \state, callback ->
  @Parser \s ->
    loopHelp Bool.false state callback s

loopHelp : Bool, state, (state -> Parser c x (Step state a)), State c -> PStep c x a
loopHelp = \p, state, callback, s0 ->
    @Parser parse = callback state
    when parse s0 is
        Good p1 step s1 ->
            when step is
                Loop newState ->
                    loopHelp (p || p1) newState callback s1
                Done result ->
                    Good (p || p1) result s1
        Bad p1 x ->
            Bad (p || p1) x



# -- BACKTRACKABLE ------------------ 

backtrackable : Parser c x a -> Parser c x a
backtrackable = \@Parser parse ->
  @Parser \s0 ->
    when parse s0 is
      Bad _ x ->
        Bad Bool.false x

      Good _ a s1 ->
        Good Bool.false a s1

commit : a -> Parser c x a
commit = \a ->
  @Parser \s -> Good Bool.true a s


# -- SYMBOL ------------------ 

symbol : Token x -> Parser c x {}
symbol =
  token

# -- KEYWORD ------------------ 

keyword : Token x -> Parser c x {}
keyword = \{str:kwd, prob: expecting} ->
    progress = !(List.isEmpty kwd)
    @Parser \s ->
        when isSubString kwd {offset: s.offset, row: s.row, col: s.col} s.src is
            Err _ ->
                Bad Bool.false (fromState s expecting)
            Ok pos if Result.isErr (isSubChar (\c -> c == 95) pos.offset s.src) -> #placeholder predicative    
                Bad Bool.false (fromState s expecting) 
            Ok p ->
                Good progress {}
                    { src: s.src,
                      offset: p.offset,
                      indent: s.indent,
                      context: s.context,
                      row: p.row,
                      col: p.col
                    }

# -- TOKEN ------------------ 

Token problem : {str : RawStr, prob: problem}

token : Token x -> Parser c x {}
token = \{str, prob} ->
    progress = !(List.isEmpty str)
    @Parser \s ->
        when isSubString str {offset:s.offset, row: s.row, col: s.col} s.src is
            Err _ -> 
                Bad Bool.false (fromState s prob)
            Ok pos -> 
                Good progress {} { src: s.src,
                                   offset: pos.offset,
                                    indent: s.indent,
                                    context: s.context,
                                    row: pos.row,
                                    col: pos.col }


# -- INT ------------------ 

# -- FLOAT ------------------ 

# -- NUMBER ------------------ 
# number
#   : { int : Result (Int * -> a) x
#     , hex : Result (Int * -> a) x
#     , octal : Result (Int *-> a) x
#     , binary : Result (Int *-> a) x
#     , float : Result (Float *-> a) x
#     , invalid : x
#     , expecting : x
#     }
#   -> Parser c x a
# number = \c ->
#     @Parser \s ->
#         # 0
#         if isUtf8Code 48 s.offset s.src then 

#             zeroOffset = s.offset + 1
#             baseOffset = zeroOffset + 1

#             # x
#             if isUtf8Code 120 zeroOffset s.src then
#                 finalizeInt c.invalid c.hex baseOffset (consumeBase16 baseOffset s.src) s
#             # o    
#             else if isUtf8Code 111 zeroOffset s.src then
#                 finalizeInt c.invalid c.octal baseOffset (consumeBase 8 baseOffset s.src) s
#             # b    
#             else if isUtf8Code 98 zeroOffset s.src then
#                 finalizeInt c.invalid c.binary baseOffset (consumeBase 2 baseOffset s.src) s
#             else
#                 finalizeFloat c.invalid c.expecting c.int c.float (zeroOffset, 0) s

#         else
#             finalizeFloat c.invalid c.expecting c.int c.float (consumeBase 10 s.offset s.src) s


# consumeBase : Int -> Int -> String -> (Int, Int)
# consumeBase =
#   Elm.Kernel.Parser.consumeBase


# consumeBase16 : Int -> String -> (Int, Int)
# consumeBase16 =
#   Elm.Kernel.Parser.consumeBase16


# finalizeInt : x -> Result x (Int -> a) -> Int -> (Int, Int) -> State c -> PStep c x a
# finalizeInt invalid handler startOffset (endOffset, n) s =
#   case handler of
#     Err x ->
#       Bad True (fromState s x)

#     Ok toValue ->
#       if startOffset == endOffset
#         then Bad (s.offset < startOffset) (fromState s invalid)
#         else Good True (toValue n) (bumpOffset endOffset s)


# bumpOffset : Int -> State c -> State c
# bumpOffset newOffset s =
#   { src = s.src
#   , offset = newOffset
#   , indent = s.indent
#   , context = s.context
#   , row = s.row
#   , col = s.col + (newOffset - s.offset)
#   }


# finalizeFloat : x -> x -> Result x (Int -> a) -> Result x (Float -> a) -> (Int, Int) -> State c -> PStep c x a
# finalizeFloat invalid expecting intSettings floatSettings intPair s =
#   let
#     intOffset = Tuple.first intPair
#     floatOffset = consumeDotAndExp intOffset s.src
#   in
#   if floatOffset < 0 then
#     Bad True (fromInfo s.row (s.col - (floatOffset + s.offset)) invalid s.context)

#   else if s.offset == floatOffset then
#     Bad False (fromState s expecting)

#   else if intOffset == floatOffset then
#     finalizeInt invalid intSettings s.offset intPair s

#   else
#     case floatSettings of
#       Err x ->
#         Bad True (fromState s invalid)

#       Ok toValue ->
#         case String.toFloat (String.slice s.offset floatOffset s.src) of
#           Nothing -> Bad True (fromState s invalid)
#           Just n -> Good True (toValue n) (bumpOffset floatOffset s)




# -- END ------------------ 

end : x -> Parser c x {}
end = \x ->
  @Parser \s ->
    if List.len s.src == s.offset then
      Good Bool.false {} s
    else
      Bad Bool.false (fromState s x)

# -- CHOMPED STRINGS -----------

getChompedString : Parser c x a -> Parser c x RawStr
getChompedString = \parser ->
  mapChompedString (\s, _ -> s) parser

mapChompedString : (RawStr, a -> b), Parser c x a -> Parser c x b
mapChompedString = \func, @Parser parse ->
  @Parser \s0 ->
    when parse s0 is
        Bad p x ->
            Bad p x

        Good p a s1 ->
            expect (s1.offset >= s0.offset)
            length = Num.toNat (s1.offset - s0.offset) #never negative
            Good p (func (List.sublist s0.src {start: s0.offset, len: length}) a) s1


# -- CHOMP IF -----------

#only consumes one thing. remove newOffset from issubchar?
chompIf : (RawChar -> Bool), x -> Parser c x {}
chompIf = \isGood, expecting ->
    @Parser \s ->
        when isSubChar isGood s.offset s.src is
            Err NewLine ->  
                Good Bool.true {}
                    { s & offset: s.offset + 1,
                          row: s.row + 1,
                          col: 1 }
            Err _ -> 
                Bad Bool.false (fromState s expecting)
    
            Ok newOffset ->
                Good Bool.true {}
                    { s & offset: newOffset, col: s.col + 1 }

# -- CHOMP WHILE -----------

#I tried to keep this faithful to the Elm library. But I think this would be better without using isSubChar, because I am kind of passing around another src without need.
chompWhile : (RawChar -> Bool) -> Parser c x {}
chompWhile = \isGood ->
    @Parser \s ->
        finalPos =
            s.src |> List.walkUntil {offset: s.offset, row: s.row, col: s.col} \pos, _ ->
                when isSubChar isGood pos.offset s.src is 
                    Err NewLine ->
                        Continue {offset: pos.offset + 1, row: pos.row + 1, col:1}
                    Err _ -> 
                        Break pos
                    Ok newOffset ->
                        Continue {pos & offset: newOffset, col: pos.col + 1}
        
        Good (s.offset < finalPos.offset) {} 
            { s & offset: finalPos.offset,
                  row: finalPos.row,
                  col: finalPos.col }
                    

# -- CHOMP UNTIL -----------

chompUntil : Token x -> Parser c x {}
chompUntil = \{str, prob} ->
    @Parser \s ->

        when findSubString str {offset:s.offset, row: s.row, col: s.col} s.src is
            Err (EndOfList pos) ->
                Bad Bool.false (fromInfo pos.row pos.col prob s.context)
            
            Ok pos ->
                Good (s.offset < pos.offset) {}
                    { s & offset: pos.offset,
                          row: pos.row,
                          col: pos.col }


chompUntilEndOr : RawStr -> Parser c x {}
chompUntilEndOr = \str ->
    @Parser \s ->
        newPos = 
            when findSubString str {offset: s.offset, row: s.row, col: s.col} s.src is
                Err (EndOfList pos) -> pos
                Ok pos -> pos

        Good (s.offset < newPos.offset) {}
        { s & offset: newPos.offset,
            row: newPos.row,
            col: newPos.col }

# -- CONTEXT -----------

inContext : context, Parser context x a -> Parser context x a
inContext = \con, @Parser parse ->
    @Parser \s0 ->
        stateInContext =
            s0.context 
            |> List.prepend {row: s0.row, col: s0.col, context: con} 
            |> changeContext s0 
            
        when parse stateInContext is
            Good p a s1 ->
                Good p a (changeContext s0.context s1)

            Bad _ _ as step ->
                step


changeContext : List (Located c), State c -> State c
changeContext = \newContext, s ->
    { s & context: newContext }

# -- INDENTATION -----------


getIndent : Parser c x Nat
getIndent =
  @Parser \s -> Good Bool.false s.indent s



withIndent : Nat, Parser c x a -> Parser c x a
withIndent = \newIndent, @Parser parse ->
    @Parser \s0 ->
        when parse (changeIndent newIndent s0) is
            Good p a s1 ->
                Good p a (changeIndent s0.indent s1)

            Bad p x ->
                Bad p x


changeIndent : Nat, State c -> State c
changeIndent =\newIndent, s ->
    { s & indent: newIndent }

# -- POSITION -----------

# This name is confusing due to my definition of Position.
getPosition : Parser c x {row: Nat, col: Nat}
getPosition =
  @Parser \s -> Good Bool.false {row: s.row, col:s.col} s


getRow : Parser c x Nat
getRow =
  @Parser \s -> Good Bool.false s.row s


getCol : Parser c x Nat
getCol =
  @Parser \s -> Good Bool.false s.col s


getOffset : Parser c x Nat
getOffset =
  @Parser \s -> Good Bool.false s.offset s


getSource : Parser c x RawStr
getSource =
  @Parser \s -> Good Bool.false s.src s

# -- LOW LEVEL HELPERS -----------

#These are most likely to be optimizable

Position : {offset: Nat, row: Nat, col: Nat}

newLine: RawChar
newLine = 10

carriageReturn: RawChar
carriageReturn = 13

space: RawChar
space = 32

posUpdate: Position, RawChar -> Position
posUpdate = \pos, c ->
    if c == newLine then
        {offset: pos.offset + 1, row: pos.row + 1, col: 1}
    else 
        {pos & offset: pos.offset + 1, col: pos.col + 1}

isSubString : RawStr, Position, RawStr -> Result Position [NotFound, OutOfBounds]
isSubString =\smallStr, pos, bigStr ->
    if pos.offset + List.len smallStr <= List.len bigStr then

        smallStr |> List.walkTry pos \p, c ->
            char <- Result.try (List.get bigStr p.offset)

            if c == char then
                Ok (pos |> posUpdate c)
            else
                Err NotFound
                
    else
        Err NotFound


expect 
    p = {offset: 0, row: 1, col: 1}
    smallStr = Str.toUtf8 "Hell"
    bigStr = Str.toUtf8 "Hello there"
    pos = Result.withDefault (isSubString smallStr p bigStr) p
    pos == {offset: 4, row: 1, col: 5}

expect 
    p = {offset: 0, row: 2, col: 4}
    smallStr = Str.toUtf8 "Hello\n there"
    bigStr = Str.toUtf8 "Hello\n there neighbour"
    pos = Result.withDefault (isSubString smallStr p bigStr) p
    pos == {offset: 12, row: 3, col: 7} 

expect 
    p = {offset: 4, row: 2, col: 5}
    smallStr = Str.toUtf8 "now"
    bigStr = Str.toUtf8 "Hi\n now"
    pos = Result.withDefault (isSubString smallStr p bigStr) p
    pos == {offset: 7, row: 2, col: 8} 

expect 
    p = {offset: 4, row: 2, col: 5}
    smallStr = Str.toUtf8 "now and again"
    bigStr = Str.toUtf8 "Hi\n now"
    res = (isSubString smallStr p bigStr)
    when res is 
        Err NotFound -> Bool.true 
        _ -> Bool.false  

expect 
    p = {offset: 0, row: 1, col: 1}
    smallStr = Str.toUtf8 "ice"
    bigStr = Str.toUtf8 "Hello\n there neighbour"
    res = (isSubString smallStr p bigStr)
    when res is 
        Err NotFound -> Bool.true 
        _ -> Bool.false          


isSubChar: (RawChar -> Bool), Nat, RawStr -> Result Nat [NotFound, NewLine, OutOfBounds]
isSubChar = \predicate, offset, str ->
    char <- Result.try (List.get str offset)
    if predicate char then
        Ok (offset + 1)
    else if char==newLine then
        Err NewLine
    else
        Err NotFound


# isUtf8Code: RawChar, Nat, RawStr -> Bool        
# isUtf8Code = \code, offset, str ->
#     when str |> List.get offset is
#         Err _ -> 
#             Bool.false
#         Ok char -> 
#             code == char
    

findSubString : RawStr, Position, RawStr -> Result Position [EndOfList Position]
findSubString = \smallStr, pos, bigStr ->
    smallLen = List.len smallStr

    finalPos = 
        bigStr |> List.walkFromUntil pos.offset pos \p,c ->
            subStr = List.sublist bigStr {start: p.offset, len: smallLen}

            newPos = pos |> posUpdate c
            if smallStr == subStr then
                Break newPos
            else 
                Continue newPos 

    if finalPos.offset == List.len bigStr then
        Err (EndOfList finalPos)
    else 
        Ok finalPos
    
    


# -- VARIABLES -----------

# -- SEQUENCES -----------

# -- WHITESPACE -----------

spaces : Parser c x {}
spaces =
  chompWhile (\c -> c == space || c == newLine || c == carriageReturn)




