interface Parser.Advanced.Bytes 
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
             inContext]
    imports []


# -- PARSERS ------------------ 


Parser context problem value := 
    State context -> PStep context problem value

#Backtrackable : [Backtrack, NoBacktrack]

#ParseResult context input problem value :
#    Result {backtrackable: Backtrackable, value: value, state: State context input} {backtrackable: Backtrackable, errors: List [ParserFailure context problem] }

PStep context problem value : 
    [Good Bool value (State context),
     Bad Bool (Bag context problem)]

State context :
  { src : List U8,
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

run : Parser c x a, List U8 -> Result a (List (DeadEnd c x))
run = \(@Parser parse), input ->
    state = { src: input,
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

Token problem : {str : List U8, prob: problem}

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

# -- END ------------------ 

end : x -> Parser c x {}
end = \x ->
  @Parser \s ->
    if List.len s.src == s.offset then
      Good Bool.false {} s
    else
      Bad Bool.false (fromState s x)

# -- CHOMPED STRINGS -----------

getChompedString : Parser c x a -> Parser c x (List U8)
getChompedString = \parser ->
  mapChompedString (\s, _ -> s) parser

mapChompedString : (List U8, a -> b), Parser c x a -> Parser c x b
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
chompIf : (U8 -> Bool), x -> Parser c x {}
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
chompWhile : (U8 -> Bool) -> Parser c x {}
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


chompUntilEndOr : List U8 -> Parser c x {}
chompUntilEndOr = \lst ->
    @Parser \s ->
        newPos = 
            when findSubString lst {offset: s.offset, row: s.row, col: s.col} s.src is
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


getSource : Parser c x (List U8)
getSource =
  @Parser \s -> Good Bool.false s.src s

# -- LOW LEVEL HELPERS -----------

#These are most likely to be optimizable

Position : {offset: Nat, row: Nat, col: Nat}

newLine: U8
newLine = 10

posUpdate: Position, U8 -> Position
posUpdate = \pos, c ->
    if c == newLine then
        {offset: pos.offset + 1, row: pos.row + 1, col: 1}
    else 
        {pos & offset: pos.offset + 1, col: pos.col + 1}

#This is missnamed.
isSubString : List U8, Position, List U8 -> Result Position [NotFound, OutOfBounds]
isSubString =\smallLst, pos, bigLst ->
    if pos.offset + List.len smallLst <= List.len bigLst then

        smallLst |> List.walkTry pos \p, c ->
            char <- Result.try (List.get bigLst p.offset)

            if c == char then
                Ok (pos |> posUpdate c)
            else
                Err NotFound
                
    else
        Err NotFound


expect 
    p = {offset: 0, row: 1, col: 1}
    smallLst = Str.toUtf8 "Hell"
    bigLst = Str.toUtf8 "Hello there"
    pos = Result.withDefault (isSubString smallLst p bigLst) p
    pos == {offset: 4, row: 1, col: 5}

expect 
    p = {offset: 0, row: 2, col: 4}
    smallLst = Str.toUtf8 "Hello\n there"
    bigLst = Str.toUtf8 "Hello\n there neighbour"
    pos = Result.withDefault (isSubString smallLst p bigLst) p
    pos == {offset: 12, row: 3, col: 7} 

expect 
    p = {offset: 4, row: 2, col: 5}
    smallLst = Str.toUtf8 "now"
    bigLst = Str.toUtf8 "Hi\n now"
    pos = Result.withDefault (isSubString smallLst p bigLst) p
    pos == {offset: 7, row: 2, col: 8} 

expect 
    p = {offset: 4, row: 2, col: 5}
    smallLst = Str.toUtf8 "now and again"
    bigLst = Str.toUtf8 "Hi\n now"
    res = (isSubString smallLst p bigLst)
    when res is 
        Err NotFound -> Bool.true 
        _ -> Bool.false  

expect 
    p = {offset: 0, row: 1, col: 1}
    smallLst = Str.toUtf8 "ice"
    bigLst = Str.toUtf8 "Hello\n there neighbour"
    res = (isSubString smallLst p bigLst)
    when res is 
        Err NotFound -> Bool.true 
        _ -> Bool.false          


isSubChar: (U8 -> Bool), Nat, List U8 -> Result Nat [NotFound, NewLine, OutOfBounds]
isSubChar = \predicate, offset, lst ->
    char <- Result.try (List.get lst offset)
    if predicate char then
        Ok (offset + 1)
    else if char==newLine then
        Err NewLine
    else
        Err NotFound

#TODO
#isAsciiCode: Nat, Nat, List U8 -> Bool        


findSubString : List U8, Position, List U8 -> Result Position [EndOfList Position]
findSubString = \smallLst, pos, bigLst ->
    smallLen = List.len smallLst

    finalPos = 
        bigLst |> List.walkFromUntil pos.offset pos \p,c ->
            sbList = List.sublist bigLst {start: p.offset, len: smallLen}

            newPos = pos |> posUpdate c
            if smallLst == sbList then
                Break newPos
            else 
                Continue newPos 

    if finalPos.offset == List.len bigLst then
        Err (EndOfList finalPos)
    else 
        Ok finalPos
    
    


# -- VARIABLES -----------

# -- SEQUENCES -----------

# -- WHITESPACE -----------





