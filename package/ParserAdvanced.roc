interface ParserAdvanced 
    exposes [run,
             succeed,
             problem,
             map,
             map2,
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
             fromState]
    imports []


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


#------------------        

succeed : a -> Parser c x a
succeed = \a ->
    @Parser \s -> Good Bool.false a s

problem : x -> Parser c x a
problem = \x ->
    @Parser \s -> Bad Bool.false (fromState s x)    


#---------------------

map : Parser c x a, (a -> b) -> Parser c x b
map = \@Parser parse, func ->
    @Parser \s0 ->
        when parse s0 is
            Good p a s1 -> Good p (func a) s1
            Bad p x -> Bad p x

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

lazy : ({} -> Parser c x a) -> Parser c x a
lazy = \thunk ->
  @Parser \s ->
        @Parser parse = thunk {}
        parse s

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


#-------------------------

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



#----------

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

