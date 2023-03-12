interface Stack 
    exposes [Stack, new, size, peek, push, pop, #Principals
             fromList, toList, isEmpty, onTopOf, descend, ascend #Ergonomics
             ]
    imports []


Stack a := List a

# -- Core -------

new : Stack *
new = 
    @Stack []

size: Stack * -> Nat
size = \@Stack lst ->
    List.len lst    

peek: Stack a -> Result a [StackWasEmpty]
peek = \@Stack lst ->
    _ <- Result.onErr (List.last lst)
    Err StackWasEmpty    

push: Stack a, a -> Stack a
push = \@Stack lst, a ->
    @Stack (lst |> List.append a)

pop: Stack a -> Result {stack: Stack a, elem: a} [StackWasEmpty]
pop = \@Stack lst ->
    when List.last lst is
        Ok elem ->
            Ok {stack: @Stack (lst |> List.dropLast), elem}
        Err _ ->
            Err StackWasEmpty


# -- Ergonomics --------

isEmpty: Stack * -> Bool
isEmpty = \@Stack lst ->
    lst |> List.isEmpty

onTopOf : Stack a, Stack a -> Stack a
onTopOf = \@Stack l1, @Stack l2 ->
    @Stack (l2 |> List.concat l1)

fromList: List a -> Stack a
fromList = \lst ->
    @Stack (lst |> List.reverse)

toList: Stack a -> List a
toList = \@Stack lst ->
    lst |> List.reverse     

descend: Stack a, state, (state, a -> state) -> state
descend = \@Stack lst, s, f ->
    lst |> List.walkBackwards s f 

ascend: Stack a, state, (state, a -> state) -> state
ascend = \@Stack lst, s, f ->
    lst |> List.walk s f     