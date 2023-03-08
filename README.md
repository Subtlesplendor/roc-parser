# roc-parser
A port of Elm's Parser library to roc, empowered by Roc's type system





## TODO

  1. Implement Elm's parser as a parser of List U8.
  2. Generalize Elm's parser to generic input. (if possible!)
  3. "Rocify" the parser to better match the idioms of Roc.
      - Accumulate errors bit by bit?
      - combinators for backpassing notation?
      - Clever types for context?


## A few ideas

  1. For a generic `List a`, perhaps one should have a more generic position type.
  2. For sure it would be good to define Position = {...} and use it also within the state.
  3. Likely it would be good to have a Generic module that only has parser combinators and primitive parsers.
  
