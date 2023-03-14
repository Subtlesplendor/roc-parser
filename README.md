# roc-parser
Work In Progress: a port of Elm's Parser library to Roc, empowered by Roc's type system. 

https://www.roc-lang.org/



## Differences to the Elm parser
This parser library works for parsing generic lists, not just strings. This means that some of the concepts of the Elm parser do not carry over. So the parsers now do not keep track of row, column, or indent --- because those do not make sense for arbitrary lists.

A slight complication is hence that to generate a nice error message that refers to e.g. a row and column requires a second pass through of the source to determine these quantities at the location of the error.

  
