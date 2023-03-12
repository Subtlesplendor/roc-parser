interface ExampleParsers.Minimal.MinimalStr
    exposes [parseStr, Parser, char, strFromCodeunit, strFromScalar]
    imports [Parser.Minimal.{oneIf}]



#Parses raw strings

#Types
RawChar: U8
RawStr: List U8

Parser v : Parser.Minimal.Parser RawChar v

#Type converters (from old parser)

strFromRaw : RawStr -> Str
strFromRaw = \rawStr ->
    rawStr
    |> Str.fromUtf8
    |> Result.withDefault "Unexpected problem while turning a List U8 (that was originally a Str) back into a Str. This should never happen!"

strToRaw : Str -> RawStr
strToRaw = \str ->
    str |> Str.toUtf8

strFromScalar : U32 -> Str
strFromScalar = \scalarVal ->
    Str.appendScalar "" (Num.intCast scalarVal)
    |> Result.withDefault "Unexpected problem while turning a U32 (that was probably originally a scalar constant) into a Str. This should never happen!"

strFromCodeunit : U8 -> Str
strFromCodeunit = \cu ->
    strFromRaw [cu]

#Go!
isChar : RawChar -> (RawChar -> Bool)
isChar = \c1 -> \c2 ->
    c1 == c2

char : RawChar -> Parser RawChar
char = \c ->
    oneIf (isChar c)
    

parseStr : Parser a, Str -> Result a [OutOfBounds, Fail, Str]
parseStr = \parser, input ->
    parser
    |> parseRawStr (strToRaw input)

parseRawStr : Parser a, RawStr -> Result a [OutOfBounds, Fail, Str]
parseRawStr = \parser, input ->
    Parser.Minimal.run parser input