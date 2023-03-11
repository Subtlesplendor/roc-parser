interface Example 
    exposes []
    imports []


example : [Foo Str, Bar Bool], _ -> [Foo Str, Bar Bool]a
example = \tag, a ->
    when tag is
        Foo str ->
            Bar (Str.isEmpty str)
        Bar x if x == Bool.true -> 
            Bar Bool.true
        Bar _ -> 
            a