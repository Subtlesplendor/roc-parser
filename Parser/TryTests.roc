interface Parser.TryTests 
    exposes []
    imports [Parser.FullStack.{backtrackable, const, fail, try, onFail}, Stack]

notBacktrackable : Parser c i p a -> Parser c i p a
notBacktrackable = \@Parser parse ->
    @Parser \s0 ->
        when parse s0 is
            Err err ->
                Err {err & backtrackable: No}
            Ok step ->
                Ok {step & backtrackable: No}

# -------OK OK ---------------------
expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = backtrackable (const {})  # Ok{Yes}
    @Parser second = backtrackable (const {}) # Ok{Yes}
    finalParser = \s0 ->
        step1 <- try (first s0)
        second step1.state

    step = finalParser initial
    when step is
        Ok res ->
            res.backtrackable == Yes 
        Err _ -> Bool.false

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = backtrackable (const {})  # Ok{Yes}
    @Parser second = notBacktrackable (const {}) # Ok{No}
    finalParser = \s0 ->
        step1 <- try (first s0)
        second step1.state

    step = finalParser initial
    when step is
        Ok res ->
            res.backtrackable == No 
        Err _ -> Bool.false        

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = notBacktrackable (const {})  # Ok{No}
    @Parser second = notBacktrackable (const {}) # Ok{No}
    finalParser = \s0 ->
        step1 <- try (first s0)
        second step1.state

    step = finalParser initial
    when step is
        Ok res ->
            res.backtrackable == No 
        Err _ -> Bool.false 

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = notBacktrackable (const {})  # Ok{No}
    @Parser second = backtrackable (const {}) # Ok{Yes}
    @Parser finalParser = @Parser \s0 ->
        step1 <- try (first s0)
        second step1.state

    step = finalParser initial
    when step is
        Ok res ->
            res.backtrackable == No 
        Err _ -> Bool.false

# -------OK Err ---------------------
expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = backtrackable (const {})  # Ok{Yes}
    @Parser second = backtrackable fail # Err{Yes}
    @Parser finalParser = @Parser \s0 ->
        step1 <- try (first s0)
        second step1.state

    step = finalParser initial
    when step is
        Ok _ ->
            Bool.false
        Err res -> 
            res.backtrackable == Yes

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = backtrackable (const {})  # Ok{Yes}
    @Parser second = notBacktrackable fail # Err{No}
    @Parser finalParser = @Parser \s0 ->
        step1 <- try (first s0)
        second step1.state

    step = finalParser initial
    when step is
        Ok _ ->
            Bool.false
        Err res -> 
            res.backtrackable == No       

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = notBacktrackable (const {})  # Ok{No}
    @Parser second = notBacktrackable fail # Err{No}
    @Parser finalParser = @Parser \s0 ->
        step1 <- try (first s0)
        second step1.state

    step = finalParser initial
    when step is
        Ok _ ->
            Bool.false
        Err res -> 
            res.backtrackable == No

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = notBacktrackable (const {})  # Ok{No}
    @Parser second = backtrackable fail # Err{Yes}
    @Parser finalParser = @Parser \s0 ->
        step1 <- try (first s0)
        second step1.state

    step = finalParser initial
    when step is
        Ok _ ->
            Bool.false
        Err res -> 
            res.backtrackable == No

# -------Err OK ---------------------

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = backtrackable fail  # Err{Yes}
    @Parser second = backtrackable (const {}) # Ok{Yes}
    @Parser finalParser = @Parser \s0 ->
        step1 <- try (first s0)
        second step1.state

    step = finalParser initial
    when step is
        Ok _ ->
            Bool.false
        Err err -> 
            err.backtrackable == Yes


expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = backtrackable fail  # Err{Yes}
    @Parser second = notBacktrackable (const {}) # Ok{No}
    @Parser finalParser = @Parser \s0 ->
        step1 <- try (first s0)
        second step1.state

    step = finalParser initial
    when step is
        Ok _ ->
            Bool.false
        Err err -> 
            err.backtrackable == Yes 

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = notBacktrackable fail  # Err{No}
    @Parser second = notBacktrackable (const {}) # Ok{No}
    @Parser finalParser = @Parser \s0 ->
        step1 <- try (first s0)
        second step1.state

    step = finalParser initial
    when step is
        Ok _ ->
            Bool.false
        Err err -> 
            err.backtrackable == No

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = notBacktrackable fail  # Err{No}
    @Parser second = backtrackable (const {}) # Ok{Yes}
    @Parser finalParser = @Parser \s0 ->
        step1 <- try (first s0)
        second step1.state

    step = finalParser initial
    when step is
        Ok _ ->
            Bool.false
        Err err -> 
            err.backtrackable == No                                                 


# -------Err Err ---------------------

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = backtrackable fail  # Err{Yes}
    @Parser second = backtrackable fail # Err{Yes}
    @Parser finalParser = @Parser \s0 ->
        step1 <- try (first s0)
        second step1.state

    step = finalParser initial
    when step is
        Ok _ ->
            Bool.false
        Err err -> 
            err.backtrackable == Yes


expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = backtrackable fail  # Err{Yes}
    @Parser second = notBacktrackable fail # Err{No}
    @Parser finalParser = @Parser \s0 ->
        step1 <- try (first s0)
        second step1.state

    step = finalParser initial
    when step is
        Ok _ ->
            Bool.false
        Err err -> 
            err.backtrackable == Yes 

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = notBacktrackable fail  # Err{No}
    @Parser second = notBacktrackable fail # Err{No}
    @Parser finalParser = @Parser \s0 ->
        step1 <- try (first s0)
        second step1.state

    step = finalParser initial
    when step is
        Ok _ ->
            Bool.false
        Err err -> 
            err.backtrackable == No

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = notBacktrackable fail  # Err{No}
    @Parser second = backtrackable fail # Err{Yes}
    @Parser finalParser = @Parser \s0 ->
        step1 <- try (first s0)
        second step1.state

    step = finalParser initial
    when step is
        Ok _ ->
            Bool.false
        Err err -> 
            err.backtrackable == No  







# --- OK OK --------
expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = backtrackable (const {})  # Ok{Yes}
    @Parser second = backtrackable (const {}) # Ok{Yes}
    @Parser finalParser = @Parser \s0 ->
        _ <- onFail (first s0)
        second s0

    step = finalParser initial
    when step is
        Ok res ->
            res.backtrackable == Yes 
        Err _ -> 
            Bool.false

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = backtrackable (const {})  # Ok{Yes}
    @Parser second = notBacktrackable (const {}) # Ok{No}
    @Parser finalParser = @Parser \s0 ->
        _ <- onFail (first s0)
        second s0

    step = finalParser initial
    when step is
        Ok res ->
            res.backtrackable == Yes 
        Err _ -> 
            Bool.false

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = notBacktrackable (const {})  # Ok{No}
    @Parser second = backtrackable (const {}) # Ok{Yes}
    @Parser finalParser = @Parser \s0 ->
        _ <- onFail (first s0)
        second s0

    step = finalParser initial
    when step is
        Ok res ->
            res.backtrackable == No 
        Err _ -> 
            Bool.false

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = notBacktrackable (const {})  # Ok{No}
    @Parser second = notBacktrackable (const {}) # Ok{No}
    @Parser finalParser = @Parser \s0 ->
        _ <- onFail (first s0)
        second s0

    step = finalParser initial
    when step is
        Ok res ->
            res.backtrackable == No 
        Err _ -> 
            Bool.false 


# --------- Ok Err ---------

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = backtrackable (const {})  # Ok{Yes}
    @Parser second = backtrackable fail # Err{Yes}
    @Parser finalParser = @Parser \s0 ->
        _ <- onFail (first s0)
        second s0

    step = finalParser initial
    when step is
        Ok res ->
            res.backtrackable == Yes 
        Err _ -> 
            Bool.false

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = backtrackable (const {})  # Ok{Yes}
    @Parser second = notBacktrackable fail # Err{No}
    @Parser finalParser = @Parser \s0 ->
        _ <- onFail (first s0)
        second s0

    step = finalParser initial
    when step is
        Ok res ->
            res.backtrackable == Yes 
        Err _ -> 
            Bool.false

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = notBacktrackable (const {})  # Ok{No}
    @Parser second = backtrackable fail # Err{Yes}
    @Parser finalParser = @Parser \s0 ->
        _ <- onFail (first s0)
        second s0

    step = finalParser initial
    when step is
        Ok res ->
            res.backtrackable == No 
        Err _ -> 
            Bool.false

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = notBacktrackable (const {})  # Ok{No}
    @Parser second = notBacktrackable fail # Err{No}
    @Parser finalParser = @Parser \s0 ->
        _ <- onFail (first s0)
        second s0

    step = finalParser initial
    when step is
        Ok res ->
            res.backtrackable == No 
        Err _ -> 
            Bool.false     


# ------- Err Ok -------

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = backtrackable fail  # Err{Yes}
    @Parser second = backtrackable (const {}) # Ok{Yes}
    @Parser finalParser = @Parser \s0 ->
        _ <- onFail (first s0)
        second s0

    step = finalParser initial
    when step is
        Ok res ->
            res.backtrackable == Yes 
        Err _ -> 
            Bool.false 

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = backtrackable fail  # Err{Yes}
    @Parser second = notBacktrackable (const {}) # Ok{No}
    @Parser finalParser = @Parser \s0 ->
        _ <- onFail (first s0)
        second s0

    step = finalParser initial
    when step is
        Ok res ->
            res.backtrackable == No 
        Err _ -> 
            Bool.false   

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = notBacktrackable fail  # Err{No}
    @Parser second = backtrackable (const {}) # Ok{Yes}
    @Parser finalParser = @Parser \s0 ->
        _ <- onFail (first s0)
        second s0

    step = finalParser initial
    when step is
        Ok res ->
            res.backtrackable == No 
        Err _ -> 
            Bool.false    

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = notBacktrackable fail  # Err{No}
    @Parser second = notBacktrackable (const {}) # Ok{No}
    @Parser finalParser = @Parser \s0 ->
        _ <- onFail (first s0)
        second s0

    step = finalParser initial
    when step is
        Ok res ->
            res.backtrackable == No 
        Err _ -> 
            Bool.false                                


# --- Err Err ------

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = backtrackable fail  # Err{Yes}
    @Parser second = backtrackable fail # Err{Yes}
    @Parser finalParser = @Parser \s0 ->
        _ <- onFail (first s0)
        second s0

    step = finalParser initial
    when step is
        Ok _ ->
             Bool.false
        Err err -> 
            err.backtrackable == Yes

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = backtrackable fail  # Err{Yes}
    @Parser second = notBacktrackable fail # Err{No}
    @Parser finalParser = @Parser \s0 ->
        _ <- onFail (first s0)
        second s0

    step = finalParser initial
    when step is
        Ok _ ->
             Bool.false
        Err err -> 
            err.backtrackable == No

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = notBacktrackable fail  # Err{No}
    @Parser second = backtrackable fail # Err{Yes}
    @Parser finalParser = @Parser \s0 ->
        _ <- onFail (first s0)
        second s0

    step = finalParser initial
    when step is
        Ok _ ->
             Bool.false
        Err err -> 
            err.backtrackable == No

expect
    initial = {src: ['A'], offset: 0, context: Stack.new}
    @Parser first = notBacktrackable fail  # Err{No}
    @Parser second = notBacktrackable fail # Err{No}
    @Parser finalParser = @Parser \s0 ->
        _ <- onFail (first s0)
        second s0

    step = finalParser initial
    when step is
        Ok _ ->
             Bool.false
        Err err -> 
            err.backtrackable == No               