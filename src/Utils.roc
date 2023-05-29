interface Utils exposes [enumerate, unwrap, trimLeftUnsafe, identity] imports []

unwrap : [Ok a, Err _], Str -> a
unwrap = \x, message ->
    when x is
        Ok v -> v
        Err _ -> crash message

expect
    out = "hi" |> Ok |> unwrap "Oh no!"
    out == "hi"

enumerate = \list -> List.range { start: At 0, end: At (List.len list) } |> List.map2 list (\element, index -> (element, index))

expect
    out = enumerate ["a", "b", "c"]
    out == [(0, "a"), (1, "b"), (2, "c")]

trimLeftUnsafe = \s, toRemove -> (s |> Str.splitFirst toRemove |> unwrap (Str.joinWith ["You promised that `", toRemove, "` would be in the string!"] "")).after

expect
    out = trimLeftUnsafe "Hello, Roc." "Hello"
    out == ", Roc."

identity = \x -> x
