interface Utils exposes [unwrap, enumerate, zip, identity] imports []

unwrap : [Ok a, Err _], Str -> a
unwrap = \x, message ->
    when x is
        Ok v -> v
        Err _ -> crash message

expect
    out = "hi" |> Ok |> unwrap "Oh no!"
    out == "hi"

zip = \listA, listB ->
    List.map2 listA listB (\a, b -> (a, b))

expect
    out = zip [A, B, C] [3, 2, 1]
    out == [(A, 3), (B, 2), (C, 1)]

enumerate = \list ->
    List.range { start: At 0, end: At (List.len list) } |> zip list

expect
    out = enumerate [A, B, C]
    out == [(0, A), (1, B), (2, C)]

identity = \x -> x

expect
    out = identity A
    out == A
