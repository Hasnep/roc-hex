interface Hex exposes [fromStr, toStr] imports [Utils]

## Convert a hexadecimal string to an integer
fromStr : Str -> [Ok I64, Err [InvalidHexStr]]
fromStr = \hexStr ->
    if Str.isEmpty hexStr then
        Err InvalidHexStr
    else
        isNegative = hexStr |> Str.startsWith "-"
        hexDigits = (if isNegative then Utils.trimLeftUnsafe hexStr "-" else hexStr) |> Str.graphemes |> List.map digitFromStr
        if List.any hexDigits Result.isErr then
            Err InvalidHexStr
        else
            hexDigits
            |> List.map (\x -> Utils.unwrap x "This should never happen because we already checked that all the graphemes are valid hex digits.")
            |> List.reverse
            |> Utils.enumerate
            |> List.map (\(index, hexDigit) -> (Num.powInt 16 index) * (Num.toNat hexDigit))
            |> List.sum
            |> Num.toI64
            |> (if isNegative then Num.neg else Utils.identity)
            |> Ok

expect
    out = fromStr ""
    out == Err InvalidHexStr

expect
    out =
        ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "1a", "1b", "1c", "1d", "1e", "1f", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "2a", "2b", "2c", "2d", "2e", "2f", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "3a", "3b", "3c", "3d", "3e", "3f", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "4a", "4b", "4c", "4d", "4e", "4f", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "5a", "5b", "5c", "5d", "5e", "5f", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "6a", "6b", "6c", "6d", "6e", "6f", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "7a", "7b", "7c", "7d", "7e", "7f", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "8a", "8b", "8c", "8d", "8e", "8f", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99", "9a", "9b", "9c", "9d", "9e", "9f", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "aa", "ab", "ac", "ad", "ae", "af", "b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "ba", "bb", "bc", "bd", "be", "bf", "c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "ca", "cb", "cc", "cd", "ce", "cf", "d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "da", "db", "dc", "dd", "de", "df", "e0", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "ea", "eb", "ec", "ed", "ee", "ef", "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "fa", "fb", "fc", "fd", "fe", "ff", "100"]
        |> List.map Hex.fromStr
    out == List.range { start: At 0, end: At 256 } |> List.map Ok

expect
    out = ["-1", "-2", "-3", "-4", "-5", "-6", "-7", "-8", "-9", "-a", "-b", "-c", "-d", "-e", "-f", "-10", "-11", "-12", "-13", "-14", "-15", "-16", "-17", "-18", "-19", "-1a", "-1b", "-1c", "-1d", "-1e", "-1f", "-20", "-21", "-22", "-23", "-24", "-25", "-26", "-27", "-28", "-29", "-2a", "-2b", "-2c", "-2d", "-2e", "-2f", "-30", "-31", "-32", "-33", "-34", "-35", "-36", "-37", "-38", "-39", "-3a", "-3b", "-3c", "-3d", "-3e", "-3f", "-40", "-41", "-42", "-43", "-44", "-45", "-46", "-47", "-48", "-49", "-4a", "-4b", "-4c", "-4d", "-4e", "-4f", "-50", "-51", "-52", "-53", "-54", "-55", "-56", "-57", "-58", "-59", "-5a", "-5b", "-5c", "-5d", "-5e", "-5f", "-60", "-61", "-62", "-63", "-64", "-65", "-66", "-67", "-68", "-69", "-6a", "-6b", "-6c", "-6d", "-6e", "-6f", "-70", "-71", "-72", "-73", "-74", "-75", "-76", "-77", "-78", "-79", "-7a", "-7b", "-7c", "-7d", "-7e", "-7f", "-80", "-81", "-82", "-83", "-84", "-85", "-86", "-87", "-88", "-89", "-8a", "-8b", "-8c", "-8d", "-8e", "-8f", "-90", "-91", "-92", "-93", "-94", "-95", "-96", "-97", "-98", "-99", "-9a", "-9b", "-9c", "-9d", "-9e", "-9f", "-a0", "-a1", "-a2", "-a3", "-a4", "-a5", "-a6", "-a7", "-a8", "-a9", "-aa", "-ab", "-ac", "-ad", "-ae", "-af", "-b0", "-b1", "-b2", "-b3", "-b4", "-b5", "-b6", "-b7", "-b8", "-b9", "-ba", "-bb", "-bc", "-bd", "-be", "-bf", "-c0", "-c1", "-c2", "-c3", "-c4", "-c5", "-c6", "-c7", "-c8", "-c9", "-ca", "-cb", "-cc", "-cd", "-ce", "-cf", "-d0", "-d1", "-d2", "-d3", "-d4", "-d5", "-d6", "-d7", "-d8", "-d9", "-da", "-db", "-dc", "-dd", "-de", "-df", "-e0", "-e1", "-e2", "-e3", "-e4", "-e5", "-e6", "-e7", "-e8", "-e9", "-ea", "-eb", "-ec", "-ed", "-ee", "-ef", "-f0", "-f1", "-f2", "-f3", "-f4", "-f5", "-f6", "-f7", "-f8", "-f9", "-fa", "-fb", "-fc", "-fd", "-fe", "-ff", "-100"] |> List.map Hex.fromStr
    out == List.range { start: At -1, end: At -256, step: -1 } |> List.map Ok

## Convert an integer to a hexadecimal string
toStr : I64 -> Str
toStr = \number ->
    (negativeSign, numberAbs) = if Num.isNegative number then ("-", Num.abs number) else ("", number)
    numberToHexDigits = \n, digitsAccumulator ->
        (quotient, remainder) = (n // 16, n % 16)
        if quotient == 0 then
            digitsAccumulator |> List.prepend (remainder |> Num.toU8 |> digitToStrUnsafe)
        else
            numberToHexDigits quotient (digitsAccumulator |> List.prepend (remainder |> Num.toU8 |> digitToStrUnsafe))
    digits = numberToHexDigits numberAbs []
    digits |> List.prepend negativeSign |> Str.joinWith ""

expect
    out = List.range { start: At 0, end: At 256 } |> List.map toStr
    out == ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "1a", "1b", "1c", "1d", "1e", "1f", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "2a", "2b", "2c", "2d", "2e", "2f", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "3a", "3b", "3c", "3d", "3e", "3f", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "4a", "4b", "4c", "4d", "4e", "4f", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "5a", "5b", "5c", "5d", "5e", "5f", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "6a", "6b", "6c", "6d", "6e", "6f", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "7a", "7b", "7c", "7d", "7e", "7f", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "8a", "8b", "8c", "8d", "8e", "8f", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99", "9a", "9b", "9c", "9d", "9e", "9f", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "aa", "ab", "ac", "ad", "ae", "af", "b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "ba", "bb", "bc", "bd", "be", "bf", "c0", "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "ca", "cb", "cc", "cd", "ce", "cf", "d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "da", "db", "dc", "dd", "de", "df", "e0", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "ea", "eb", "ec", "ed", "ee", "ef", "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "fa", "fb", "fc", "fd", "fe", "ff", "100"]

expect
    out = List.range { start: At -1, end: At -256, step: -1 } |> List.map toStr
    out == ["-1", "-2", "-3", "-4", "-5", "-6", "-7", "-8", "-9", "-a", "-b", "-c", "-d", "-e", "-f", "-10", "-11", "-12", "-13", "-14", "-15", "-16", "-17", "-18", "-19", "-1a", "-1b", "-1c", "-1d", "-1e", "-1f", "-20", "-21", "-22", "-23", "-24", "-25", "-26", "-27", "-28", "-29", "-2a", "-2b", "-2c", "-2d", "-2e", "-2f", "-30", "-31", "-32", "-33", "-34", "-35", "-36", "-37", "-38", "-39", "-3a", "-3b", "-3c", "-3d", "-3e", "-3f", "-40", "-41", "-42", "-43", "-44", "-45", "-46", "-47", "-48", "-49", "-4a", "-4b", "-4c", "-4d", "-4e", "-4f", "-50", "-51", "-52", "-53", "-54", "-55", "-56", "-57", "-58", "-59", "-5a", "-5b", "-5c", "-5d", "-5e", "-5f", "-60", "-61", "-62", "-63", "-64", "-65", "-66", "-67", "-68", "-69", "-6a", "-6b", "-6c", "-6d", "-6e", "-6f", "-70", "-71", "-72", "-73", "-74", "-75", "-76", "-77", "-78", "-79", "-7a", "-7b", "-7c", "-7d", "-7e", "-7f", "-80", "-81", "-82", "-83", "-84", "-85", "-86", "-87", "-88", "-89", "-8a", "-8b", "-8c", "-8d", "-8e", "-8f", "-90", "-91", "-92", "-93", "-94", "-95", "-96", "-97", "-98", "-99", "-9a", "-9b", "-9c", "-9d", "-9e", "-9f", "-a0", "-a1", "-a2", "-a3", "-a4", "-a5", "-a6", "-a7", "-a8", "-a9", "-aa", "-ab", "-ac", "-ad", "-ae", "-af", "-b0", "-b1", "-b2", "-b3", "-b4", "-b5", "-b6", "-b7", "-b8", "-b9", "-ba", "-bb", "-bc", "-bd", "-be", "-bf", "-c0", "-c1", "-c2", "-c3", "-c4", "-c5", "-c6", "-c7", "-c8", "-c9", "-ca", "-cb", "-cc", "-cd", "-ce", "-cf", "-d0", "-d1", "-d2", "-d3", "-d4", "-d5", "-d6", "-d7", "-d8", "-d9", "-da", "-db", "-dc", "-dd", "-de", "-df", "-e0", "-e1", "-e2", "-e3", "-e4", "-e5", "-e6", "-e7", "-e8", "-e9", "-ea", "-eb", "-ec", "-ed", "-ee", "-ef", "-f0", "-f1", "-f2", "-f3", "-f4", "-f5", "-f6", "-f7", "-f8", "-f9", "-fa", "-fb", "-fc", "-fd", "-fe", "-ff", "-100"]

# Digit conversion

digitFromStr : Str -> [Ok U8, Err [InvalidHexDigit]]
digitFromStr = \hexDigitStr ->
    when hexDigitStr is
        "0" -> Ok 0
        "1" -> Ok 1
        "2" -> Ok 2
        "3" -> Ok 3
        "4" -> Ok 4
        "5" -> Ok 5
        "6" -> Ok 6
        "7" -> Ok 7
        "8" -> Ok 8
        "9" -> Ok 9
        "a" | "A" -> Ok 10
        "b" | "B" -> Ok 11
        "c" | "C" -> Ok 12
        "d" | "D" -> Ok 13
        "e" | "E" -> Ok 14
        "f" | "F" -> Ok 15
        _ -> Err InvalidHexDigit

expect
    out = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "A", "b", "B", "c", "C", "d", "D", "e", "E", "f", "F", "x"] |> List.map digitFromStr
    out == [Ok 0, Ok 1, Ok 2, Ok 3, Ok 4, Ok 5, Ok 6, Ok 7, Ok 8, Ok 9, Ok 10, Ok 10, Ok 11, Ok 11, Ok 12, Ok 12, Ok 13, Ok 13, Ok 14, Ok 14, Ok 15, Ok 15, Err InvalidHexDigit]

digitToStrUnsafe : U8 -> Str
digitToStrUnsafe = \digit ->
    when digit is
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        9 -> "9"
        10 -> "a"
        11 -> "b"
        12 -> "c"
        13 -> "d"
        14 -> "e"
        15 -> "f"
        _ -> crash "You promised this would never happen!"

expect
    out = List.range { start: At 0, end: At 15 } |> List.map digitToStrUnsafe
    out == ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f"]
