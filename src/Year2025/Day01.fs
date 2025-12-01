namespace Year2025

module Day01 =
    open Aoc
    open XParsec
    open XParsec.CharParsers

    type Rotation =
        | Left of int
        | Right of int

    let pLeft = pchar 'L' >>. pint32 |>> Left

    let pRight = pchar 'R' >>. pint32 |>> Right

    let pRotations = Parser.sepEndBy1 (pLeft <|> pRight) newline

    let rec shrink n =
        if n > 99 then shrink (n - 100)
        elif n < 0 then shrink (n + 100)
        else n

    let expand r =
        match r with
        | Left i -> Seq.replicate i -1
        | Right i -> Seq.replicate i 1

    let part1 =
        Parser.parse pRotations
        >> Seq.mapFold
            (fun s r ->
                let n =
                    match r with
                    | Left i -> s - i
                    | Right i -> s + i
                    |> shrink

                n, n)
            50
        >> fst
        >> Seq.filter (fun l -> l = 0)
        >> Seq.length

    let part2 =
        Parser.parse pRotations
        >> Seq.collect expand
        >> Seq.fold
            (fun (c, s) n ->
                let x = c + n |> shrink
                if x = 0 then x, s + 1 else x, s

            )
            (50, 0)
        >> snd
