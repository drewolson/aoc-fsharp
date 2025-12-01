namespace Year2025

module Day01 =
    open Aoc.Parser
    open XParsec
    open XParsec.CharParsers

    let pLeft = pchar 'L' >>. pint32 |>> fun n -> -n

    let pRight = pchar 'R' >>. pint32

    let pRotations = sepEndBy1' (pLeft <|> pRight) newline

    let shrink n =
        let x = n % 100
        if x < 0 then x + 100 else x

    let countZeros: seq<int> -> int =
        Seq.scan (fun s n -> s + n |> shrink) 50
        >> Seq.filter (fun l -> l = 0)
        >> Seq.length

    let part1 = parse pRotations >> countZeros

    let part2 =
        parse pRotations
        >> Seq.collect (fun r -> if r < 0 then Seq.replicate -r -1 else Seq.replicate r 1)
        >> countZeros
