namespace Year2024

module Day01 =
    open Aoc
    open XParsec

    let pLine =
        parser {
            let! x = CharParsers.pint32
            do! CharParsers.spaces1
            let! y = CharParsers.pint32

            return [| x; y |]
        }

    let pInput = Parser.sepEndBy1' pLine CharParsers.newline

    let part1 input =
        input
        |> Parser.parse pInput
        |> Array.transpose
        |> Array.map Array.sort
        |> Array.transpose
        |> Array.map (fun l -> l[0] - l[1] |> abs)
        |> Array.sum

    let part2 input =
        let buckets = input |> Parser.parse pInput |> Array.transpose

        buckets[0]
        |> Array.sumBy (fun n -> n * (buckets[1] |> Array.filter (fun x -> x = n) |> Array.length))
