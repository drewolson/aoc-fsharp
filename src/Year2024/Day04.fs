namespace Year2024

module Day04 =
    open XParsec
    open XParsec.CharParsers
    open Aoc.Parser

    let pGrid =
        sepEndBy1 (many1 (asciiLetter |>> string) |>> Array.ofSeq) newline
        |>> Array.ofSeq

    let coordsToWord (grid: array<array<string>>) coords =
        coords
        |> Array.map (fun ray ->
            ray
            |> Array.map (fun (x, y) ->
                try
                    grid.[x].[y]
                with _ ->
                    "")
            |> String.concat "")

    let validWords grid (x, y) =
        [| [| x, y; x + 1, y; x + 2, y; x + 3, y |]
           [| x, y; x, y + 1; x, y + 2; x, y + 3 |]
           [| x, y; x + 1, y + 1; x + 2, y + 2; x + 3, y + 3 |]
           [| x, y; x + 1, y - 1; x + 2, y - 2; x + 3, y - 3 |]
           [| x, y; x - 1, y + 1; x - 2, y + 2; x - 3, y + 3 |]
           [| x, y; x - 1, y; x - 2, y; x - 3, y |]
           [| x, y; x, y - 1; x, y - 2; x, y - 3 |]
           [| x, y; x - 1, y - 1; x - 2, y - 2; x - 3, y - 3 |] |]
        |> coordsToWord grid
        |> Array.filter (fun w -> w = "XMAS")
        |> Array.length

    let validWords' (grid: array<array<string>>) (x, y) =
        [| [| x - 1, y + 1; x, y; x + 1, y - 1 |]
           [| x - 1, y - 1; x, y; x + 1, y + 1 |] |]
        |> coordsToWord grid
        |> Array.forall (fun s -> s = "MAS" || s = "SAM")

    let part1 input =
        let grid = parse pGrid input
        let ysize = Array.length grid
        let xsize = Array.length grid[0]

        seq { 0..ysize }
        |> Seq.collect (fun y -> seq { 0..xsize } |> Seq.map (fun x -> x, y))
        |> Seq.sumBy (validWords grid)

    let part2 input =
        let grid = parse pGrid input
        let ysize = Array.length grid
        let xsize = Array.length grid[0]

        seq { 0..ysize }
        |> Seq.collect (fun y -> seq { 0..xsize } |> Seq.map (fun x -> x, y))
        |> Seq.filter (validWords' grid)
        |> Seq.length
