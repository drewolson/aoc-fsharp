namespace Year2025

module Day04 =
    let parse (input: string) =
        let grid =
            input.TrimEnd().Split [| '\n' |]
            |> Array.map (fun row -> row |> Array.ofSeq |> Array.map string)
            |> array2D

        let xSize = grid.GetLength 1
        let ySize = grid.GetLength 0

        let coords =
            seq {
                for x in 0 .. xSize - 1 do
                    for y in 0 .. ySize - 1 do
                        x, y
            }

        grid, coords

    let get (grid: string[,]) (x, y) =
        try
            grid[y, x]
        with _ ->
            ""

    let accessible (grid: string[,]) (x, y) =
        if get grid (x, y) <> "@" then
            false
        else
            let c =
                [| x - 1, y - 1
                   x - 1, y
                   x - 1, y + 1
                   x, y + 1
                   x, y - 1
                   x + 1, y - 1
                   x + 1, y
                   x + 1, y + 1 |]
                |> Array.map (get grid)
                |> Array.filter (fun c -> c = "@")
                |> Array.length

            c < 4

    let remove grid coords =
        let rec aux sum =
            let r = Seq.filter (accessible grid) coords |> Array.ofSeq

            if Seq.isEmpty r then
                sum
            else
                for x, y in r do
                    grid[y, x] <- "."

                aux (sum + Seq.length r)

        aux 0

    let part1 input =
        let grid, coords = parse input

        coords |> Seq.filter (accessible grid) |> Seq.length

    let part2 input =
        let grid, coords = parse input

        remove grid coords
