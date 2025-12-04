namespace Year2025

module Day04 =
    let parse (input: string) =
        let grid =
            input.TrimEnd().Split [| '\n' |]
            |> Array.map (fun row -> row |> Array.ofSeq |> Array.map string)
            |> array2D

        let coords =
            seq {
                for r in 0 .. grid.GetLength 0 - 1 do
                    for c in 0 .. grid.GetLength 1 - 1 do
                        r, c
            }

        grid, Array.ofSeq coords

    let get (grid: string[,]) (r, c) =
        try
            grid[r, c]
        with _ ->
            ""

    let accessible grid (r, c) =
        if get grid (r, c) <> "@" then
            false
        else
            let c =
                [| r - 1, c - 1
                   r - 1, c
                   r - 1, c + 1
                   r, c + 1
                   r, c - 1
                   r + 1, c - 1
                   r + 1, c
                   r + 1, c + 1 |]
                |> Array.map (get grid)
                |> Array.filter (fun c -> c = "@")
                |> Array.length

            c < 4

    let remove grid coords =
        let rec aux sum =
            let rem = Array.filter (accessible grid) coords

            if Seq.isEmpty rem then
                sum
            else
                for r, c in rem do
                    grid[r, c] <- "."

                aux (sum + Seq.length rem)

        aux 0

    let part1 input =
        let grid, coords = parse input

        coords |> Array.filter (accessible grid) |> Array.length

    let part2 input =
        let grid, coords = parse input

        remove grid coords
