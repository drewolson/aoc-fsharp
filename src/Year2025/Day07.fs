namespace Year2025

module Day07 =
    open Aoc

    let parse (input: string) =
        let lines = input.TrimEnd().Split [| '\n' |]
        let height = Array.length lines

        let cells =
            lines
            |> Array.indexed
            |> Array.collect (fun (r, l) -> l |> Array.ofSeq |> Array.indexed |> Array.map (fun (c, x) -> (r, c), x))

        let start = cells |> Array.find (fun (_, x) -> x = 'S') |> fst

        let splitters =
            cells |> Array.filter (fun (_, x) -> x = '^') |> Array.map fst |> Set.ofArray

        height, splitters, start

    let countSplits height splitters start =
        let rec aux beams count =
            if Set.isEmpty beams then
                count
            else
                let nextBeams =
                    beams
                    |> Set.map (fun (r, c) -> r + 1, c)
                    |> Set.filter (fun (r, _) -> r < height)

                let splits, next =
                    Set.fold
                        (fun (s, b) (r, c) ->
                            if Set.contains (r, c) splitters then
                                s + 1, b |> Set.add (r, c + 1) |> Set.add (r, c - 1)
                            else
                                s, Set.add (r, c) b)
                        (0, Set.empty)
                        nextBeams

                aux next (count + splits)

        aux (Set.singleton start) 0

    let countPaths height splitters start =
        let cache = Cache.empty ()

        let rec aux (r, c) =
            Cache.get_or cache (r, c) (fun _ ->
                if r > height then
                    1L
                elif Set.contains (r, c) splitters then
                    aux (r, c + 1) + aux (r, c - 1)
                else
                    aux (r + 1, c))

        aux start



    let part1 input = input |> parse |||> countSplits

    let part2 input = input |> parse |||> countPaths
