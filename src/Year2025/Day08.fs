namespace Year2025

module Day08 =
    open Aoc
    open XParsec
    open XParsec.CharParsers

    module UF =
        open System.Collections.Generic

        type t<'a> =
            { parents: Dictionary<'a, 'a>
              items: array<'a> }

        let create s =
            let d = new Dictionary<'a, 'a>()
            Seq.iter (fun e -> d[e] <- e) s
            { parents = d; items = Array.ofSeq s }

        let rec find x uf =
            if uf.parents[x] = x then
                x
            else
                uf.parents[x] <- find uf.parents[x] uf
                uf.parents[x]

        let union a b uf = uf.parents[find a uf] <- find b uf

        let components uf =
            uf.items |> Array.groupBy (fun e -> find e uf) |> Array.map snd

    let pPoint =
        parser {
            let! x = pint64 .>> pchar ','
            let! y = pint64 .>> pchar ','
            let! z = pint64

            return x, y, z
        }

    let pInput = Parser.sepEndBy' pPoint newline

    let rec pairs l =
        match l with
        | []
        | [ _ ] -> []
        | h :: t -> t |> List.map (fun e -> h, e) |> List.append (pairs t)

    let rec merge uf pairs =
        match pairs with
        | [] -> -1L
        | (a, b) :: t ->
            UF.union a b uf

            if Array.length (UF.components uf) = 1 then
                let ax, _, _ = a
                let bx, _, _ = b

                ax * bx
            else
                merge uf t

    let part1 n input =
        let points = input |> Parser.parse pInput |> List.ofSeq
        let uf = UF.create points

        points
        |> pairs
        |> List.sortBy (fun ((x1, y1, z1), (x2, y2, z2)) ->
            sqrt ((x1 - x2 |> float) ** 2 + (y1 - y2 |> float) ** 2 + (z1 - z2 |> float) ** 2))
        |> List.take n
        |> List.iter (fun (a, b) -> UF.union a b uf)

        uf
        |> UF.components
        |> Array.map Array.length
        |> Array.sortBy (fun i -> -i)
        |> Array.take 3
        |> Array.reduce (fun a b -> a * b)

    let part2 input =
        let points = input |> Parser.parse pInput |> List.ofSeq
        let uf = UF.create points

        points
        |> pairs
        |> List.sortBy (fun ((x1, y1, z1), (x2, y2, z2)) ->
            sqrt ((x1 - x2 |> float) ** 2 + (y1 - y2 |> float) ** 2 + (z1 - z2 |> float) ** 2))
        |> merge uf
