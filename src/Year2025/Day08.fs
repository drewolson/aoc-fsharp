namespace Year2025

module Day08 =
    open Aoc
    open XParsec
    open XParsec.CharParsers

    module UF =
        open System.Collections.Generic

        type t<'a> = Dictionary<'a, 'a>

        let create s =
            let d = new Dictionary<'a, 'a>()
            Seq.iter (fun e -> d[e] <- e) s
            d

        let rec find x (uf: t<'a>) =
            if uf[x] = x then
                x
            else
                uf[x] <- find uf[x] uf
                uf[x]

        let union a b (uf: t<'a>) = uf[find a uf] <- find b uf

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

    let rec merge uf l count (pairs: list<(int64 * int64 * int64) * (int64 * int64 * int64)>) =
        match pairs with
        | [] -> -1L
        | (a, b) :: t ->
            let newCount = if UF.find a uf <> UF.find b uf then count + 1 else count

            if newCount = l - 1 then
                let ax, _, _ = a
                let bx, _, _ = b

                ax * bx
            else
                UF.union a b uf
                merge uf l newCount t


    let part1 n input =
        let points = input |> Parser.parse pInput |> List.ofSeq
        let uf = UF.create points

        points
        |> pairs
        |> List.sortBy (fun ((x1, y1, z1), (x2, y2, z2)) ->
            sqrt ((x1 - x2 |> float) ** 2 + (y1 - y2 |> float) ** 2 + (z1 - z2 |> float) ** 2))
        |> List.take n
        |> List.iter (fun (a, b) -> UF.union a b uf)

        points
        |> List.groupBy (fun p -> UF.find p uf)
        |> List.map (snd >> List.length)
        |> List.sortBy (fun i -> -i)
        |> List.take 3
        |> List.reduce (fun a b -> a * b)

    let part2 input =
        let points = input |> Parser.parse pInput |> List.ofSeq
        let uf = UF.create points

        let l = List.length points

        points
        |> pairs
        |> List.sortBy (fun ((x1, y1, z1), (x2, y2, z2)) ->
            sqrt ((x1 - x2 |> float) ** 2 + (y1 - y2 |> float) ** 2 + (z1 - z2 |> float) ** 2))
        |> merge uf l 0
