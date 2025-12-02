namespace Year2025

module Day02 =
    open XParsec
    open XParsec.CharParsers
    open Aoc.Parser

    let pRange =
        parser {
            let! s = pbigint .>> pchar '-'
            let! e = pbigint

            return s, e
        }

    let pRanges = sepBy' pRange (pchar ',')

    let invalidFrom n =
        let s = string n
        let l = String.length s

        let start =
            if l % 2 = 1 then
                pown 10I (l / 2)
            else
                let p = l / 2
                let a = bigint.Parse(s[0 .. p - 1])
                let b = bigint.Parse(s[p .. l - 1])
                if a >= b then a else a + 1I

        Seq.initInfinite (fun i -> start + bigint i)
        |> Seq.map (fun i ->
            let str = string i
            bigint.Parse(str + str))

    let invalidSum (s, e) =
        s |> invalidFrom |> Seq.takeWhile (fun i -> i <= e) |> Seq.sum

    let repeated n =
        let s = string n
        let l = String.length s

        seq { 0 .. l / 2 - 1 }
        |> Seq.map (fun i -> s[0..i] |> Seq.replicate (l / (i + 1)) |> String.concat "")
        |> Seq.exists (fun c -> c = s)

    let invalidSum' (s, e) =
        seq { s..e } |> Seq.filter repeated |> Seq.sum

    let part1 = parse pRanges >> Seq.sumBy invalidSum

    let part2 = parse pRanges >> Seq.sumBy invalidSum'
