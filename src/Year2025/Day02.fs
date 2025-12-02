namespace Year2025

module Day02 =
    open XParsec
    open XParsec.CharParsers
    open Aoc.Parser

    let pRange =
        parser {
            let! s = pbigint .>> pchar '-'
            let! e = pbigint

            return seq { s..e }
        }

    let pRanges = sepBy' pRange (pchar ',')

    let mirrored n =
        let s = string n
        let l = String.length s

        l % 2 = 0 && s[0 .. l / 2 - 1] = s[l / 2 .. l - 1]

    let repeated n =
        let s = string n
        let l = String.length s

        seq { 0 .. l / 2 - 1 }
        |> Seq.map (fun i -> s[0..i] |> Seq.replicate (l / (i + 1)) |> String.concat "")
        |> Seq.exists (fun c -> c = s)

    let solve f =
        parse pRanges >> Seq.sumBy (Seq.filter f >> Seq.sum)

    let part1 = solve mirrored

    let part2 = solve repeated
