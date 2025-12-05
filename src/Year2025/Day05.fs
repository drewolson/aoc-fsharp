namespace Year2025

module Day05 =
    open Aoc.Parser
    open XParsec
    open XParsec.CharParsers

    let pRange =
        parser {
            let! a = pint64 .>> pchar '-'
            let! b = pint64

            return a, b
        }

    let pRanges = sepBy1' pRange newline

    let pInput =
        parser {
            let! ranges = pRanges .>> (newline .>> newline)
            let! ids = sepBy1' pint64 newline

            return ranges, ids
        }

    let fresh ranges id =
        ranges |> Seq.exists (fun (a, b) -> a <= id && id <= b)

    let rec merge ranges =
        match ranges with
        | [] -> []
        | [ r ] -> [ r ]
        | (sa, ea) :: (sb, eb) :: t when ea < sb -> (sa, ea) :: merge ((sb, eb) :: t)
        | (sa, ea) :: (_, eb) :: t when ea >= eb -> merge ((sa, ea) :: t)
        | (sa, ea) :: (sb, eb) :: t when ea >= sb && ea <= eb -> merge ((sa, eb) :: t)
        | _ -> []

    let part1 input =
        let ranges, ids = parse pInput input
        ids |> Seq.filter (fresh ranges) |> Seq.length

    let part2 =
        parse pInput
        >> fst
        >> List.ofSeq
        >> List.sort
        >> merge
        >> List.sumBy (fun (a, b) -> b - a + 1L)
