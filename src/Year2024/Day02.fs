namespace Year2024

module Day02 =
    open Aoc
    open XParsec

    let pLine = Parser.sepBy1' CharParsers.pint32 (CharParsers.pchar ' ') |>> List.ofSeq

    let pInput = Parser.sepEndBy1' pLine CharParsers.newline

    let safe line =
        let rec aux l =
            match l with
            | a :: b :: rest ->
                let diff = abs (b - a)
                diff >= 1 && diff <= 3 && aux (b :: rest)
            | [ _ ] -> true
            | _ -> false

        aux line && (List.sort line = line || List.sort line = List.rev line)

    let rec expand line =
        match line with
        | [] -> [ [] ]
        | h :: t -> t :: (expand t |> List.map (fun l -> h :: l))

    let part1 = Parser.parse pInput >> Seq.filter safe >> Seq.length

    let part2 =
        Parser.parse pInput
        >> Seq.map expand
        >> Seq.filter (Seq.exists safe)
        >> Seq.length
