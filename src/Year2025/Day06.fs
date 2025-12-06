namespace Year2025

module Day06 =
    open Aoc.Parser
    open XParsec
    open XParsec.CharParsers

    let pLine =
        many (pchar ' ') >>. sepBy' pint64 (many1 (pchar ' ')) .>> many (pchar ' ')

    let pLines = sepBy' pLine newline

    let pOp = pchar '*' >>% (*) <|> (pchar '+' >>% (+))

    let pOps = sepBy' pOp (many1 (pchar ' '))

    let pIinput =
        parser {
            let! nums = pLines .>> many (pchar ' ') .>> spaces
            let! ops = pOps

            return nums, ops
        }

    let rec split =
        let rec aux acc s =
            if Array.isEmpty s then
                acc
            else
                let curr = s |> Array.takeWhile (fun i -> i <> "") |> Array.map int64

                let next =
                    s |> Array.skipWhile (fun i -> i <> "") |> Array.skipWhile (fun i -> i = "")

                aux (Array.append acc [| curr |]) next

        aux [||]

    let part1 input =
        let nums, ops = parse pIinput input

        nums
        |> Seq.transpose
        |> Seq.indexed
        |> Seq.sumBy (fun (i, l) -> Seq.reduce ops[i] l)

    let part2 (input: string) =
        let lines = input.TrimEnd().Split [| '\n' |]
        let nums = lines[.. lines.Length - 2]
        let ops = parse pOps lines[lines.Length - 1]

        nums
        |> Array.map Array.ofSeq
        |> Array.transpose
        |> Array.map (Array.map string >> String.concat "")
        |> Array.map (fun s -> s.Trim())
        |> split
        |> Array.indexed
        |> Array.sumBy (fun (i, l) -> Seq.reduce ops[i] l)
