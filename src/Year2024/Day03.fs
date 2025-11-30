namespace Year2024

module Day03 =
    open Aoc
    open XParsec
    open XParsec.CharParsers

    type Token =
        | Mul of int * int
        | Do
        | Dont

    let pDo = pstring "do()" >>% Do

    let pDont = pstring "don't()" >>% Dont

    let pMul =
        parser {
            let! a = pstring "mul(" >>. pint32 .>> pstring ","
            let! b = pint32 .>> pstring ")"

            return Mul(a, b)
        }

    let pToken = Parser.fix (fun r -> choice [ pDo; pDont; pMul; anyChar >>. r ])

    let pMuls = many1 pToken

    let part1 =
        Parser.parse pMuls
        >> Seq.sumBy (fun t ->
            match t with
            | Mul(a, b) -> a * b
            | _ -> 0)

    let part2 =
        Parser.parse pMuls
        >> Seq.fold
            (fun (s, p) t ->
                match t with
                | Mul(a, b) when p -> s + a * b, p
                | Do -> s, true
                | Dont -> s, false
                | _ -> s, p)
            (0, true)
        >> fst
