namespace Year2025

module Day03 =
    let joltage n batteries =
        let l = Array.length batteries

        let rec aux i n digits =
            if n = 0 then
                digits |> Array.map string |> String.concat "" |> int64
            else
                let slice = batteries[i + 1 .. l - n]
                let m = Array.max slice
                let i' = Array.findIndex (fun n -> n = m) slice
                aux (i' + i + 1) (n - 1) (Array.append digits [| m |])

        aux -1 n [||]

    let solve n (input: string) =
        input.TrimEnd().Split [| '\n' |]
        |> Array.map (Array.ofSeq >> Array.map string >> Array.map int64)
        |> Array.sumBy (joltage n)

    let part1 = solve 2

    let part2 = solve 12
