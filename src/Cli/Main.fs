namespace Cli

module Main =
    open System.IO
    open Argu

    type Options =
        | [<AltCommandLine("-y")>] Year of int
        | [<Mandatory; AltCommandLine("-d")>] Day of int
        | [<Mandatory; AltCommandLine("-p")>] Part of int

        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Year _ -> "year (default: 2025)"
                | Day _ -> "day"
                | Part _ -> "part (1 or 2)"

    type Args = { Year: int; Day: int; Part: int }

    let run input args =
        match args with
        | { Year = 2024; Day = 1; Part = 1 } -> Year2024.Day01.part1 input |> printfn "%A"
        | { Year = 2024; Day = 1; Part = 2 } -> Year2024.Day01.part2 input |> printfn "%A"
        | { Year = 2024; Day = 2; Part = 1 } -> Year2024.Day02.part1 input |> printfn "%A"
        | { Year = 2024; Day = 2; Part = 2 } -> Year2024.Day02.part2 input |> printfn "%A"
        | { Year = 2025; Day = 1; Part = 1 } -> Year2025.Day01.part1 input |> printfn "%A"
        | { Year = year; Day = day; Part = part } -> failwithf "Unknown year, day, and part: %i, %i, %i" year day part


    [<EntryPoint>]
    let main argv =
        let parser = ArgumentParser.Create(programName = "aoc")

        try
            let results = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)

            let args =
                { Year = results.GetResult(Year, defaultValue = 2025)
                  Day = results.GetResult Day
                  Part = results.GetResult Part }

            let path = sprintf "data/%i/day%02i.txt" args.Year args.Day

            let input = File.ReadAllText path

            run input args

            0
        with ex ->
            eprintfn "%s" ex.Message

            1
