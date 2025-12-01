namespace Aoc

module Parser =
    open XParsec

    let private extractFirst c p s =
        parser {
            let! struct (values, _) = c p s
            return values
        }

    let sepEndBy1' p s = extractFirst sepEndBy1 p s

    let sepEndBy' p s = extractFirst sepEndBy p s

    let sepBy1' p s = extractFirst sepBy1 p s

    let sepBy' p s = extractFirst sepBy p s

    let parse p str =
        match Reader.ofString str () |> p with
        | Error err -> failwithf "%A" err
        | Ok success -> success.Parsed

    let delay p = parser { return! p () }

    let rec fix f = parser { return! f (fix f) }
