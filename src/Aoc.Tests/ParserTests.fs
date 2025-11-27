namespace Aoc.Tests

module ParserTests =
    open Xunit
    open FsCheck.Xunit
    open XParsec
    open XParsec.Parsers
    open XParsec.CharParsers
    open System.Collections.Immutable

    [<Fact>]
    let ``sepByEndBy works with a trailing seperator`` () =
        let result =
            "1;2;3;" |> Aoc.Parser.parse (Aoc.Parser.sepEndBy pint32 (pstring ";") .>> eof)

        Assert.Equal([| 1; 2; 3 |], result)

    [<Property>]
    let ``sepByEndBy parses arbitrary sized collections`` (list: ImmutableArray<int>) =
        let input = list |> Seq.map string |> String.concat ";"

        let result =
            input |> Aoc.Parser.parse (Aoc.Parser.sepEndBy pint32 (pstring ";") .>> eof)

        list = result
