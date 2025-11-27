namespace Aoc.Tests

module ``Parser`` =
    open Aoc
    open FsCheck.Xunit
    open System.Collections.Immutable
    open XParsec
    open XParsec.CharParsers
    open XParsec.Parsers
    open Xunit

    [<Fact>]
    let ``sepByEndBy works with a trailing seperator`` () =
        let result = "1;2;3;" |> Parser.parse (Parser.sepEndBy pint32 (pstring ";") .>> eof)

        Assert.Equal([| 1; 2; 3 |], result)

    [<Property>]
    let ``sepByEndBy parses arbitrary sized collections`` (list: ImmutableArray<int>) =
        let input = list |> Seq.map string |> String.concat ";"

        let result = input |> Parser.parse (Parser.sepEndBy pint32 (pstring ";") .>> eof)

        list = result
