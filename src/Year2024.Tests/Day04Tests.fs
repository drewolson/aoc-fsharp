namespace Year2024.Tests

module ``day 04`` =
    open Xunit
    open Year2024

    let input =
        """MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
"""

    [<Fact>]
    let ``part 1`` () = Assert.Equal(18, Day04.part1 input)

    [<Fact>]
    let ``part 2`` () = Assert.Equal(9, Day04.part2 input)
