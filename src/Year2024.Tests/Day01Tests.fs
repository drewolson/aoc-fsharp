namespace Year2024.Tests

module Day01Tests =
    open Xunit
    open Year2024

    let input =
        """3   4
4   3
2   5
1   3
3   9
3   3
"""

    [<Fact>]
    let ``part 1`` () = Assert.Equal(11, Day01.part1 input)

    [<Fact>]
    let ``part 2`` () = Assert.Equal(31, Day01.part2 input)
