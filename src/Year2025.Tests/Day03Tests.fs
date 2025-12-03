namespace Year2025.Tests

module ``day 03`` =
    open Xunit
    open Year2025

    let input =
        """987654321111111
811111111111119
234234234234278
818181911112111
"""

    [<Fact>]
    let ``part 1`` () = Assert.Equal(357L, Day03.part1 input)

    [<Fact>]
    let ``part 2`` () =
        Assert.Equal(3121910778619L, Day03.part2 input)
