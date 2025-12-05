namespace Year2025.Tests

module ``day 05`` =
    open Xunit
    open Year2025

    let input =
        """3-5
10-14
16-20
12-18

1
5
8
11
17
32
"""

    [<Fact>]
    let ``part 1`` () = Assert.Equal(3, Day05.part1 input)

    [<Fact>]
    let ``part 2`` () = Assert.Equal(14L, Day05.part2 input)
