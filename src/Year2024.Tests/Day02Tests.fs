namespace Year2024.Tests

module ``day 02`` =
    open Xunit
    open Year2024

    let input =
        """7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
"""

    [<Fact>]
    let ``part 1`` () = Assert.Equal(2, Day02.part1 input)

    [<Fact>]
    let ``part 2`` () = Assert.Equal(4, Day02.part2 input)
