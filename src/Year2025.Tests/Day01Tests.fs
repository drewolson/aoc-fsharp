namespace Year2025.Tests

module ``day 01`` =
    open Xunit
    open Year2025

    let input =
        """L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
"""

    [<Fact>]
    let ``part 1`` () = Assert.Equal(3, Day01.part1 input)

    [<Fact>]
    let ``part 2`` () = Assert.Equal(6, Day01.part2 input)
