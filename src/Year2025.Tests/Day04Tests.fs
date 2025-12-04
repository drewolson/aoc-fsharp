namespace Year2025.Tests

module ``day 04`` =
    open Xunit
    open Year2025

    let input =
        """..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
"""

    [<Fact>]
    let ``part 1`` () = Assert.Equal(13, Day04.part1 input)

    [<Fact>]
    let ``part 2`` () = Assert.Equal(43, Day04.part2 input)
