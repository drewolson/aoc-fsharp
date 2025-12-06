namespace Year2025.Tests

module ``day 06`` =
    open Xunit
    open Year2025

    let input =
        """123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
"""

    [<Fact>]
    let ``part 1`` () =
        Assert.Equal(4277556L, Day06.part1 input)

    [<Fact>]
    let ``part 2`` () =
        Assert.Equal(3263827L, Day06.part2 input)
