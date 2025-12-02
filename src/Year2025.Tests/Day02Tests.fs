namespace Year2025.Tests

module ``day 02`` =
    open Xunit
    open Year2025

    let input =
        """11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124
"""

    [<Fact>]
    let ``part 1`` () =
        Assert.Equal(1227775554I, Day02.part1 input)

    [<Fact>]
    let ``part 2`` () =
        Assert.Equal(4174379265I, Day02.part2 input)
