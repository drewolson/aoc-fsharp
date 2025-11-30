namespace Year2024.Tests

module ``day 03`` =
    open Xunit
    open Year2024

    let input =
        """xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"""

    let input2 =
        """xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"""

    [<Fact>]
    let ``part 1`` () = Assert.Equal(161, Day03.part1 input)

    [<Fact>]
    let ``part 2`` () = Assert.Equal(48, Day03.part2 input2)
