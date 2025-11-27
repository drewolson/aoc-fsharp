namespace Year2025.Tests

module ``day 01`` =
    open Xunit
    open Year2025

    [<Fact>]
    let ``part 1`` () = Assert.Equal(1, Day01.part1 "foo")

    [<Fact>]
    let ``part 2`` () = Assert.Equal(2, Day01.part2 "foo")
