module Tests

open Xunit
open AdventOfCode

[<Fact>]
let ``Get url`` () =
    let url = Utils.url 2020 1
    Assert.Equal(url, "https://adventofcode.com/2020/day/1/input")

[<Fact>]
let ``Get input for day 1 2019`` () =
    Assert.NotNull(Utils.getInput "../../../../../.token" 2019 1)