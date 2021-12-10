module Tests

open Xunit
open AdventOfCode

[<Fact>]
let ``Get url`` () =
    let url = Core.inputUrl 2020 1
    Assert.Equal("https://adventofcode.com/2020/day/1/input", url)

[<Fact>]
let ``Get input for day 1 2019`` () =
    Assert.NotNull(Core.input 2019 1)
