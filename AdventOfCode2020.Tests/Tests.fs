module Tests

open Xunit
open AdventOfCode2020.Say

[<Fact>]
let ``My test`` () =
    Assert.True(true)

[<Fact>]
let ``First test`` () =
    Assert.True(hello "john")