module EitherTests

open Xunit
open AdventOfCode.Types

[<Fact>]
let ``right value member`` () =
    let either = Right 5
    Assert.Equal(5, either.rightValue)

[<Fact>]
let ``static get right`` () =
    let either = Right 5
    Assert.Equal(5, Either.getRight either)

[<Fact>]
let ``left value member`` () =
    let either = Left "Hello"
    Assert.Equal("Hello", either.leftValue)

[<Fact>]
let ``static get left`` () =
    let either = Left "Hello"
    Assert.Equal("Hello", Either.getLeft either)

[<Fact>]
let ``map left`` () =
    let either = Left 8
    Assert.Equal(Left 16, Either.map ((*) 2) id either)

[<Fact>]
let ``map right`` () =
    let either = Right "world"
    Assert.Equal(Right "Hello world", Either.map id ((+) "Hello ") either)