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