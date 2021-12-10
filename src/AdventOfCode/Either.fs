namespace AdventOfCode

module Types =
    type Either<'a, 'b> =
        | Left of 'a
        | Right of 'b

        member x.isLeft =
            match x with
            | Left _ -> true
            | _ -> false

        member x.isRight =
            match x with
            | Right _ -> true
            | _ -> false

        member x.rightValue =
            match x with
            | Right value -> value
            | _ -> invalidArg "either" "A 'Right' either was not provided"

        member x.leftValue =
            match x with
            | Left value -> value
            | _ -> invalidArg "either" "A 'Left' either was not provided"


    module Either =
        let inline isLeft (either: Either<'a, 'b>) = either.isLeft

        let inline isRight (either: Either<'a, 'b>) = either.isRight

        let getLeft (either: Either<'a, 'b>) = either.leftValue

        let getRight (either: Either<'a, 'b>) = either.rightValue

        let map l r either =
            match either with
            | Left x -> Left <| l x
            | Right x -> Right <| r x
