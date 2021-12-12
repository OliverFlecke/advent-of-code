namespace AdventOfCode

type public ISolution =
    abstract year : int
    abstract day : int

    abstract solveA : string -> string

    abstract solveB : string -> string
