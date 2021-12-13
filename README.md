# Advent of Code in F\#

Contains template for getting puzzle input and solving problems for [Advent of Code](https://adventofcode.com).

## Library usage

Use `input year day` to get your input for a given puzzle.

To submit an answer, use `submit year day level answer`.

## Structure

Solutions are stored in the `AOC` directory at `<year>/<day>.fs`.

To run (and build first), use `dotnet run --project src/AOC` from the root of this project.
This will run all solutions that have been implemented.
To only run solutions for a single year or day, use `--year` and `--day` arguments to specify which solution to run, e.g.

```sh
dotnet run --project src/AOC -- --year 2021 --day 1
```

After a solution have been implemented, it can be submitted with the `--submit-a` and `--sumbit-b` flags.

Input for these challenges are unique to each user, and to avoid putting unnecessary load to the `adventofcode` servers, each input file is cached.
These are stored under `input/<year>/<day>.txt`.

### Note for older solutions

Older solutions have been implemented as F# scripts, and are stored at the root under `<year>/<day>.fsx`.
They can be executed with

To run a given solution use `dotnet fsi <year>/<day>.fsx`, e.g.

```sh
dotnet fsi 2020/01.fsx
```

### Setup in script solutions

Before you can run the solution, you will have to build the library dll.

```sh
dotnet build -c Release AdventOfCode.sln
```

The library then needs to be referenced in the `fsx` solution files when used.
This would generally look like:

```fsharp
#r "../src/AdventOfCode/bin/Release/net6.0/AdventOfCode.dll"
```

### Authentication

To use the helper functions to retreive input and submit answers, the library will look for a `AOC_TOKEN` environment variable with a valid cookie session.

1. Go to adventofcode.com and login
2. Open the developer settings in your browser (F12)
3. Go to `application` -> `Cookies`.
4. You should see a `session` variable - this is the token we need.
5. Add this to your environment with `export AOC_TOKEN=<your token>`

(I usually put it in a `.token` file to easily load the variable in later sessions)

## Solution template

The following is a template for how to implement the `ISolution` interface:

```fsharp
namespace AdventOfCode.Solutions

open AdventOfCode
open AdventOfCode.Utils

type Year2021Day1() =
    interface ISolution with
        member _.year = 2021
        member _.day = 1

        member _.testA = Seq.empty
        member _.testB = Seq.empty

        member self.solveA input = ""

        member self.solveB input = ""
```
