# Advent of Code 2020 in F\#

Contains template for getting puzzle input and solving problems for [Advent of Code](https://adventofcode.com).

For now, this is focused on solutions for the 2020 event.

## Authentication

To use the utilitize to retreive input and submit answers, the library will look for a `AOC_TOKEN` environment variable with a valid cookie session.

```sh
export AOC_TOKEN=<your token>
```

You can then use `getInput year day` to get your input for a given puzzle.
To submit an answer, use `submit year day level answer`.
