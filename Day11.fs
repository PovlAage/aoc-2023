module aoc_2023.Day11
open Utility
let day = 11

[<Struct>]
type Point = Point of (int64*int64)
type Galaxies = (int * Point) list
let px (Point (x, y)) = x
let py (Point (x, y)) = y
let parse (lines:string list) =
    let X, Y = lines[0].Length, lines.Length
    [for y in 0..(Y-1) do for x in 0..(X-1) do if lines[y][x] = '#' then yield Point (x, y)]
let expand galaxies factor =
    let maxx = galaxies |> List.map px |> List.max |> int64
    let maxy = galaxies |> List.map py |> List.max |> int64
    let emptyXs = Set.difference (Set.ofList [0L..maxx]) (galaxies |> List.map px |> Set.ofList)
    let emptyYs = Set.difference (Set.ofList [0L..maxy]) (galaxies |> List.map py |> Set.ofList)
    let expandedPosition (Point (x, y)) =
        let countSmaller z0 = Set.filter (fun z -> z < z0) >> Set.count >> int64
        Point (x + (int64 factor - 1L) * (countSmaller x emptyXs), y + (int64 factor - 1L) * (countSmaller y emptyYs))
    galaxies |> List.map expandedPosition    
    
let dist (Point (x1, y1)) (Point (x2, y2)) = abs (x2 - x1) + abs (y2 - y1)

let result factor (galaxies:Point list) =
    let expanded = expand galaxies factor |> List.indexed
    List.allPairs expanded expanded |>
    List.filter (fun (g1, g2) -> fst g1 < fst g2) |>
    List.sumBy (fun (g1, g2) -> dist (snd g1) (snd g2))
let resultA = result 2
let resultB = result 1000000

let run v =
    use _ = measureElapsed day
    
    let stestInput = """
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
"""
    if v then
        let testInput = stestInput |> multiLineToList |> parse
        verifyq (resultA testInput) 374
    let input = (inputLines day) |> parse
    verify (resultA input) 9723824
    
    verify (resultB input) 731244261352L
