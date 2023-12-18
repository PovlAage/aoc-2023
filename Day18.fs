module aoc_2023.Day18
open Utility
open ArrayAoc2D
let day = 18
type Color = Color of string
type Line = Line of Direction * int
let reLine = System.Text.RegularExpressions.Regex(@"^(?<direction>[UDRL]) (?<distance>\d+) \(#(?<color>[0-9a-f]{6})\)$")
let parseLineA line =
    let m = reLine.Match(line)
    assert m.Success
    let v (name:string) =
        m.Groups[name].Value
    let direction = match v "direction" with
                    | "U" -> Direction.Up
                    | "D" -> Direction.Down
                    | "L" -> Direction.Left
                    | "R" -> Direction.Right
                    | d -> failwithf $"Unmatched {d}"
    let distance = int (v "distance")
    Line (direction, distance)

let parseLineB line =
    let m = reLine.Match(line)
    assert m.Success
    let v (name:string) =
        m.Groups[name].Value
    let color = v "color"
    let distance = parseHex (color.Substring(0, 5))
    let direction = match color[5] with
                    | '0' -> Direction.Right
                    | '1' -> Direction.Down
                    | '2' -> Direction.Left
                    | '3' -> Direction.Up
                    | d -> failwithf $"Unmatched {d}"
    Line (direction, distance)

let parseA = List.map parseLineA
let parseB = List.map parseLineB

let move point direction distance =
    let delta = delta direction
    [1..distance] |> List.map (addTimes point delta)
let traverse lines p =
    let folder points (line:Line) =
         let (Line (direction, distance)) = line
         (move (List.last points) direction distance)
    lines |> List.scan folder [p]
let dig lines =
    let points = traverse lines (Point (0, 0)) |> List.concat
    let xs = points |> List.map px
    let ys = points |> List.map py
    let xbase, ybase = List.min xs - 1, List.min ys - 1
    let xlen, ylen = List.max xs - xbase + 3, List.max ys - ybase + 3
    let arr = Array2D.createBased xbase ybase xlen ylen 0
    for (Point (x1, y1), Point (x2, y2)) in points |> List.pairwise do
        arr[x2, y2] <- 1
        let xl, yl = (x2 - (y2 - y1), y2 + (x2 - x1))
        let xr, yr = (x2 + (y2 - y1), y2 - (x2 - x1))
        if arr[xl, yl] = 0 then arr[xl, yl] <- 2
        if arr[xr, yr] = 0 then arr[xr, yr] <- 3
    floodFill arr 0 [2; 3]
    let inner = if arr[xbase, ybase] = 2 then 3 else 2
    arr |> Array2D.iteri (fun x y c -> if c = inner then arr[x, y] <- 1)
    arr
    // flood fill, first paint left/right edge
    
let result input =
    let arr = dig input
    let mutable count = 0
    arr |> Array2D.iter (fun c -> if c = 1 then count <- count + 1)
    count

let run v =
    use _ = measureElapsed day
    
    let stestInput = """
R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
"""
    // for l in testInput do
    //     printfn $"{l}"
    // for p in traverse testInput (Point (0, 0)) do
    //     printfn $"{p}"
    // let arr = dig testInput
    // dump arr
    if v then
        let testInput = stestInput |> multiLineToList |> parseA
        verify (result testInput) 62
    let input = inputLines day |> parseA
    verify (result input) 62573

    // if v then
    //     let testInput = stestInput |> multiLineToList |> parseB
    //     for l in testInput do
    //         printfn $"{l}"
    //     verify (result testInput) 0
    