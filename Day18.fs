module aoc_2023.Day18
open Utility
open ArrayAoc2D
let day = 18
type Color = Color of string
type Line = Line of Direction * int
type DigPlanSteps = DigPlanSteps of (int list) * (int list)
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
    addTimes point (delta direction) distance
let getTurningPoints lines p =
    let folder p (line:Line) =
         let (Line (direction, distance)) = line
         move p direction distance
    lines |> List.scan folder p
let dig lines =
    let turningPoints = getTurningPoints lines (Point (0, 0))
    let coordStats p =
        let coords = turningPoints |> List.map p
        let withOffset = List.append coords (coords |> List.map ((+) 1))
        let distinctAndSorted = withOffset |> List.distinct |> List.sort
        let withPadding = (List.head distinctAndSorted - 1) :: distinctAndSorted @ [List.last distinctAndSorted + 1]
        let steps = withPadding |> List.pairwise |> List.map (fun (x1, x2) -> x2 - x1)
        withPadding, steps
    let xs, xsteps = coordStats px
    let ys, ysteps = coordStats py
    let arr = Array2D.create xs.Length ys.Length 0L
    let trenchPointsFolder points (line:Line) =
        let (Line (direction, distance)) = line
        let (Point (ix, iy)) = List.last points
        let dx, dy = delta direction
        let rec loop acc (ix, iy) remainingDistance =
            assert (remainingDistance >= 0)
            if remainingDistance = 0 then
                acc
            else
                let ix = ix + dx
                let iy = iy + dy
                assert (dx = 0 || dy = 0)
                let stepDistance = if dx <> 0 then xsteps[ix] else ysteps[iy]
                loop (Point(ix, iy) :: acc) (ix, iy) (remainingDistance - stepDistance)
        List.rev (loop [] (ix, iy) distance)        
    let ix0, iy0 = xs |> List.findIndex ((=) 0), ys |> List.findIndex ((=) 0)        
    let trenchPoints = lines |> List.scan trenchPointsFolder [Point(ix0, iy0)] |> List.concat
        
    for Point (x1, y1), Point (x2, y2) in trenchPoints |> List.pairwise do
        arr[x2, y2] <- 1
        let xl, yl = (x2 - (y2 - y1), y2 + (x2 - x1))
        let xr, yr = (x2 + (y2 - y1), y2 - (x2 - x1))
        if arr[xl, yl] = 0 then arr[xl, yl] <- 2
        if arr[xr, yr] = 0 then arr[xr, yr] <- 3
    floodFill arr 0 2
    floodFill arr 0 3
    let inner = if arr[0, 0] = 2 then 3 else 2
    arr |> Array2D.iteri (fun x y c -> arr[x, y] <- if c = inner || c = 1 then (int64 xsteps[x]) * (int64 ysteps[y]) else 0L)
    arr
    
let result input =
    let arr = dig input
    let mutable count = 0L
    arr |> Array2D.iter (fun c -> count <- count + c)
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
    if v then
        let testInput = stestInput |> multiLineToList |> parseA
        verify (result testInput) 62
    let input = inputLines day |> parseA
    verify (result input) 62573

    if v then
        let testInput = stestInput |> multiLineToList |> parseB
        verify (result testInput) 952408144115L
    let input = inputLines day |> parseB
    verify (result input) 54662804037719L
