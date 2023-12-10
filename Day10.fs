module aoc_2023.Day10
open Utility
let day = 10
type Direction =
    | North = 1
    | South = 2
    | East = 4
    | West = 8
type Tile =
    | Vertical = '|'
    | Horizontal = '-'
    | NorthEast = 'L'
    | NorthWest = 'J'
    | SouthWest = '7'
    | SouthEast = 'F'
    | Ground = '.'
    | Starting = 'S'
type Point = Point of int * int
let add (Point (x1, y1)) (Point (x2, y2)) = Point (x1 + x2, y1 + y2)
let parseTile = function
    | '|' -> Tile.Vertical
    | '-' -> Tile.Horizontal
    | 'L' -> Tile.NorthEast
    | 'J' -> Tile.NorthWest
    | '7' -> Tile.SouthWest
    | 'F' -> Tile.SouthEast
    | '.' -> Tile.Ground
    | 'S' -> Tile.Starting
    | c -> failwithf $"Unmatched {c}"
let directions = function
    | Tile.Vertical -> Direction.North ||| Direction.South
    | Tile.Horizontal -> Direction.East ||| Direction.West
    | Tile.NorthEast -> Direction.North ||| Direction.East
    | Tile.NorthWest -> Direction.North ||| Direction.West
    | Tile.SouthEast -> Direction.South ||| Direction.East
    | Tile.SouthWest -> Direction.South ||| Direction.West
    | Tile.Ground -> enum<Direction>(0)
    | t -> failwithf $"Unmatched {t}"
let opposite = function
    | Direction.North -> Direction.South
    | Direction.South -> Direction.North
    | Direction.East -> Direction.West
    | Direction.West -> Direction.East
    | d -> failwithf $"Unmatched {d}"
let parse (lines:string list) =
    let X, Y = (lines[0].Length + 2), (lines.Length + 2)
    let arr = Array2D.create X Y '.'
    let mutable (sx, sy) = (-1, -1)
    for y in 0..(Y-3) do
        for x in 0..(X-3) do
            arr[x+1, y+1] <- lines[y][x]
            if parseTile (lines[y][x]) = Tile.Starting then
                sx <- x+1
                sy <- y+1
    Point (sx, sy), arr

let dump arr =
    let X, Y = Array2D.length1 arr, Array2D.length2 arr
    for y in 0..(Y-1) do
        for x in 0..(X-1) do
            printf $"{arr[x, y]}"
        printfn ""

let move (Point (x, y)) d =
    match d with
    | Direction.North -> Point (x, y-1)
    | Direction.South -> Point (x, y+1)
    | Direction.East -> Point (x+1, y)
    | Direction.West -> Point (x-1, y)
    | _ -> failwithf $"Unmatched {d}"

let readTile (arr:char[,]) (Point (x, y)) = parseTile arr[x, y]

[<TailCall>]
let rec walkPipe (arr:char[,]) acc (pos, prevDirection) =
    let currentTile = readTile arr pos
    if currentTile = Tile.Starting then
        acc
    else
        let nextDirection = directions currentTile ^^^ (opposite prevDirection)
        let nextPos = move pos nextDirection
        walkPipe arr ((pos, prevDirection) :: acc) (nextPos, nextDirection)

let initial s arr =
    Direction.GetValues() |>
    Seq.map (fun d -> (d, move s d)) |>
    Seq.map (fun (d, p) -> let t = readTile arr p in (d, p, t) ) |>
    Seq.find (fun (d, p, t) -> 0 <> int(directions t &&& (opposite d)))
let resultA (s, arr) =
    let (d, p, t) = initial s arr
    (walkPipe arr [(s, d)] (p, d)).Length / 2
let resultB (s, arr) =
    let (d, p, t) = initial s arr
    let pipe = walkPipe arr [(s, d)] (p, d)
    let X, Y = (Array2D.length1 arr), (Array2D.length2 arr)
    let arrPaint = Array2D.create X Y '.'
    let mutable countV = 0
    let mutable countH = 0
    // all painting done with this helper, to get correct count
    let paint (Point (x, y)) c =
        if arrPaint[x, y] = '.' then
            arrPaint[x, y] <- c
            if c = 'V' then countV <- countV + 1
            if c = 'H' then countH <- countH + 1
    
    // paint pipe
    for p, _ in pipe do
        let (Point (x, y)) = p
        paint p arr[x, y]

    // paint on left and right side of pipe
    for p, d in pipe do
        let leftPoints, rightPoints =
            match d with
            | Direction.East -> [(-1, -1); (0, -1); (1, -1)], [(-1, 1); (0, 1); (1, 1)] 
            | Direction.West -> [(-1, 1); (0, 1); (1, 1)], [(-1, -1); (0, -1); (1, -1)]
            | Direction.North -> [(-1, -1); (-1, 0); (-1, 1)], [(1, -1); (1, 0); (1, 1)]
            | Direction.South -> [(1, -1); (1, 0); (1, 1)], [(-1, -1); (-1, 0); (-1, 1)]
            | _ -> failwithf $"Unmatched {d}"
        for px, py in leftPoints do paint (add p (Point (px, py))) 'V'
        for px, py in rightPoints do paint (add p (Point (px, py))) 'H'

    // flood fill from V/H frontier while progress is made
    let neighbours = [Point (-1, 0); Point(1, 0); Point(0, -1); Point(0, 1)]
    let mutable progress = true
    while progress do
        let flood x y c =
            if c = 'V' || c = 'H' then
                let clearNeighbours =
                    neighbours |>
                    List.map (add (Point (x, y))) |>
                    List.filter (fun (Point (xx, yy)) -> xx >= 0 && yy >= 0 && xx < X && yy < Y && arrPaint[xx, yy] = '.')
                for pn in clearNeighbours do paint pn c
                paint (Point (x, y)) (if c = 'V' then 'v' else 'h') // optimization: avoid revisiting these
                if not clearNeighbours.IsEmpty then progress <- true 
        progress <- false
        arrPaint |> Array2D.iteri flood

    // top left corner is outer, so inner is its complement
    if arrPaint[0, 0].ToString().ToLower() = "h" then countV else countH

let run v =
    use _ = measureElapsed day
    
    if v then
        let stestInput = """
.....
.S-7.
.|.|.
.L-J.
.....
"""
        let testInput = parse (stestInput |> multiLineToList)
        verifyq (resultA testInput) 4

    let input = parse (inputLines day)
    verify (resultA input) 6690
    
    if v then
        let stestInput = """
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
"""
        let testInput = parse (stestInput |> multiLineToList)
        verifyq (resultB testInput) 4

    verify (resultB input) 525
