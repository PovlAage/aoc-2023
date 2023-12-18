module aoc_2023.Day10
open Utility
open ArrayAoc2D
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

    let paint (Point (x, y)) c =
        if arrPaint[x, y] = '.' then
            arrPaint[x, y] <- c
    
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
        for l in leftPoints do paint (add p l) 'V'
        for r in rightPoints do paint (add p r) 'H'

    floodFill arrPaint '.' 'V'
    floodFill arrPaint '.' 'H'

    // top left corner is outer, so inner is its complement
    let mutable countV = 0
    let mutable countH = 0
    arrPaint |> Array2D.iteri (fun x y c -> if c = 'V' then countV <- countV + 1 elif c = 'H' then countH <- countH + 1)
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
