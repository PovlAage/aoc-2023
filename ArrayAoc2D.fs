module ArrayAoc2D
open Checked

[<Struct>]
type Point = Point of int*int
type Dims =
    { XBase: int; XMax: int; YBase: int; YMax: int }
    member this.IsInbounds (Point (x, y)) =
        this.XBase <= x && x <= this.XMax && this.YBase <= y && y <= this.YMax

type Direction =
    | Up
    | Down
    | Left
    | Right
let allDirections = [Up; Down; Left; Right]
let delta = function
    | Up -> (0, -1)
    | Down -> (0, 1)
    | Left -> (-1, 0)
    | Right -> (1, 0)
let add (Point (x, y)) (dx, dy) = Point (x + dx, y + dy)
let addTimes (Point (x, y)) (dx, dy) c = Point (x + c * dx, y + c * dy)
let px (Point (x, y)) = x
let py (Point (x, y)) = y
let dims arr =
    Array2D.length1 arr, Array2D.length2 arr

let dims2 arr =
    let basex, basey = Array2D.base1 arr, Array2D.base2 arr
    let maxx, maxy = basex + Array2D.length1 arr - 1, basey + Array2D.length2 arr - 1
    { XBase = basex; YBase = basey; XMax = maxx; YMax = maxy }

let dump arr =
    let dims2 = dims2 arr
    
    for y in dims2.YBase..dims2.YMax do
        for x in dims2.XBase..dims2.XMax do
            printf $"{arr[x, y]}"
        printfn ""

let lines2array (lines:string list) =
    let lx, ly = lines[0].Length, lines.Length
    Array2D.init lx ly (fun x y -> lines[y][x])

let isInBounds arr p =
    let dims = dims2 arr
    dims.IsInbounds(p)

let floodFill arr blank colour =
    // flood fill from sources while progress is made
    let dims = dims2 arr
    let neighbours = allDirections |> List.map delta
    let rec loop frontier =        
        let blankNeighbours p =
            neighbours |>
            List.map (add p) |>
            List.filter (fun (Point (xx, yy)) -> dims.IsInbounds(Point(xx, yy)) && arr[xx, yy] = blank)
        let newFrontier = frontier |> List.collect blankNeighbours |> List.distinct
        if not (List.isEmpty newFrontier) then
            for (Point (xx, yy)) in newFrontier do arr[xx, yy] <- colour
            loop newFrontier
    let initialFrontier = [for x in dims.XBase..dims.XMax do
                           for y in dims.YBase..dims.YMax do
                           let p = Point (x, y)
                           if arr[x, y] = colour then yield p]
    loop initialFrontier