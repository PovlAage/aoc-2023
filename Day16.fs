module aoc_2023.Day16
open System.Collections.Generic
open Utility
open aoc_2023.ArrayAoc2D
let day = 16

type Direction =
    | Up
    | Down
    | Left
    | Right

type Tile =
    | Empty = '.'
    | MirrorSlash = '/'
    | MirrorBackslash = '\\'
    | SplitterHorizontal = '-'
    | SplitterVertical = '|'

type State = Set<Point * Direction>

let dp = function
    | Up -> (0, -1)
    | Down -> (0, 1)
    | Left -> (-1, 0)
    | Right -> (1, 0)

let rec affectedRec arr (acc:Set<Point>) (Point (x, y)) d =
    let dx, dy = dp d
    let pnext = Point (x + dx, y + dy)
    if not (inBounds arr pnext) then
        acc, None
    else
        let acc = acc |> Set.add pnext
        let (Point (px, py)) = pnext
        match arr[px, py], d with
        | Tile.SplitterVertical, Direction.Left -> (acc, Some pnext)
        | Tile.SplitterVertical, Direction.Right -> (acc, Some pnext)
        | Tile.SplitterHorizontal, Direction.Up -> (acc, Some pnext)
        | Tile.SplitterHorizontal, Direction.Down -> (acc, Some pnext)
        | Tile.MirrorBackslash, Right -> affectedRec arr acc pnext Down 
        | Tile.MirrorBackslash, Up -> affectedRec arr acc pnext Left 
        | Tile.MirrorBackslash, Left -> affectedRec arr acc pnext Up 
        | Tile.MirrorBackslash, Down -> affectedRec arr acc pnext Right
        | Tile.MirrorSlash, Right -> affectedRec arr acc pnext Up
        | Tile.MirrorSlash, Up -> affectedRec arr acc pnext Right
        | Tile.MirrorSlash, Left -> affectedRec arr acc pnext Down
        | Tile.MirrorSlash, Down -> affectedRec arr acc pnext Left
        | z -> failwithf $"Unmatched {z}"

let affected arr p d = affectedRec arr Set.empty p d


[<TailCall>]
let rec loop (arr:char[,]) (beams:State) (frontier:State) =
    if Set.isEmpty frontier then
        beams
    else
        let beams = Set.union beams frontier
        let nextFrontier = seq {
            for p, d in frontier do
                let (Point (px, py)) = p
                let ds = match charEnum(arr[px, py]), d with
                            | Tile.Empty, _ -> [d]
                            | Tile.MirrorBackslash, Right -> [Down]
                            | Tile.MirrorBackslash, Up -> [Left]
                            | Tile.MirrorBackslash, Left -> [Up]
                            | Tile.MirrorBackslash, Down -> [Right]
                            | Tile.MirrorSlash, Right -> [Up]
                            | Tile.MirrorSlash, Up -> [Right]
                            | Tile.MirrorSlash, Left -> [Down]
                            | Tile.MirrorSlash, Down -> [Left]
                            | Tile.SplitterHorizontal, Right -> [Right]
                            | Tile.SplitterHorizontal, Up -> [Left; Right]
                            | Tile.SplitterHorizontal, Left -> [Left]
                            | Tile.SplitterHorizontal, Down -> [Left; Right]
                            | Tile.SplitterVertical, Right -> [Up; Down]
                            | Tile.SplitterVertical, Up -> [Up]
                            | Tile.SplitterVertical, Left -> [Up; Down]
                            | Tile.SplitterVertical, Down -> [Down]
                            | z -> failwithf $"Unmatched {z}"
                for dd in ds do
                    let dx, dy = dp dd
                    let p2 = Point (px + dx, py + dy)
                    yield (p2, dd)
        }
        let freshFrontier = Set.difference (Set.ofSeq nextFrontier) beams |> Set.filter (fst >> inBounds arr)
        loop arr beams freshFrontier

let countEnergized arr initial =
    let frontier = Set.ofList [initial]
    let final = loop arr Set.empty frontier
    final |> Set.map fst |> Set.count
    
let resultA arr = countEnergized arr (Point(0, 0), Right)

let resultB arr =
    let mx, my = Array2D.length1 arr - 1, Array2D.length2 arr - 1
    let edges = [
        [0..mx] |> List.map (fun x -> (Point (x, 0), Direction.Down))
        [0..mx] |> List.map (fun x -> (Point (x, my), Direction.Up))
        [0..my] |> List.map (fun y -> (Point (0, y), Direction.Right))
        [0..my] |> List.map (fun y -> (Point (mx, y), Direction.Left))
                ] |> List.concat
    edges |> List.map (countEnergized arr) |> List.max
    
let run v =
    use _ = measureElapsed day
    
    let stestInput = """
.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
"""
    let testInput = stestInput |> multiLineToList |> lines2array
    if v then
        verify (resultA testInput) 46
    let input = inputLines day |> lines2array
    verify (resultA input) 6816
    
    if v then
        verify (resultB testInput) 51
    // verify (resultB input) 8163
