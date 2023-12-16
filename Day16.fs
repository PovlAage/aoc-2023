module aoc_2023.Day16
open Utility
open ArrayAoc2D
open System.Collections.Generic
let day = 16

type Tile =
    | Empty = '.'
    | MirrorSlash = '/'
    | MirrorBackslash = '\\'
    | SplitterHorizontal = '-'
    | SplitterVertical = '|'

type State = Set<Point * Direction>

let parse = lines2array >> Array2D.map charEnum

let rec affectedRec arr (acc:Set<Point>) (Point (x, y)) d =
    let dx, dy = delta d
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
        | _ -> affectedRec arr acc pnext d

let affected arr p d =
    affectedRec arr Set.empty p d

[<TailCall>]
let rec loop (cache:Dictionary<_, _>) arr (beams:Set<Point>) (prevFrontier:Set<Point*Direction>) (frontier:(Point*Direction) list) =
    match frontier with
    | [] -> beams
    | head :: rest when prevFrontier |> Set.contains head -> loop cache arr beams prevFrontier rest
    | head :: rest ->
        let prevFrontier = prevFrontier |> Set.add head
        let points, split =
            let key = head
            let isCached, value = cache.TryGetValue key
            if isCached then
                value
            else
                let value = head ||> affected arr
                cache.Add(key, value)
                value
        let beams = Set.union beams points
        let newFrontier =
            match split with
            | Some (Point (px, py)) when arr[px, py] = Tile.SplitterHorizontal -> let p = Point (px, py) in [(p, Left); (p, Right)] 
            | Some (Point (px, py)) when arr[px, py] = Tile.SplitterVertical -> [let p = Point (px, py) in (p, Up); (p, Down)]
            | _ -> []
        loop cache arr beams prevFrontier (rest @ newFrontier)

let countEnergized cache arr initial =
    let frontier = [initial]
    let final = loop cache arr Set.empty  Set.empty frontier
    final |> Set.count
    
let resultA arr =
    let cache = Dictionary<_, _>()
    countEnergized cache arr (Point(-1, 0), Right)

let resultB arr =
    let mx, my = Array2D.length1 arr - 1, Array2D.length2 arr - 1
    let edges = [
        [0..mx] |> List.map (fun x -> (Point (x, -1), Direction.Down))
        [0..mx] |> List.map (fun x -> (Point (x, my + 1), Direction.Up))
        [0..my] |> List.map (fun y -> (Point (-1, y), Direction.Right))
        [0..my] |> List.map (fun y -> (Point (mx + 1, y), Direction.Left))
                ] |> List.concat
    let cache = Dictionary<_, _>()
    edges |> List.map (countEnergized cache arr) |> List.max
    
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
    let testInput = stestInput |> multiLineToList |> parse
    if v then
        verify (resultA testInput) 46
    let input = inputLines day |> parse
    verify (resultA input) 6816
    
    if v then
        verify (resultB testInput) 51
    verify (resultB input) 8163
