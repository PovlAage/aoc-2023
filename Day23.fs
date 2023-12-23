module aoc_2023.Day23
open Utility
open ArrayAoc2D
let day = 23
type Arr = char array2d
let parse lines =
    let arr = lines2array lines
    let dims = dims2 arr
    let arrPadded = Array2D.create (dims.XLength+2) (dims.YLength+2) '#'
    Array2D.blit arr 0 0 arrPadded 1 1 dims.XLength dims.YLength
    arrPadded

[<TailCall>]
let rec longestPath (map:char[,]) allowUpslope (goal:Point) (visitedForks:Set<Point>) (count:int) (prev:Point option) (current:Point) =
    if current = goal then
        count
    else
        let isValidStep p =
            let (Point (x, y)) = p
            map[x, y] <> '#' && Some p <> prev && (not (visitedForks |> Set.contains p))
        let validDirections =
            if allowUpslope then
                allDirections
            else
                let (Point (x, y)) = current
                match map[x, y] with
                | 'v' -> [Down]
                | '<' -> [Left]
                | '>' -> [Right]
                | '^' -> [Up]
                | _ -> allDirections
        let neighbours = validDirections |> List.map delta |> List.map (fun d -> add current d) |> List.filter isValidStep
        match neighbours with
        | [] -> -1 // blind path
        | [next] -> longestPath map allowUpslope goal visitedForks (1 + count) (Some current) next
        | ns ->
            let visitedForks = visitedForks |> Set.add current
            ns |> Seq.map (longestPath map allowUpslope goal visitedForks (1 + count) (Some current)) |> Seq.max

let result allowUpslope arr =       
    let initial = Point(2, 1)
    let dims = dims2 arr
    let goal = Point(dims.XLength-3, dims.YLength-2)
    longestPath arr allowUpslope goal Set.empty 0 None initial
let resultA = result false
let resultB = result true
        
let run v =
    use _ = measureElapsed day
    
    let stestInput = """
#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#
"""
    let testInput = stestInput |> multiLineToList |> parse
    if v then
        verify (resultA testInput) 94
    let input = inputLines day |> parse
    verify (resultA input) 2294

    if v then
        verify (resultB testInput) 154
    // verify (resultB input) 0
