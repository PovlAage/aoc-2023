module aoc_2023.Day23
open Utility
open ArrayAoc2D
let day = 23
type Arr = char array2d
type Position = Position of Point * int64
let parse lines =
    let arr = lines2array lines
    let dims = dims2 arr
    let arrPadded = Array2D.create (dims.XLength+2) (dims.YLength+2) '#'
    Array2D.blit arr 0 0 arrPadded 1 1 dims.XLength dims.YLength
    arrPadded

let isRoadB (arr:Arr) p =
    let (Point (x,y)) = p
    arr[x, y] <> '#'

let isFork (arr:Arr) (p:Point) =
    if not (isRoadB arr p) then
        false
    else
        allDirections |> Seq.map delta |> Seq.map (add p) |> Seq.filter (isRoadB arr) |> Seq.length >= 3

[<TailCall>]
let rec longestPath (map:char[,]) (forksByPoint:Map<Point,int64>) allowUpslope (goal:Point) (visitedForks:int64) (count:int) (prev:Point option) (current:Point) =
    if current = goal then
        count
    else
        let isValidStep p =
            let (Point (x, y)) = p
            if map[x, y] = '#' || Some p = prev then
                false
            else
                let forkIndex = forksByPoint |> Map.tryFind p
                match forkIndex with
                | Some i -> visitedForks &&& i = 0
                | None -> true
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
        | [] ->
            -1 // blind path
        | [next] -> longestPath map forksByPoint allowUpslope goal visitedForks (1 + count) (Some current) next
        | ns ->
            let forkIndex = forksByPoint |> Map.find current
            assert (visitedForks &&& forkIndex = 0)
            let visitedForks = visitedForks ||| forkIndex
            ns |> Seq.map (longestPath map forksByPoint allowUpslope goal visitedForks (1 + count) (Some current)) |> Seq.max

let neighbours map forksByPoint prev pos =
    let (Position (p, visitedForks)) = pos
    let chooseNeighbour np =
        if Some np = prev then
            None
        else
            match forksByPoint |> Map.tryFind np with
            | Some i when visitedForks &&& i = 0 -> Some (Position (np, visitedForks ||| i))
            | Some i -> None
            | None -> Some (Position (np, visitedForks))
    map |> Map.find p |> Seq.choose chooseNeighbour |> List.ofSeq
        
[<TailCall>]
let rec longestPath3 (map:Map<Point,Point list>) (forksByPoint:Map<Point,int64>) (goal:Point) (count:int) (prev:Point option) (currentPos:Position) =
    let (Position (currentPoint, visitedForks)) = currentPos
    if currentPoint = goal then
        count
    else
        let neighbours = neighbours map forksByPoint prev currentPos
        match neighbours with
        | [] ->
            -1 // blind path
        | ns ->
            ns |> Seq.map (longestPath3 map forksByPoint goal (1 + count) (Some currentPoint)) |> Seq.max

let dijkstra (map:Map<Point,Point list>) (forksByPoint:Map<Point,int64>) initial =
    let neighbours prev p =
        let vs = neighbours map forksByPoint prev p
        Seq.zip (Seq.initInfinite (fun _ -> -1)) vs
    let queue = System.Collections.Generic.PriorityQueue()
    let dists = System.Collections.Generic.Dictionary()
    let prevs = System.Collections.Generic.Dictionary()
    dists.TryAdd(initial, 0) |> ignore

    let infinity = System.Int32.MaxValue
    // for u in 1..maxu do
    //     for d in allDirections do
    //         arr |> Array2D.iteri (fun x y c -> let pos = Position (Point(x, y), (d, u)) in dists.Add(pos, infinity); prevs.Add(pos, None))
    queue.Enqueue(initial, 0)
    
    let getDist pos =
        let exists, value = dists.TryGetValue pos
        if exists then
            value
        else
            dists.Add(pos, infinity)
            infinity
    let getPrev pos =
        let exists, value = prevs.TryGetValue pos
        if exists then
            value
        else
            None
    while queue.Count > 0 do
        let mutable u = Position (Point (-1, -1), 0L)
        let mutable priority = -1
        let success = queue.TryDequeue(&u, &priority)
        if success && priority = getDist u then
            let prev = getPrev u
            for edge, v in neighbours prev u do
                let alt = getDist u + edge
                if alt < getDist v then
                    dists[v] <- alt
                    prevs[v] <- let (Position (pu, _)) = u in Some pu
                    queue.Enqueue(v, alt)
    
    dists, prevs

let buildMap allowUpslope (arr:Arr) =
    let validDirections p =
        if allowUpslope then
            allDirections
        else
            let (Point (x, y)) = p
            match arr[x, y] with
            | 'v' -> [Down]
            | '<' -> [Left]
            | '>' -> [Right]
            | '^' -> [Up]
            | _ -> allDirections
    let neighbours p = validDirections p |> List.map delta |> List.map (fun d -> add p d) |> List.filter (fun np -> arr[px np, py np] <> '#')
    
    let dims = dims2 arr
    let points = Seq.allPairs [1..dims.XLength-2] [1..dims.YLength-2] |> Seq.filter (fun (x, y) -> arr[x,y] <> '#') |> Seq.map Point
    points |> Seq.map (fun p -> (p, neighbours p)) |> Map.ofSeq
    

let result allowUpslope arr =       
    let initial = Point(2, 1)
    let dims = dims2 arr
    let goal = Point(dims.XLength-3, dims.YLength-2)

    let forksSeq = Seq.allPairs [1..dims.XLength-2] [1..dims.YLength-2] |> Seq.map Point |> Seq.filter (isFork arr)
    let indices = Seq.initInfinite (pown 2L)
    let forksByIndex = Seq.zip indices forksSeq |> Map.ofSeq
    let forksByPoint = forksByIndex |> Map.toSeq |> Seq.map (fun (x, y) -> y, x) |> Map.ofSeq
    printfn $"Fork count {forksByIndex.Count}"
    
    longestPath arr forksByPoint allowUpslope goal 0L 0 None initial
let resultA = result false
let result2 allowUpslope arr =
    let initial = Point(2, 1)
    let dims = dims2 arr
    let goal = Point(dims.XLength-3, dims.YLength-2)
    let map = buildMap allowUpslope arr
    let forksSeq = map |> Map.toSeq |> Seq.filter (fun (p, ns) -> ns.Length > 2) |> Seq.map fst
    let forksByPoint = Seq.zip forksSeq (Seq.initInfinite (pown 2L)) |> Map.ofSeq
    let dists, prevs = dijkstra map forksByPoint (Position (initial, 0L))
    dists |> Seq.map (fun kv -> kv.Key, kv.Value) |> Seq.filter (fun kv -> let (Position (p, _)) = fst kv in p = goal) |> Seq.map snd |> Seq.min
let result3 allowUpslope arr =
    let initial = Point(2, 1)
    let dims = dims2 arr
    let goal = Point(dims.XLength-3, dims.YLength-2)
    let map = buildMap allowUpslope arr
    let forksSeq = map |> Map.toSeq |> Seq.filter (fun (p, ns) -> ns.Length > 2) |> Seq.map fst
    let forksByPoint = Seq.zip forksSeq (Seq.initInfinite (pown 2L)) |> Map.ofSeq
    longestPath3 map forksByPoint goal 0 None (Position (initial, 0L))
let resultA2 = result2 false
let resultA3 = result3 false
let resultB = result3 true
        
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
        verify (resultA3 testInput) 94
    let input = inputLines day |> parse
    verify (resultA3 input) 2294

    if v then
        verify (resultB testInput) 154
    // verify (resultB input) 0
