module aoc_2023.Day17
open Utility
open ArrayAoc2D
let day = 17
type Arr = int array2d
let parse = lines2array >> Array2D.map (fun c -> int (string (c)))
type Position = Position of Point * (Direction * int)
let nextPoint arr pos dd =
    let (Position (p, (d, u))) = pos
    let pp = add p (delta dd)
    let (Point (ppx, ppy)) = pp
    if isInBounds arr pp then
        Some (arr[ppx, ppy], Position (pp, (dd, if dd = d then u+1 else 1)))
    else
        None
let normalNeighbours (arr:Arr) pos =
    let (Position (p, (d, u))) = pos
    let notMoreThanThree dd = u < 3 || dd <> d
    let notReverse dd = match dd with
                            | Down -> d <> Up
                            | Up -> d <> Down
                            | Left -> d <> Right
                            | Right -> d <> Left
    let isInbounds dd = isInBounds arr (add p (delta dd)) 
    let isValidDirection dd = notMoreThanThree dd && notReverse dd && isInbounds dd
    allDirections |> List.filter isValidDirection |> List.choose (nextPoint arr pos)

let ultraNeighbours (arr:Arr) pos =
    let (Position (p, (d, u))) = pos
    let X, Y = dims arr
    let goal = Point(X-1, Y-1)
    let straightBetween4and10 dd =
        if u < 4 then
            dd = d
        elif 4 <= u && u < 10 then
            true
        else
            dd <> d
    let notReverse dd = match dd with
                            | Down -> d <> Up
                            | Up -> d <> Down
                            | Left -> d <> Right
                            | Right -> d <> Left
    let isValidPosition (Position (pp, (dd, uu))) =
        (pp <> goal || uu >= 4)
    let isValidDirection dd = straightBetween4and10 dd && notReverse dd
    allDirections |> List.filter isValidDirection |> List.choose (nextPoint arr pos) |> List.filter (snd >> isValidPosition)

let dijkstra neighbours maxu (arr:Arr) initial =
    let queue = System.Collections.Generic.PriorityQueue()
    let dists = System.Collections.Generic.Dictionary()
    let prevs = System.Collections.Generic.Dictionary()
    dists.Add(initial, 0)

    let infinity = System.Int32.MaxValue    
    for u in 1..maxu do
        for d in allDirections do
            arr |> Array2D.iteri (fun x y c -> let pos = Position (Point(x, y), (d, u)) in dists.Add(pos, infinity); prevs.Add(pos, None))
    queue.Enqueue(initial, 0)
    
    while queue.Count > 0 do
        let mutable u = Position (Point (-1, -1), (Up, -1))
        let mutable priority = -1
        let success = queue.TryDequeue(&u, &priority)
        if success && priority = dists[u] then
            for edge, v in neighbours arr u do
                let alt = dists[u] + edge
                if alt < dists[v] then
                    dists[v] <- alt
                    prevs[v] <- Some u
                    queue.Enqueue(v, alt)
    
    dists, prevs

let minDist neighbours minu maxu arr =     
    let initial = Position (Point(0, 0), (Right, 0))
    let X, Y = ArrayAoc2D.dims arr
    let goal = Point(X-1, Y-1)
    let dists, prevs = dijkstra neighbours maxu arr initial
    let goalPos = dists.Keys |> Seq.filter (fun (Position (p, (d, u))) -> p = goal && u >= minu) |> Seq.minBy (fun p -> dists[p])
    // let mutable p = goalPos
    // while p <> initial do
    //     printfn $"{p}\t{dists[p]}"
    //     p <- prevs[p].Value
    dists[goalPos]
let resultA = minDist normalNeighbours 0 3
let resultB = minDist ultraNeighbours 4 10
let run v =
    use _ = measureElapsed day
    
    let stestInput = """
2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
"""
    let testInput = stestInput |> multiLineToList |> parse
    if v then
        verify (resultA testInput) 102
    let input = inputLines day |> parse
    verify (resultA input) 859
    
    if v then
        verify (resultB testInput) 94
        let stestInput2 = """
111111111111
999999999991
999999999991
999999999991
999999999991
"""
        let testInput2 = stestInput2 |> multiLineToList |> parse
        verify (resultB testInput2) 71

    verify (resultB input) 0
