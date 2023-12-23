module aoc_2023.Day22
open Utility
let day = 22
type Point = { X: int; Y: int; Z: int }
type PointXY = { X: int; Y: int }
type Brick = { Name: int; Points: Point list }
type Snapshot = Brick list
let xy (p:Point) : PointXY = { X = p.X; Y = p.Y } 
let brick name (p1:Point) (p2:Point) =
    let rec brickPoints p1 p2 =
        let { X = x1; Y = y1; Z = z1 }, { X = x2; Y = y2; Z = z2 } = p1, p2
        if x1 <> x2 then
            assert (y1 = y2)
            assert (z1 = z2)
            if x1 > x2 then
                brickPoints p2 p1
            else
                [for x in x1..x2 -> { p1 with X = x }]
        elif y1 <> y2 then
            assert (x1 = x2)
            assert (z1 = z2)
            if y1 > y2 then
                brickPoints p2 p1
            else
                [for y in y1..y2 -> { p1 with Y = y }]
        elif z1 <> z2 then
            assert (x1 = x2)
            assert (y1 = y2)
            if z1 > z2 then
                brickPoints p2 p1
            else
                [for z in z1..z2 -> { p1 with Z = z }]
        else
            [p1]
    let points = brickPoints p1 p2
    { Name = name; Points = points }
let parseLine (i, line) =
    let parsePoint (sp:string) = let [| x; y; z |] = sp.Split(',') |> Array.map int in { X = x; Y = y; Z = z}
    let sp1, sp2 = line |> split2 '~'
    let p1, p2 = parsePoint sp1, parsePoint sp2
    brick i p1 p2
let parse = List.indexed >> List.map parseLine

let zmin brick =
    brick.Points |> List.map _.Z |> List.min

let settle bricks =
    let folder (settled:Brick list) dropping =
        let droppingXY = dropping.Points |> List.map xy
        let droppingZ = zmin dropping
        let floorZ = settled |> List.collect _.Points |>
                        List.map (fun p -> ((xy p), p.Z)) |> List.filter (fun (xy, z) -> droppingXY |> List.contains xy) |> List.map snd
        let floorzmax = if floorZ.IsEmpty then 0 else List.max floorZ
        assert (droppingZ - floorzmax >= 1)
        let droppedBrick = { dropping with Points = dropping.Points |> List.map (fun p -> { p with Z = p.Z - (droppingZ - floorzmax) + 1 }) }
        droppedBrick :: settled
    bricks |> List.sortBy zmin |> List.fold folder []

let supportersMap input =
    let settled = settle input
    let supporters b =
        let zmin = zmin b
        let isSupportedBy b2 =
            let relevantPoints = b2.Points |> List.filter (fun p -> p.Z + 1 = zmin)
            b2.Name <> b.Name && Seq.allPairs b.Points relevantPoints |> Seq.exists (fun (p1, p2) -> p1.X = p2.X && p1.Y = p2.Y && p1.Z = p2.Z + 1)
        settled |> List.filter (fun b2 -> b2.Points |> List.exists (fun p -> p.Z + 1 = zmin)) |> List.filter isSupportedBy |> List.map _.Name
    settled |> Seq.map (fun b -> b.Name, supporters b) |> Map.ofSeq
let resultA input =
    let supportersMap = supportersMap input
    let soleSupporters = supportersMap.Values |> Seq.filter (fun bs -> bs.Length = 1) |> Seq.map List.exactlyOne |> List.ofSeq |> List.distinct
    supportersMap.Count - soleSupporters.Length

// let resultB input =
//     let supportersMap = supportersMap input
    
let run v =
    use _ = measureElapsed day
    
    let stestInput = """
1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9
"""
    let testInput = stestInput |> multiLineToList |> parse
    if v then
        verify (resultA testInput) 5
    let input = inputLines day |> parse
    verify (resultA input) 515
    //
    // let steps = 26501365
    
    //