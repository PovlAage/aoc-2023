module aoc_2023.Day14
open Utility
let day = 14

let parse (lines:string list) =
    let mx, my = (lines[0].Length - 1), (lines.Length - 1)
    let arr = Array2D.create (mx+3) (my+3) '#'
    for y in 0..my do
        for x in 0..mx do
            arr[x+1, y+1] <- lines[y][x]
    arr

let dump arr =
    let mx, my = (Array2D.length1 arr - 1), (Array2D.length2 arr - 1)
    for y in 0..my do
        for x in 0..mx do
            printf $"{arr[x, y]}"
        printfn ""

let tilt (dx, dy) (arr:char[,]) =
    let mutable progress = true
    while progress do
        progress <- false
        let mx, my = Array2D.length1 arr - 1, Array2D.length2 arr - 1
        let move x y c =
            if c = '.' && arr[x+dx, y+dy] = 'O' then arr[x, y] <- 'O'; arr[x+dx, y+dy] <- '.'; progress <- true
        arr |> Array2D.iteri move
let tiltNorth = tilt (0, 1)
let tiltSouth = tilt (0, -1)
let tiltEast = tilt (-1, 0)
let tiltWest = tilt (1, 0)
let spinCycle arr =
    tiltNorth arr
    tiltWest arr
    tiltSouth arr
    tiltEast arr

let getLoad arr =
    let my = Array2D.length2 arr - 1
    let mutable sum = 0
    let measure x y c =
        if c = 'O' then sum <- sum + my - y
    arr |> Array2D.iteri measure
    sum

let resultA arr =
    let copy = Array2D.copy arr
    tiltNorth copy
    getLoad copy

let resultB arr =
    let arr = Array2D.copy arr
    let skip = 100
    for i in 1..skip do
        spinCycle arr

    let (i0, load0, arr0:char[,]) = (skip, getLoad arr, Array2D.copy arr)

    let matches = seq {
        for i in (skip+1)..1000000 do
            spinCycle arr
            let thisLoad = getLoad arr
            if thisLoad = load0 then
                let mx, my = Array2D.length1 arr - 1, Array2D.length2 arr - 1
                let diff = seq { for x in 1..mx do for y in 1..my -> struct (x, y) } |> Seq.filter (fun struct (x, y) -> arr[x, y] <> arr0[x, y])
                if diff |> Seq.isEmpty then yield (i, thisLoad, Array2D.copy arr)
    }

    let (i1, load1, arr1) = matches |> Seq.head
    let cycleLength = i1 - i0
    let copy = Array2D.copy arr1
    let mutable i2 = i1
    while ((i2 - i0) % cycleLength <> (1000000000 - i0) % cycleLength) do
        spinCycle copy
        i2 <- i2 + 1
    getLoad copy
let run v =
    use _ = measureElapsed day
    
    let stestInput = """
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
"""
    let testInput = stestInput |> multiLineToList |> parse
    // dump testInput
    if v then
        verify (resultA testInput) 136
    let input = inputLines day |> parse
    verify (resultA input) 107430

    if v then
        verify (resultB testInput) 64
    //
    // if v then
    //     verify (resultB testInput) 400
    verify (resultB input) 96317
