module aoc_2023.Day21
open Utility
open ArrayAoc2D
let day = 21
type Arr = char array2d
let parse lines =
    let arr = lines2array lines
    let dims = dims2 arr
    let arrPadded = Array2D.create (dims.XLength+2) (dims.YLength+2) '#'
    Array2D.blit arr 0 0 arrPadded 1 1 dims.XLength dims.YLength
    arrPadded

let step1 (scratch:char[,]) (arr:char[,]) =
    let neighbours = allDirections |> List.map delta
    arr |> Array2D.iteri (fun x y c -> if c = 'O' then for (dx, dy) in neighbours do if scratch[x+dx, y+dy] = '.' then scratch[x+dx, y+dy] <- 'O')

let resultA arr steps =
    let cleanCopy = Array2D.copy arr
    arr |> Array2D.iteri (fun x y c -> if c = 'S' then cleanCopy[x, y] <- '.')
    let initial = Array2D.copy arr
    arr |> Array2D.iteri (fun x y c -> if c = 'S' then initial[x, y] <- 'O')
    let folder arr _ =
        let scratch = Array2D.copy cleanCopy
        step1 scratch arr
        scratch
    let final = [1..steps] |> List.fold folder initial
    let mutable count = 0
    final |> Array2D.iter (fun c -> if c = 'O' then count <- count + 1)
    count
        
let run v =
    use _ = measureElapsed day
    
    let stestInput = """
...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
"""
    let testInput = stestInput |> multiLineToList |> parse
    if v then
        verify (resultA testInput 6) 16
    let input = inputLines day |> parse
    verify (resultA input 64) 3724
    
    let steps = 26501365
    
    //