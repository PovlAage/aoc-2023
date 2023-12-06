module aoc_2023.Day07
open Utility
let day = 7
let parse = id
let resultA lines = -1
    
let run v =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    printfn $"day {day}"
    
    let stestInput = """
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"""
    let testInput = stestInput |> multiLineToList |> parse
    
    if v then
        verify (resultA testInput) 0
    // let input = (inputLines day) |> parse
    // verify (resultA input) 0
    
    // if v then
    //     verify (resultB testInput) 46
    // verify (resultB input) 52210644
    
    printfn $"day {day} elapsed {sw.ElapsedMilliseconds} ms"
