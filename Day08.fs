module aoc_2023.Day08
open Utility
let day = 8

let parseLine = id

let resultA = List.length

let resultB = List.length

let run v =
    use _ = measureElapsed day
    
    let stestInput = """
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"""
    
    let testInput = stestInput |> multiLineToList |> List.map parseLine
    // let input = (inputLines day) |> List.map parseLine
    if v then
        verify (resultA testInput) -1
    // verify (resultA input) -1
    
    if v then
        verify (resultB testInput) -1
    // verify (resultB input) -1
