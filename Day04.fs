module aoc_2023.Day04
open System
open Utility
let day = 4

type Card = { No: int; WinningNumbers: int list; MyNumbers: int list}
let matches c = (Set.intersect (Set.ofList c.WinningNumbers) (Set.ofList c.MyNumbers)).Count
let worth c =
    let matches = matches c
    if matches = 0 then 0 else pown 2 (matches - 1)
let resultA = List.sumBy worth
let resultB (cards: Card list) =
    let counts = Array.create (cards.Length) 1L
    for x in 0..(cards.Length - 2) do // last card cannot generate copies
        let matches = matches cards[x]
        for y in (x+1)..(min (cards.Length - 1) (x + matches)) do
            counts[y] <- counts[y] + counts[x]
    counts |> Array.sum        

let parse lines =
    let parseLine (line:string) =
        let opts = StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries
        let scard, nums = split2 ':' line
        let sWinningNumbers, sNumbersYouHave = split2 '|' nums
        let spacedNums (s:string) = s.Split(' ', opts) |> List.ofArray |> List.map int
        let _, scardno = split2space scard 
        let card = {
            No = int scardno
            WinningNumbers = spacedNums sWinningNumbers  
            MyNumbers = spacedNums sNumbersYouHave  
        }
        card
    lines |> List.map parseLine

let run v =
    use _ = measureElapsed day
    
    let stestInput = """
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"""
    let testInput = stestInput |> multiLineToList |> parse
    if v then
        verify (resultA testInput) 13
    let input = (inputLines day) |> parse
    verify (resultA input) 24706
    
    if v then
        verify (resultB testInput) 30
    verify (resultB input) 13114317
