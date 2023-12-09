module aoc_2023.Day09
open Utility
let day = 9

let parse = List.map splitInt

let diffs = List.pairwise >> List.map (fun (x, y) -> y - x)

[<TailCall>]
let rec extrapolate acc ns =
    if ns |> List.forall ((=) 0) then
        acc |> List.fold (fun (currentLeft, currentRight) (left, right) -> left - currentLeft, right + currentRight) (0, 0)
    else
        extrapolate (((List.head ns), (List.last ns)) :: acc) (diffs ns)

let lineResult = extrapolate []        
let resultA = List.sumBy (lineResult >> snd)
let resultB = List.sumBy (lineResult >> fst)

let run v =
    use _ = measureElapsed day
    
    let stestInput = """
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
"""
    if v then
        verifyq (lineResult [0;3;6;9;12;15] |> snd) 18
        let testInput = stestInput |> multiLineToList |> parse
        verifyq (resultA testInput) 114
    let input = (inputLines day) |> parse
    verify (resultA input) 1884768153

    if v then
        verifyq (lineResult [10; 13; 16; 21; 30; 45] |> fst) 5

    verify (resultB input) 1031
