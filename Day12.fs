module aoc_2023.Day12
open System.Collections.Generic
open Utility
let day = 12

type Record = string * int list
let parseLine line =
    let springs, sgroups = split2space line
    let groups = sgroups.Split(',') |> Array.map int |> List.ofArray
    springs, groups
let parse = List.map parseLine

let cache = Dictionary<_, _>()
let rec countCombinations (springs:string, groups:int list) =
    let isPossibleUndamaged i = springs[i] <> '#'
    let isPossibleDamaged i = springs[i] <> '.'
    let isPossible len index =
        assert (index + len <= springs.Length)
        (index = 0 || isPossibleUndamaged (index - 1)) &&
        [index..(index+len-1)] |> List.forall isPossibleDamaged &&
        (index + len = springs.Length || isPossibleUndamaged (index + len))
    let excise len index =
        assert (index + len <= springs.Length)
        if index = 0 then
            if len = springs.Length then
                "", ""
            else
                assert isPossibleUndamaged len
                "", springs.Substring(len + 1)
        elif index + len < springs.Length then
            assert isPossibleUndamaged (index - 1)
            assert isPossibleUndamaged (index + len)
            springs.Substring(0, index - 1), springs.Substring(index + len + 1)
        else
            assert isPossibleUndamaged (index - 1)
            springs.Substring(0, index - 1), ""

    if List.isEmpty groups then
        if springs.IndexOf('#') = -1 then 1L else 0L
    elif System.String.IsNullOrEmpty springs then
        0L
    elif springs.IndexOf('#') = -1 && springs.IndexOf('?') = -1 then
        0L
    else
        let cacheKey = springs + System.String.Join(";", groups)
        let isCached, value = cache.TryGetValue cacheKey
        if isCached then
            value
        else
            let pivotIndex, pivotValue = groups |> List.indexed |> List.maxBy snd
            let lower, (_ :: upper) = groups |> List.splitAt pivotIndex
            let value =
                [0..(springs.Length-pivotValue)] |>
                List.filter (isPossible pivotValue) |>
                List.map (fun i -> excise pivotValue i) |>
                List.sumBy (fun (l, u) -> (countCombinations (l, lower) * countCombinations (u, upper)))
            cache.Add (cacheKey, value)
            value
        
let resultA lines = lines |> List.sumBy countCombinations

let resultB lines =
    let unfold (springs:string, groups:int list) =
        (System.String.Join('?', List.replicate 5 springs), List.replicate 5 groups |> List.concat)
    lines |> List.map unfold |> resultA
let run v =
    use _ = measureElapsed day
    
    let stestInput = """
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
"""
    let testInput = stestInput |> multiLineToList |> parse
    if v then
        verifyq (resultA testInput) 21
    let input = (inputLines day) |> parse
    verify (resultA input) 7716

    if v then
        verifyq (resultB testInput) 525152
    verify (resultB input) 18716325559999L
