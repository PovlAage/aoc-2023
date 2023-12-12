module aoc_2023.Day12
open System
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

let groupsAsKey groups =
    let hex = function
        | i when 1 <= i && i <= 9 -> string(i)[0]
        | i when 10 <= i && i <= 15 -> 'a' + char(i - 10)
        | i -> failwithf $"Unmatched {i}"
    groups |> List.map hex |> Array.ofList |> String
let rec countCombinations (springs:string, groups:int list, groupsKey:string) =
    let springs = springs.Trim('.')

    let isPossibleUndamaged i = springs[i] <> '#'
    let isPossibleDamaged i = springs[i] <> '.'
    let tryExcise len index =
        assert (index + len <= springs.Length)
        if not ((index = 0 || isPossibleUndamaged (index - 1)) &&
               {index..(index+len-1)} |> Seq.forall isPossibleDamaged &&
               (index + len = springs.Length || isPossibleUndamaged (index + len))) then
            None
        else
            if index = 0 then
                if len = springs.Length then
                    Some (String.Empty, String.Empty)
                else
                    Some (String.Empty, springs.Substring(len + 1))
            elif index + len < springs.Length then
                Some (springs.Substring(0, index - 1), springs.Substring(index + len + 1))
            else
                Some (springs.Substring(0, index - 1), String.Empty)

    if List.isEmpty groups then
        if springs.IndexOf('#') = -1 then 1L else 0L
    elif String.IsNullOrEmpty springs then
        0L
    elif List.sum groups + List.length groups - 1 > springs.Length then
        0L
    elif List.sum groups > springs.Replace(".", "").Length then
        0L
    else
        let cacheKey = springs + groupsKey
        let isCached, value = cache.TryGetValue cacheKey
        if isCached then
            value
        else
            let pivotIndex, pivotValue = groups |> Seq.indexed |> Seq.maxBy snd
            match groups |> List.splitAt pivotIndex with
            | lower, _ :: upper ->
                let lowerKey, upperKey = groupsKey.Substring(0, pivotIndex), groupsKey.Substring(pivotIndex + 1)
                let value =
                    {0..(springs.Length-pivotValue)} |>
                    Seq.choose (tryExcise pivotValue) |>
                    Seq.sumBy (fun (l, u) -> (countCombinations (l, lower, lowerKey) * countCombinations (u, upper, upperKey)))
                cache.Add (cacheKey, value)
                value
            | x -> failwithf $"Unmatched {x}"
        
let resultA = List.sumBy (fun (a, b) -> countCombinations (a, b, groupsAsKey b))

let unfold (springs:string, groups:int list) =
    (String.Join('?', List.replicate 5 springs), List.replicate 5 groups |> List.concat)
let resultB = List.map unfold >> resultA

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
