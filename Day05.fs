module aoc_2023.Day05
open System
open Utility
let day = 5

type Range = { Start: int64; End: int64 }
type Seeds = Seeds of int64 list
type MappingPart = { Source: int64; Destination: int64; Length: int64 }
type Mapping = { Name: string; MappingParts: MappingPart list }

let parseSeedsLine (seedsLine:string) =
    let _, sseeds = seedsLine |> split2 ':'
    Seeds (splitInt64 sseeds)

let parseMappingRange (line:string) =
    match splitInt64 line with
    | [destination; source; length] -> { Source = source; Destination = destination; Length = length }
    | _ -> failwithf $"Could not parse mapping range {line}"
    
let parseMapping (mappingChunk:string list) =
    { Name = mappingChunk[0]; MappingParts = mappingChunk[1..] |> List.map parseMappingRange }
    
let parse (lines:string list) =
    let chunks = lines |> chunkLines
    let seedsLine = parseSeedsLine (chunks[0][0])
    let mappings = chunks[1..] |> List.map parseMapping
    (seedsLine, mappings)

let range s e =
    if e < s then failwithf "e<s: {e}<{s}"
    { Start = s; End = e }
let translate delta r = { Start = r.Start + delta; End = r.End + delta }
let sourceRange mp = { Start = mp.Source; End = mp.Source + mp.Length - 1L }
let overlap (r:Range) (mr:Range) =
    if r.Start < mr.Start then
        if r.End < mr.Start then
            None, [r]            
        elif r.End <= mr.End then
            Some { Start = mr.Start; End = r.End }, [{ Start = r.Start; End = mr.Start - 1L }]
        else
            Some mr, [{ Start = r.Start; End = mr.Start - 1L }; { Start = mr.End + 1L; End = r.End }]
    elif r.Start <= mr.End then
        if r.End <= mr.End then
            Some r, []
        else
            Some { Start = r.Start; End = mr.End }, [{ Start = mr.End + 1L; End = r.End }]
    else
        None, [r]

let mapOne (mr:MappingPart) (rs:Range list) =
    let rec loop acc rs =
        match rs with
        | [] -> acc
        | r :: rs ->
            let overlap, remainder = overlap r (sourceRange mr)
            let mapped = match overlap with
                            | Some overlap -> (translate (mr.Destination - mr.Source) overlap) :: fst acc
                            | None -> fst acc
            let unmapped = remainder @ snd acc
            loop (mapped, unmapped) rs
    loop ([], []) rs

let map (rs:Range list) (m:Mapping) =
    let rec loop acc mappingParts disjointInput =
        match mappingParts, disjointInput with
        | _, [] -> acc
        | [], _ -> disjointInput @ acc
        | mr :: mrs, _ ->
            let mapped, unmapped = mapOne mr disjointInput
            loop (mapped @ acc) mrs unmapped
    loop [] m.MappingParts rs

let resultA (Seeds seeds, mappings: Mapping list) =
    let folder = map
    let seedRanges = seeds |> List.map (fun s -> range s s)
    mappings |> List.fold folder seedRanges |>List.map (fun r -> r.Start) |> List.min
    
let resultB (Seeds seeds, mappings: Mapping list) =
    let folder = map
    let seedRanges = [for i in 0..2..seeds.Length-1 -> range seeds[i] (seeds[i] + seeds[i+1] - 1L)]
    mappings |> List.fold folder seedRanges |> List.map (fun r -> r.Start) |> List.min
    
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
        verifyq (overlap (range 1 3) (range 4 6)) (None, [(range 1 3)])
        verifyq (overlap (range 1 4) (range 4 6)) (Some (range 4 4), [(range 1 3)])
        verifyq (overlap (range 1 5) (range 4 6)) (Some (range 4 5), [(range 1 3)])
        verifyq (overlap (range 1 6) (range 4 6)) (Some (range 4 6), [range 1 3])
        verifyq (overlap (range 1 8) (range 4 6)) (Some (range 4 6), [(range 1 3); (range 7 8)])
        verifyq (overlap (range 5 6) (range 4 6)) (Some (range 5 6), [])
        verifyq (overlap (range 5 8) (range 4 6)) (Some (range 5 6), [range 7 8])
        verifyq (overlap (range 7 8) (range 4 6)) (None, [range 7 8])
        verify (resultA testInput) 35
    let input = (inputLines day) |> parse
    verify (resultA input) 340994526L
    
    if v then
        verify (resultB testInput) 46
    verify (resultB input) 52210644
    
    printfn $"day {day} elapsed {sw.ElapsedMilliseconds} ms"
