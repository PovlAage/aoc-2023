module aoc_2023.Day05
open System
open Utility
let day = 5

type Seeds = Seeds of int64 list
type MappingRange = { Source: int64; Destination: int64; Length: int64 }
type Mapping = { Name: string; Mappings: MappingRange list }

let map i (m:Mapping) =
    let rec loop rest =
        match rest with
        | [] -> i
        | mr :: rest ->
            if mr.Source <= i && i < mr.Source + mr.Length then
                mr.Destination + (i - mr.Source)
            else
                loop rest
    let res = loop m.Mappings
    // printfn $"{m.Name} maps {i} to {res}"
    res

let resultA (Seeds s, mappings: Mapping list) =
    let folder is m = is |> List.map (fun i -> map i m)
    mappings |> List.fold folder s |> List.min
    
let rec splitInts (s:string) =
    s.Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) |> List.ofArray |> List.map int64

let parseSeedsLine (seedsLine:string) =
    let _, sseeds = seedsLine |> split2 ':'
    Seeds (splitInts sseeds)
    
let parseMappingRange (line:string) =
    match splitInts line with
    | [destination; source; length] -> { Source = source; Destination = destination; Length = length }
    | _ -> failwithf $"Could not parse mapping range {line}"
    
let parseMapping (mappingChunk:string list) =
    { Name = mappingChunk[0]; Mappings = mappingChunk[1..] |> List.map parseMappingRange }
    
let parse (lines:string list) =
    let chunks = lines |> chunkLines
    let seedsLine = parseSeedsLine (chunks[0][0])
    let mappings = chunks[1..] |> List.map parseMapping
    (seedsLine, mappings)
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
        verify (resultA testInput) 35
    let input = (inputLines day) |> parse
    verify (resultA input) 340994526L
    //
    // if v then
    //     verify (resultB testInput) 30
    // verify (resultB input) 13114317
    
    printfn $"day {day} elapsed {sw.ElapsedMilliseconds} ms"
