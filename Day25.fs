module aoc_2023.Day25
open System
open Utility

let day = 25
type Graph = { V: string list; E: (string * string) list }
let parseLine (line:string) =
    let a, bs = line |> split2 ':'
    let bs = bs.Split(' ', StringSplitOptions.TrimEntries)
    bs |> Array.toList |> List.map (fun b -> (a, b))
let parse lines =
    let swap (a, b) = b, a
    // let sort pair = if (fst pair).CompareTo(snd pair) < 0 then pair else swap pair
    let connections = lines |> List.map parseLine |> List.collect id
    { V = connections |> List.map fst |> List.distinct; E = connections }
    // let reversed = connectionsSorted |> List.map swap
    // Graph (Map.ofList (connectionsSorted |> List.append reversed |> List.groupBy fst |> List.map (fun grouping -> (fst grouping, grouping |> snd |> List.map snd)))

[<TailCall>]
let rec karger (random:System.Random) (graph:Graph) =
    if graph.V.Length = 2 then
        graph
    else
        let contract graph ei =
            let u, v = graph.E[ei]
            let uv = u + v
            let replaceedge = function
                | w1, w2 when (w1 = u && w2 = v) || (w1 = v && w2 = u) -> None
                | w1, w2 when w1 = u || w1 = v -> Some (uv, w2)
                | w1, w2 when w2 = u || w2 = v -> Some (w1, uv)
                | e -> Some e
            let replacev = function
                | w when w = u || w = v -> None
                | w -> Some w
            { V = uv :: (graph.V |> List.choose replacev); E = graph.E |> List.choose replaceedge }
        let ei = random.Next(graph.E.Length)
        let graph = contract graph ei
        karger random graph
let resultA input =
    let random = System.Random()
    let rec loop() =
        let graph = karger random input
        if graph.E.Length > 3 then
            loop()
        else
            graph.V[0].Length * graph.V[1].Length / (3 * 3)
    loop()

let run v =
    use _ = measureElapsed day
    
    let stestInput = """
jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr
"""
    let testInput = stestInput |> multiLineToList |> parse
    if v then
        verify (resultA testInput) 54
    let input = inputLines day |> parse
    verify (resultA input) 600225
