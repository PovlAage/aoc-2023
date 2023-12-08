module aoc_2023.Day08
open Utility
let day = 8

type NodeInput = { Name: string; Left: string; Right: string }
[<Struct>]
type Node = Node of int * int * bool
type Instruction = Left | Right
type Map = { Instructions: Instruction list; Nodes: NodeInput list }
let parseInstruction = function
    | 'L' -> Left
    | 'R' -> Right
    | c -> failwithf $"Unmatched instruction '{c}'"
let reNode = System.Text.RegularExpressions.Regex(@"^(?<name>\w+) = \((?<left>\w+), (?<right>\w+)\)$")
let parseLine (line:string) =
    let m = reNode.Match(line)
    let value (name:string) = m.Groups[name].Value
    { Name = value "name"; Left = value "left"; Right = value "right" }
let parse (lines:string list) =
    { Instructions = lines[0].ToCharArray() |> Seq.map parseInstruction |> List.ofSeq
      Nodes = lines[2..] |> Seq.map parseLine |> List.ofSeq }
let prepare (nodes:NodeInput list) isAtGoal =
    let nameToIndex = nodes |> Seq.map _.Name |> Seq.indexed |> Seq.map (fun (a, b) -> b, a) |> Map.ofSeq
    Array.init nodes.Length (fun i -> let n = nodes[i] in Node (nameToIndex[n.Left], nameToIndex[n.Right], isAtGoal n.Name))
    
[<TailCall>]
let rec cycleLength (instructions:Instruction list) (nodes:Node array) (steps, pos) =
    let (Node (left, right, isGoal)) = pos
    if isGoal then
        steps
    else
        let nextNode i n =
            match i with
            | Left -> nodes[left]
            | Right -> nodes[right]
        let i = instructions[steps % instructions.Length]
        cycleLength instructions nodes (steps + 1, nextNode i pos)

let resultA (input:Map) =
    let isAtGoal n = (n = "ZZZ")
    let nodes = prepare input.Nodes isAtGoal
    let startIndex = input.Nodes |> List.findIndex (fun n -> n.Name = "AAA")
    cycleLength input.Instructions nodes (0, nodes[startIndex])

let resultB (input:Map) =
    let isAtGoal (n:string) = n[2] = 'Z'
    let nodes = prepare input.Nodes isAtGoal
    let startIndices = input.Nodes |> Seq.indexed |> Seq.filter (fun (i, n) -> n.Name[2] = 'A') |> Seq.map fst
    let startNodes = startIndices |> Seq.map (fun i -> nodes[i])

    let cycleLengths = startNodes |> Seq.map (fun n -> cycleLength input.Instructions nodes (0, n))
    cycleLengths |> Seq.map int64 |> Seq.reduce lcm64

let run v =
    use _ = measureElapsed day
    
    if v then
        let stestInput = """
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
"""
    
        let testInput = stestInput |> multiLineToList |> parse
        verifyq (resultA testInput) 2
    let input = (inputLines day) |> parse
    verify (resultA input) 20569
    
    if v then
        let stestInput = """
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
"""
        let testInput = stestInput |> multiLineToList |> parse
        verifyq (resultB testInput) 6
    verify (resultB input) 21366921060721L
