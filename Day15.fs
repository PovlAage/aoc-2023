module aoc_2023.Day15
open Utility
let day = 15

type Box = Box of (string * int) list
type State =
    { Boxes: Box list }
    static member Empty() = { Boxes = List.replicate 256 (Box []) }

type Instruction =
    | Equals of string * int
    | Minus of string
let parseInstruction (s:string) =
    if s.EndsWith('-') then
        Minus (s.Substring(0, s.Length - 1))
    else
        let k, v = split2 '=' s
        Equals (k, int v)
let parseA (line:string) = line.Split(',') |> List.ofArray
let parseB = List.map parseInstruction
let dump state =
    for i, b in state.Boxes |> List.indexed do
        let (Box lenses) = b
        if lenses.Length > 0 then
            printfn $"Box {i}: {b}"
        
let hash (s:string) = s.ToCharArray() |> Seq.map int |> Seq.fold (fun h c -> ((h + c) * 17) % 256) 0
let execute state instruction =
    let isMatch k0 (k, v) = (k = k0)
    let h, k = match instruction with
                | Equals (k, _) -> hash k, k
                | Minus k -> hash k, k
    let (Box lenses) = state.Boxes[h]
    let existingLens = lenses |> List.tryFindIndex (isMatch k)
    let lenses =
        match (instruction, existingLens) with
        | Equals (k, v), Some i -> lenses |> List.removeAt i |> List.insertAt i (k, v)
        | Equals (k, v), None -> (k, v) :: lenses
        | Minus k, Some i -> lenses |> List.removeAt i
        | Minus k, None -> lenses
    { Boxes = state.Boxes |> List.removeAt h |> List.insertAt h (Box lenses) }
let resultA = List.sumBy hash
let focusingPower (boxNumber, Box lenses) =
    lenses |> List.rev |> List.indexed |> List.sumBy (fun (i, (k, v)) -> (1 + boxNumber) * (1 + i) * v)
let resultB instructions =
    let finalState = instructions |> List.fold execute (State.Empty())
    finalState.Boxes |> List.indexed |> List.sumBy focusingPower

let run v =
    use _ = measureElapsed day
    
    let testInput = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" |> parseA
    if v then
        verifyq (hash "HASH") 52
        verifyq (resultA testInput) 1320
    let input = inputLine day |> parseA
    verify (resultA input) 501680

    if v then
        verifyq (resultB (parseB testInput)) 145
    verify (resultB (parseB input)) 241094
