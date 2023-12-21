module aoc_2023.Day20
open System
open Utility
let day = 20
type OnOff =
    | On
    | Off
type Pulse =
    | High
    | Low
type ModuleName = string
type ModuleType =
    | FlipFlop of OnOff
    | Conjunction of Map<ModuleName, Pulse>
    | Broadcast
type Module = { Name: ModuleName; Type: ModuleType; Destinations: List<ModuleName> }
[<Struct>]
type Message = { Src: ModuleName; Dest: ModuleName; Pulse: Pulse }
let parseLine (line:string) =
    let [|source; dest|] = line.Split("->", StringSplitOptions.TrimEntries)
    let dests = dest.Split(',', StringSplitOptions.TrimEntries) |> List.ofArray
    let name, moduleType =
                     match source with
                     | s when s[0] = '%' -> source.Substring(1), FlipFlop Off
                     | s when s[0] = '&' -> source.Substring(1), Conjunction Map.empty
                     | "broadcaster" -> source, Broadcast
                     | _ -> failwithf $"Unmatched {source}"
    { Name = name; Type = moduleType; Destinations = dests }
let parse lines =
    let modules = lines |> List.map parseLine
    let dest2sources = modules |> Seq.collect (fun m -> m.Destinations |> Seq.map (fun d -> (m.Name, d))) |>
                        Seq.groupBy snd |> Seq.map (fun (k, vs) -> (k, vs |> Seq.map fst)) |> Map.ofSeq
    let initSources sources = sources |> Seq.map (fun s -> (s, Low)) |> Map.ofSeq
    modules |> List.map (fun m -> if m.Type = (Conjunction Map.empty) then { m with Type = Conjunction (initSources dest2sources[m.Name])} else m)
    

let receive m message =
    let m2, p2 =
        match m with
        | FlipFlop Off when message.Pulse = Low -> FlipFlop On, Some High
        | FlipFlop On when message.Pulse = Low -> FlipFlop Off, Some Low
        | FlipFlop _ when message.Pulse = High -> m, None
        | Conjunction memory ->
            let memory = memory |> Map.add message.Src message.Pulse
            Conjunction memory, Some (if memory |> Map.values |> Seq.forall ((=) High) then Low else High)
        | Broadcast -> Broadcast, Some message.Pulse
    m2, p2
    
[<TailCall>]
let rec loop modules monitor (countLow, countHigh) queue =
    match queue with
    | message :: rest ->
        let monitorCount, monitorList = monitor
        if monitorList |> List.contains (message.Src, message.Pulse) then
            printfn $"Monitor {(message.Src, message.Pulse)} {monitorCount}"
            
        let countLow = if message.Pulse = Low then countLow + 1 else countLow 
        let countHigh = if message.Pulse = High then countHigh + 1 else countHigh 

        let modules, messages =
            if modules |> Map.tryFind message.Dest = None then
                if message.Pulse = Low && message.Dest = "rx" then
                    printfn $"Low pulse for rx: {message}"
                    failwithf $"forced exit"
                    modules, []
                else
                    modules, []
            else
                let current = modules |> Map.find message.Dest
                let m2, p2 = receive current.Type message
                match p2 with
                | Some p2 ->
                    let messages = current.Destinations |> List.map (fun d -> { Src = message.Dest; Dest = d; Pulse = p2 })
                    modules |> Map.add current.Name {current with Type = m2}, messages
                | None -> modules, []
        loop modules monitor (countLow, countHigh) (List.append rest messages)
    | [] -> modules, (countLow, countHigh)
let button n moduleMap monitor =
    let button = {Src = "button"; Dest = "broadcaster"; Pulse = Low }
    List.replicate n 1 |> List.fold (fun (mm, count) _ -> loop mm monitor count [button]) (moduleMap, (0, 0))
let moduleMap input = input |> Seq.map (fun m -> m.Name, m) |> Map.ofSeq
// let test1 input =
//     let mm = moduleMap input
//     let count, mm = button mm
//     count
let resultA input =
    // for m in input do
    //     printfn $"{m}"
    let moduleMap = moduleMap input
    let moduleMap, (countLow, countHigh) = button 1000 moduleMap (0, [])
    countLow * countHigh

let resultB input =
    for m in input do
        printfn $"{m}"
    let moduleMap = moduleMap input
    let folder (mm:Map<ModuleName, Module>) i =
        if i % 1000000 = 0 then
            printfn $"Button {i}"
        // let dump m =
        //     match m.Type with
        //     | FlipFlop On -> printfn $"{m.Name}"
        //     | _ -> ()
        // for m in mm.Values |> Seq.sortBy _.Name do
        //     dump m
        try
            let mm, _ = button 1 mm (i, [("kd", High); ("zf", High); ("vg", High); ("gs", High)])
            mm
        with
            _ -> failwithf $"Button {i}"
    Seq.initInfinite ((+) 1) |> Seq.fold folder moduleMap |> ignore
    
let run v =
    use _ = measureElapsed day
    
    let stestInput1 = """
broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a
"""
    let stestInput2 = """
broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output
"""
    if v then
        let testInput1 = stestInput1 |> multiLineToList |> parse
        verify (resultA testInput1) 32000000
        let testInput2 = stestInput2 |> multiLineToList |> parse
        verify (resultA testInput2) 11687500
    let input = inputLines day |> parse
    verify (resultA input) 808146535
    //
    // if v then
    //     let testInput = stestInput |> multiLineToList |> parse
    //     verify (resultB testInput) 167409079868000L
    // let input = inputLines day |> parse
    // verify (resultB input)

// lcm64 (lcm64 (lcm64 3767 3779) 3889) 4057
// val it: int64 = 224602953547789L