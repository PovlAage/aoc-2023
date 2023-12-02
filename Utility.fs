module aoc_2023.Utility

let verify actual expected =
    if actual = expected then
        printfn $"OK {actual}"
    else
        printfn $"FAIL {actual} != {expected}"
let filename no = if no < 10 then $"../../../Input/input0{no}" else $"../../../Input/input{no}"

let inputLine no = System.IO.File.ReadAllText(filename no).Trim()
let inputLines no = System.IO.File.ReadAllLines(filename no) |> List.ofArray

let multiLineToList (s:string) = s.Split(System.Environment.NewLine) |> List.ofArray |> List.filter (fun x -> not (System.String.IsNullOrWhiteSpace(x)))

let digit n d =
    (n / (pown 10 d)) % 10

let digitl (n:int64) d =
    int(n / (pown 10L d)) % 10

let mapIsSubSet<'a when 'a : comparison> (sub:Map<'a, int>) (super:Map<'a, int>) =
    let hasMore k n =
        match Map.tryFind k super with
        | Some v when v >= n -> true
        | _ -> false
    sub |> Map.forall hasMore

let mapUnion<'a when 'a : comparison> (x:Map<'a, int>) (y:Map<'a, int>) =
    let keysUnion = Set.unionMany ([x; y] |> List.map (Map.keys >> Set.ofSeq))
    let maxOpt v1 v2 =
        max (Option.defaultValue 0 v1) (Option.defaultValue 0 v2)
    keysUnion |> Seq.map (fun k -> (k, maxOpt (Map.tryFind k x) (Map.tryFind k y))) |> Map.ofSeq
