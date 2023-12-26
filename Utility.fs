module aoc_2023.Utility
open Checked
open System

let verify actual expected =
    if actual = expected then
        printfn $"OK {actual}"
    else
        printfn $"FAIL {actual} != {expected}"
let verifyq actual expected =
    if actual <> expected then
        printfn $"FAIL {actual} != {expected}"
let filename no = if no < 10 then $"../../../Input/input0{no}" else $"../../../Input/input{no}"

let inputLine no = System.IO.File.ReadAllText(filename no).Trim()
let inputLines no = System.IO.File.ReadAllLines(filename no) |> List.ofArray

let isBlank (s:string) = String.IsNullOrWhiteSpace(s)

let multiLineToList (s:string) =
    s.Split(Environment.NewLine) |> List.ofArray |> List.skipWhile isBlank |> List.rev |> List.skipWhile isBlank |> List.rev

let chunkLines (lines:string list) =
    let rec loop acc rest =
        match rest with
        | [] -> List.rev acc
        | _ ->
            let chunk, rest = match rest |> List.tryFindIndex isBlank with
                                | Some blankIndex ->
                                    rest |> List.splitAt blankIndex
                                | None ->
                                    rest, []
            loop (chunk :: acc) (rest |> List.skipWhile isBlank)
    loop [] lines

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

let split2 (c:char) (s:string) =
    match s.Split(c, StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) with
    | [| x ; y |] -> x, y
    | _ -> failwithf $"Could not split {s} into two"

let split2space = split2 ' '

let splitInt (s:string) =
    s.Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) |> List.ofArray |> List.map int

let splitInt64 (s:string) =
    s.Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) |> List.ofArray |> List.map int64

let measureElapsed day =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    { new IDisposable with member x.Dispose() = printfn $"Day {day}: {sw.ElapsedMilliseconds} ms" }

let rec gcd a b =
    match a, b with
    | _ when a < 0 || b < 0 -> gcd (abs a) (abs b)
    | _, 0 -> a
    | _ when a < b -> gcd b a
    | _ -> gcd b (a % b)    
let rec gcd64 (a:int64) (b:int64) =
    match a, b with
    | _ when a < 0 || b < 0 -> gcd64 (abs a) (abs b)
    | _, 0L -> a
    | _ when a < b -> gcd64 b a
    | _ -> gcd64 b (a % b)    
let rec gcdbig (a:bigint) (b:bigint) =
    match a, b with
    | _ when a < bigint.Zero || b < bigint.Zero -> gcdbig (abs a) (abs b)
    | _, zero when zero = bigint.Zero -> a
    | _ when a < b -> gcdbig b a
    | _ -> gcdbig b (a % b)    

let lcm a b = (abs a) * ((abs b) / gcd a b)
let lcm64 (a:int64) (b:int64) = (abs a) * ((abs b) / gcd64 a b)

let charEnum<'E when 'E : enum<char>> c = Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<char, 'E>(c)

let parseHex (s:string) =
    let parseChar (c:char) =
        if '0' <= c && c <= '9' then
            int (c - '0')
        elif 'a' <= c && c <= 'f' then
            int (c - 'a') + 10
        else
            failwithf $"Unmatched {c}"
    s.ToLower().ToCharArray() |> Array.indexed |> Array.sumBy (fun (i, c) -> let shift = 4 * (s.Length - 1 - i) in (parseChar c) <<< shift)
 