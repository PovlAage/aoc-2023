module aoc_2023.Day03
open System
open Utility
let day = 3

type Point = int * int
type Number = int * int * Point
type Symbol = char * Point
type Element =
    | Number of Number
    | Symbol of Symbol
type Diagram = Element list
let parse lines =
    let reBlanks = System.Text.RegularExpressions.Regex(@"^\.+")
    let reNumber = System.Text.RegularExpressions.Regex(@"^\d+")
    let reSymbol = System.Text.RegularExpressions.Regex(@"^.")
    let parseLine lineNo (line:string) =
        let rec loop acc pos =
            let point = (lineNo, pos)
            let tryParseBlanks =
                let m = reBlanks.Match(line.Substring(pos))
                if m.Success then
                    Some m.Length
                else
                    None
            let tryParseNumber =
                let m = reNumber.Match(line.Substring(pos))
                if m.Success then
                    Some (m.Length, (Number (Int32.Parse(m.Value), m.Length, point)))
                else
                    None
            let tryParseSymbol =
                let m = reSymbol.Match(line.Substring(pos))
                if m.Success then
                    Some (m.Length, (Symbol (m.Value[0], point)))
                else
                    None
            if pos = line.Length then
                List.rev acc
            else
                match tryParseBlanks with
                | Some l -> loop acc (pos + l)
                | None -> match tryParseNumber with
                            | Some (len, n) -> loop (n :: acc) (pos + len)
                            | None -> match tryParseSymbol with
                                        | Some (len, s) -> loop (s :: acc) (pos + len)
                                        | None -> failwithf $"Could not match at {line.Substring(pos)}"
        loop [] 0
    lines |> List.indexed |> List.map (fun (lineNo, line) -> parseLine lineNo line) |> List.concat

let resultA input =
    let numbers = input |> List.choose (fun e -> match e with Number _ -> Some e | _ -> None)
    let symbolPoints = input |> List.choose (fun e -> match e with Symbol (s, p) -> Some p | _ -> None) |> Set.ofSeq
    let hasSymbolNeighbour (Number (n, len, point)) =
        let y, x = point
        let neighbourPoints =
            [for xx in [x-1..x+len] do (y-1, xx)] @
            [(y, x-1); (y, x+len)] @
            [for xx in [x-1..x+len] do (y+1, xx)] |> Set.ofList
        not (Set.isEmpty (Set.intersect symbolPoints neighbourPoints))
    numbers |> List.filter hasSymbolNeighbour |> List.sumBy (fun (Number (n, _, _)) -> n)      
let resultB input =
    let numbers = input |> List.choose (fun e -> match e with Number _ -> Some e | _ -> None)
    let isGearNeighbour gearPoint (Number (n, len, point)) =
        let y, x = point
        let neighbourPoints =
            [for xx in [x-1..x+len] do (y-1, xx)] @
            [(y, x-1); (y, x+len)] @
            [for xx in [x-1..x+len] do (y+1, xx)] |> Set.ofList
        Set.contains gearPoint neighbourPoints
    let getNeighbours gearPoint = numbers |> List.filter (isGearNeighbour gearPoint) |> List.map (fun (Number (n, len, point)) -> n)
    let gearsWithNeighbours =
        input |>
        List.choose (fun e -> match e with Symbol ('*', p) -> Some p | _ -> None) |>
        List.map (fun p -> (p, getNeighbours p)) |>
        List.filter (fun (p, ns) -> List.length ns = 2)
    gearsWithNeighbours |> List.sumBy (snd >> (List.reduce (*)))
let run v =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    printfn $"day {day}"
    
    let stestInput = """
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"""
    let testInput = stestInput |> multiLineToList |> parse
    if v then
        verify (resultA testInput) 4361

    let input = (inputLines day) |> parse
    verify (resultA input) 531932

    if v then
        verify (resultB testInput) 467835

    verify (resultB input) 73646890

    
    printfn $"day {day} elapsed {sw.ElapsedMilliseconds} ms"
