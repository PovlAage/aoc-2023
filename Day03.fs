module aoc_2023.Day03
open System
open Utility
let day = 3

type Point = int * int
type Element =
    | Number of int * int * Point
    | Symbol of char * Point
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
                if m.Success then Some m.Length else None
            let tryParseElement =
                let m = reNumber.Match(line.Substring(pos))
                if m.Success then
                    Some (m.Length, (Number (int m.Value, m.Length, point)))
                else
                    let m = reSymbol.Match(line.Substring(pos))
                    if m.Success then Some (m.Length, (Symbol (m.Value[0], point))) else None
            if pos = line.Length then
                List.rev acc
            else
                match tryParseBlanks with
                | Some l -> loop acc (pos + l)
                | None -> match tryParseElement with
                            | Some (len, e) -> loop (e :: acc) (pos + len)
                            | None -> failwithf $"Could not match at {line.Substring(pos)}"
        loop [] 0
    lines |> List.indexed |> List.map (fun (lineNo, line) -> parseLine lineNo line) |> List.concat

let isNeighbour p e =
    let py, px = p
    match e with
    | Number(_, len, (y, x)) -> y - 1 <= py && py <= y + 1 && x - 1 <= px && px <= x + len
    | Symbol(c, (y, x)) -> y - 1 <= py && py <= y + 1 && x - 1 <= px && px <= x + 1

let resultA input =
    let numbers = input |> List.choose (fun e -> match e with Number _ -> Some e | _ -> None)
    let symbolPoints = input |> List.choose (fun e -> match e with Symbol (s, p) -> Some p | _ -> None) |> Set.ofSeq
    let hasSymbolNeighbour e =
        symbolPoints |> Set.exists (fun sp -> isNeighbour sp e)
    numbers |> List.filter hasSymbolNeighbour |> List.sumBy (fun (Number (n, _, _)) -> n)
    
let resultB input =
    let numbers = input |> List.choose (fun e -> match e with Number _ -> Some e | _ -> None)
    let getNeighbours gearPoint = numbers |> List.filter (isNeighbour gearPoint) |> List.map (fun (Number (n, _, _)) -> n)
    let gearsWithNeighbours =
        input |>
        List.choose (function Symbol ('*', p) -> Some (getNeighbours p) | _ -> None) |>
        List.filter (fun ns -> ns.Length = 2)
    gearsWithNeighbours |> List.sumBy (List.reduce (*))

let run v =
    use _ = measureElapsed day
    
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
