module aoc_2023.Day02
open System
open Utility
let day = 2

type Color = Red | Green | Blue
type Hand = Hand of Map<Color, int>
type Game = Game of int * (Hand list)
type Bag = Bag of Map<Color, int>

let parseGame (line:string) =
    let opts = StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries
    let sgameNo, shands = line |> split2 ':' 
    let gameNo = sgameNo |> split2space |> snd |> int
    let parseColorCount (s:string) =
        let scount, scol = s |> split2space
        let count = int scount
        let color = match scol with
                    | "red" -> Red
                    | "green" -> Green
                    | "blue" -> Blue
                    | _ -> failwithf $"invalid {scol}"
        color, count
    let parseHand (shand:string) =
        let h = shand.Split(',', opts) |> Array.map (_.Trim() >> parseColorCount) |> Map.ofArray
        Hand h
    Game (gameNo, shands.Split(';', opts) |> List.ofArray|> List.map parseHand)
let parseInput = List.map parseGame

let isPossible (Bag bag) (Game (no, hands)) =
    hands |> List.forall (fun (Hand h) -> mapIsSubSet h bag)

let minBag (Game (no, hands)) =
    hands |> List.map (fun (Hand h) -> h) |> List.fold mapUnion Map.empty        

let power (cubes:Map<Color,int>) = cubes |> Map.values |> Seq.reduce (*)
let resultA games =
    let bag = Bag ([(Red, 12); (Green, 13); (Blue, 14)] |> Map.ofList)
    games |> List.filter (isPossible bag) |> List.sumBy (fun (Game (no, hands)) -> no)
let resultB games =
    games |> List.sumBy (minBag >> power)

let run v =
    use _ = measureElapsed day
    
    let stestInput = """
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"""
    let testInput = stestInput |> multiLineToList |> parseInput

    if v then
        verify (resultA testInput) 8

    let input = inputLines day |> parseInput 
    verify (resultA input) 2617

    if v then
        verify (resultB testInput) 2286

    let input = inputLines day |> parseInput 
    verify (resultB input) 59795
