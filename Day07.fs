module aoc_2023.Day07
open Utility
let day = 7

type HandType =
    | FiveKind = 7
    | FourKind = 6
    | FullHouse = 5
    | ThreeKind = 4
    | TwoPair = 3
    | OnePair = 2
    | High = 1
type Bid = int
type Card = Card of int
type Hand = { HandType: HandType; Cards: Card list; Bid: int }

let classify descendingCounts =
    match descendingCounts with
    | [5] -> HandType.FiveKind
    | [4;_] -> HandType.FourKind
    | [3;2] -> HandType.FullHouse
    | [3;1;1] -> HandType.ThreeKind
    | [2;2;1] -> HandType.TwoPair
    | [2;1;1;1] -> HandType.OnePair
    | [1;1;1;1;1] -> HandType.High
    | _ -> failwithf $"Unmatched descendingCounts {descendingCounts}"

let parseHandA (s:string) =
    let parseCard = function
    | 'A' -> Card 14
    | 'K' -> Card 13
    | 'Q' -> Card 12
    | 'J' -> Card 11
    | 'T' -> Card 10
    | c when '2' <= c && c <= '9' -> Card (int (c - '0'))
    | c -> failwithf $"Unmatched card {c}"
    let scards, sbid = split2space s
    let cards = scards.ToCharArray() |> Array.map parseCard |> List.ofArray
    let counts = cards |> List.groupBy id |> List.map (snd >> _.Length) |> List.sortDescending
    { HandType = classify counts; Cards = cards; Bid = int sbid }

let parseHandB (s:string) =
    let parseCard = function
    | 'A' -> Card 14
    | 'K' -> Card 13
    | 'Q' -> Card 12
    | 'J' -> Card 1
    | 'T' -> Card 10
    | c when '2' <= c && c <= '9' -> Card (int (c - '0'))
    | c -> failwithf $"Bad card {c}"
    let scards, sbid = split2space s
    let cards = scards.ToCharArray() |> Array.map parseCard |> List.ofArray
    let jokers, notJokers = cards |> List.partition (fun c -> c = Card 1)
    let counts = match notJokers |> List.groupBy id |> List.map (snd >> _.Length) |> List.sortDescending with
                    | head :: rest -> (head + jokers.Length) :: rest
                    | [] -> [5]
    { HandType = classify counts; Cards = cards; Bid = int sbid }

let compare hand1 hand2 =
    if hand1.HandType <> hand2.HandType then
        hand1.HandType.CompareTo(hand2.HandType)
    else
        let firstDifferentCard = Seq.zip hand1.Cards hand2.Cards |> Seq.tryFind (fun x -> fst x <> snd x)
        match firstDifferentCard with
        | Some (Card c1, Card c2) -> c1.CompareTo(c2)
        | None -> 0

let result (hands:Hand list) =
    hands |> List.sortWith compare |> Seq.indexed |> Seq.sumBy (fun (rank, hand) -> (rank + 1) * hand.Bid)

let run v =
    use _ = measureElapsed day
    
    let stestInput = """
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"""
    
    if v then
        let testInput = stestInput |> multiLineToList |> List.map parseHandA
        verify (result testInput) 6440
    let inputA = (inputLines day) |> List.map parseHandA
    verify (result inputA) 255048101
    
    if v then
        let testInput = stestInput |> multiLineToList |> List.map parseHandB
        verify (result testInput) 5905
    let inputB = (inputLines day) |> List.map parseHandB
    verify (result inputB) 253718286
