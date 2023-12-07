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
type Card = Card of int
type Hand = { HandType: HandType; Cards: Card list; Bid: int }

let parseCardA = function
    | 'A' -> Card 14
    | 'K' -> Card 13
    | 'Q' -> Card 12
    | 'J' -> Card 11
    | 'T' -> Card 10
    | c when '2' <= c && c <= '9' -> Card (int (c - '0'))
    | c -> failwithf $"Unmatched card {c}"

let classify descendingCounts =
    match descendingCounts with
    | [5] -> HandType.FiveKind
    | [4;1] -> HandType.FourKind
    | [3;2] -> HandType.FullHouse
    | [3;1;1] -> HandType.ThreeKind
    | [2;2;1] -> HandType.TwoPair
    | [2;1;1;1] -> HandType.OnePair
    | [1;1;1;1;1] -> HandType.High
    | _ -> failwithf $"Unmatched descendingCounts {descendingCounts}"

let countDescendingA = List.groupBy id >> List.map (snd >> _.Length) >> List.sortDescending

let parse parser counter (s:string) =
    let scards, sbid = split2space s
    let cards = scards.ToCharArray() |> Array.map parser |> List.ofArray
    { HandType = classify (counter cards); Cards = cards; Bid = int sbid }

let parseHandA = parse parseCardA countDescendingA

let parseHandB =
    let countDescendingB cards =
        let jokers, notJokers = cards |> List.partition ((=) (Card 1))
        match countDescendingA notJokers with
        | head :: rest -> (head + jokers.Length) :: rest
        | [] -> [5]
    parse (parseCardA >> (fun c -> if c = Card 11 then Card 1 else c)) countDescendingB

let compareHands hand1 hand2 =
    match hand1.HandType.CompareTo(hand2.HandType) with
    | 0 ->
        Seq.zip hand1.Cards hand2.Cards |>
        Seq.map (fun (Card c1, Card c2) -> c1.CompareTo(c2)) |>
        Seq.tryFind ((<>) 0) |>
        Option.defaultValue 0
    | c -> c

let result (hands:Hand list) =
    hands |> List.sortWith compareHands |> Seq.indexed |> Seq.sumBy (fun (index, hand) -> (index + 1) * hand.Bid)

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
