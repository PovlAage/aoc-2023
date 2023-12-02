module aoc_2023.Day01
open System
open Utility
let day = 1

let resultA lines =
    let re = System.Text.RegularExpressions.Regex(@"0|1|2|3|4|5|6|7|8|9")
    let cvalue (line:string) =
        let m = re.Matches(line)
        let first = m[0].Value
        let last = m[m.Count - 1].Value
        Int32.Parse(first + last)
    lines |> List.sumBy cvalue

let resultB lines =
    let re = System.Text.RegularExpressions.Regex(@"(?=(0|1|2|3|4|5|6|7|8|9|one|two|three|four|five|six|seven|eight|nine))")
    let cvalue (line:string) =
        let digit = function
            | "one" -> 1
            | "two" -> 2
            | "three" -> 3
            | "four" -> 4
            | "five" -> 5
            | "six" -> 6
            | "seven" -> 7
            | "eight" -> 8
            | "nine" -> 9
            | d -> Int32.Parse(d)
            | s -> failwithf $"unexpected {s}"
        let m = re.Matches(line)
        let first = string(digit m[0].Groups[1].Value)
        let last = string(digit m[m.Count - 1].Groups[1].Value)
        // printfn $"{line}\t{first}+{last}"
        Int32.Parse(first + last)
    lines |> List.sumBy cvalue

let run v =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    printfn $"day {day}"
    
    if v then
        let testInput = """
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
"""
        verify (resultA (multiLineToList testInput)) 142

    let input = inputLines day
    verify (resultA input) 55108

    if v then
        let testInput = """
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
"""
        verify (resultB (multiLineToList testInput)) 281
    
    verify (resultB input) 56324

    printfn $"day {day} elapsed {sw.ElapsedMilliseconds} ms"
