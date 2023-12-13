module aoc_2023.Day13
open Utility
let day = 13

type Line = int list
type Input = { Horizontal: int64[]; Vertical: int64[] }
let parseChunk (lines:string list) =
    assert (lines[0].Length <= 63)
    assert (lines.Length <= 63)
    let mx, my = lines[0].Length - 1, lines.Length - 1
    let vertical, horizontal = Array.create (mx + 1) 0L, Array.create (my + 1) 0L
    for y in 0..my do
        for x in 0..mx do
            if lines[y][x] = '#' then
                horizontal[y] <- horizontal[y] + (1L <<< x)
                vertical[x] <- vertical[x] + (1L <<< y)
    { Horizontal = horizontal; Vertical = vertical }
       
let flip input (x:int, y:int) =
    input.Horizontal[y] <- input.Horizontal[y] ^^^ (1L <<< x)
    input.Vertical[x] <- input.Vertical[x] ^^^ (1L <<< y)
let findReflection (nums:int64[]) (except:int option) =
    let isAxis a =
        {0..min (a-1) (nums.Length-1-a)} |> Seq.forall (fun i -> nums[a-i-1] = nums[a+i])
    {1..nums.Length-1} |> Seq.filter ((<>) (Option.defaultValue -1 except)) |> Seq.tryFind isAxis
let findReflections (excepth:int option, exceptv:int option) inputItem =
    findReflection inputItem.Horizontal excepth, findReflection inputItem.Vertical exceptv
let score (h:int option, v:int option) =
    match h, v with
    | Some c, None -> 100 * c
    | None, Some c -> c
    | unmatched -> failwithf $"Unmatched {unmatched}"

let findReflectionsB inputItem =
    let exceptions = findReflections (None, None) inputItem
    seq {
        for x in {0..inputItem.Vertical.Length-1} do
            for y in {0..inputItem.Horizontal.Length-1} do
                flip inputItem (x, y)
                let result = findReflections exceptions inputItem
                flip inputItem (x, y)
                if result <> (None, None) then yield result
    } |> Seq.head
let resultA = List.sumBy (findReflections (None, None) >> score)
let resultB = List.sumBy (findReflectionsB >> score)

let run v =
    use _ = measureElapsed day
    
    let stestInput = """
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
"""
    let testInput = stestInput |> multiLineToList |> chunkLines |> List.map parseChunk
    if v then
        verifyq (findReflections (None, None) testInput[0]) (None, Some 5)
        verifyq (findReflections (None, None) testInput[1]) (Some 4, None)
        verifyq (resultA testInput) 405
    let input = (inputLines day |> chunkLines) |> List.map parseChunk
    verify (resultA input) 34889
    
    if v then
        verify (resultB testInput) 400
    verify (resultB input) 34224
