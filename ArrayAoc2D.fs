module ArrayAoc2D
open Checked

[<Struct>]
type Point = Point of int*int

type Direction =
    | Up
    | Down
    | Left
    | Right

let delta = function
    | Up -> (0, -1)
    | Down -> (0, 1)
    | Left -> (-1, 0)
    | Right -> (1, 0)

let dims arr =
    Array2D.length1 arr, Array2D.length2 arr

let dump arr =
    let mx, my = dims arr
    for y in 0..(my-1) do
        for x in 0..(mx-1) do
            printf $"{arr[x, y]}"
        printfn ""

let lines2array (lines:string list) =
    let lx, ly = lines[0].Length, lines.Length
    Array2D.init lx ly (fun x y -> lines[y][x])

let inBounds arr (Point (x, y)) = 0 <= x && x < Array2D.length1 arr && 0 <= y && y < Array2D.length2 arr
