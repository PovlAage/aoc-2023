module aoc_2023.Day06
open Utility
let day = 6

type Race = Race of int64 * int64

let distance time chargeTime =
    if not (0 < chargeTime && chargeTime < time) then failwithf $"{chargeTime} not in ]0;{time}["
    (time - chargeTime) * chargeTime
let roots (Race (time, record)) =
    let d = time * time - 4L * record
    (double(time) - sqrt (double(d))) / 2.0 , (double(time) + sqrt (double(d))) / 2.0
let countWaysToWin (Race (time, record)) =
    let r1, r2 = roots (Race (time, record))
    let rr1, rr2 = ceil r1, floor r2
    let rr1 = if rr1 = r1 then int rr1 + 1 else int rr1
    let rr2 = if rr2 = r2 then int rr2 - 1 else int rr2
    rr2 - rr1 + 1

let result = List.map countWaysToWin >> List.reduce (*)
    
let run v =
    use _ = measureElapsed day
    
    if v then
        let testInput = [Race(7, 9) ; Race(15, 40) ; Race(30, 200)]
        let testRace = Race (7, 9)
        verifyq (distance 7 1) 6
        verifyq (distance 7 2) 10
        verifyq (distance 7 3) 12
        verifyq (distance 7 4) 12
        verifyq (distance 7 5) 10
        verifyq (distance 7 6) 6
        verifyq (countWaysToWin testRace) 4
        verifyq (result testInput) 288
    let input = [Race(52, 426) ; Race(94, 1374) ; Race(75, 1279) ; Race(94, 1216)]
    verify (result input) 2449062
    
    if v then
        let testInputB = [Race (71530, 940200)]
        verifyq (result testInputB) 71503
    let inputB = [Race (52947594, 426137412791216L)]
    verify (result inputB) 33149631
