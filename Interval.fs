module Interval
open Checked
type Interval =
    { Lower: int; Upper: int}
    static member Empty() = { Lower = System.Int32.MaxValue; Upper = System.Int32.MinValue }
    static member All() = { Lower = System.Int32.MinValue; Upper = System.Int32.MaxValue }
    static member Smaller(i) = { Lower = System.Int32.MinValue; Upper = i - 1 }
    static member Greater(i) = { Lower = i + 1; Upper = System.Int32.MaxValue }
    member this.IsEmpty() = this.Lower = System.Int32.MaxValue
    member this.IsAll() = this.Lower = System.Int32.MinValue && this.Upper = System.Int32.MaxValue

let rec intersect (i:Interval) (j:Interval) =
    if i.IsEmpty() || j.IsEmpty() then
        Interval.Empty()
    elif j.Lower < i.Lower then
        intersect j i
    elif i.IsAll() then
        j
    elif j.IsAll() then
        i
    else
        assert (i.Lower <= j.Lower)
        if j.Lower > i.Upper then
            Interval.Empty()
        elif j.Upper <= i.Upper then
            j
        else
            { Lower = j.Lower; Upper = i.Upper }
