module aoc_2023.Day24
open System
open Utility
open Checked
let day = 24

type Vec2 =
    { X: int64; Y: int64 }
    static member Zero = { X=0; Y=0 }
    member this.Scale(c) = { X=c*this.X; Y=c*this.Y }
    member this.Add(v) = { X=this.X+v.X; Y=this.Y+v.Y }
type Vector =
    { X: int64; Y: int64; Z: int64 }
    static member Zero = { X=0; Y=0; Z=0 }
    static member FromList(coors) = let [x;y;z] = coors in { X=x; Y=y; Z=z }
    member this.Scale(c) = { X=c*this.X; Y=c*this.Y; Z=c*this.Z }
    member this.Add(v) = { X=this.X+v.X; Y=this.Y+v.Y; Z=this.Z+v.Z }
    member this.Project2(v): Vec2 = { X=this.X; Y=this.Y }

[<CustomComparison; CustomEquality>]
type Rational =
    { T: bigint; N: bigint }
    static member Zero = { T = bigint.Zero; N = bigint.One }
    static member One = { T = bigint.One; N = bigint.One }
    static member MinusOne = { T = bigint.MinusOne; N = bigint.One }
    static member Normalized(T:bigint, N:bigint) =
        assert (N <> bigint.Zero)
        let d = gcdbig T N
        let r = { T = T/d; N = N/d }
        if sign r.N = -1 then { T = -r.T; N = -r.N } else r
    static member OfInt(i) = { T=i; N=bigint.One }
    member x.IsZero() = x.T = bigint.Zero
    member x.IsNonZero() = not (x.IsZero())
    member x.Add(y) = Rational.Normalized(x.T * y.N + x.N * y.T, x.N * y.N)
    member x.Multiply(y) = Rational.Normalized(x.T * y.T, x.N * y.N)
    member x.Divide(y) = Rational.Normalized(x.T * y.N, x.N * y.T)
    member x.Scale(c) = Rational.Normalized(c * x.T, x.N)
    member x.UnaryMinus() = if x.T = bigint.Zero then x else { T = -x.T; N = x.N }
    override x.ToString() = if x.N = bigint.One then $"{x.T}" else $"{x.T}/{x.N}"
    static member CommonDenominator (q1, q2) = {T = q1.T*q2.N; N = q1.N*q2.N}, {T = q2.T*q1.N; N = q2.N*q1.N }

    interface IComparable<Rational> with
        member this.CompareTo other =
            let this, other = Rational.CommonDenominator (this, other)
            this.T.CompareTo(other.T)
    interface IComparable with
        member this.CompareTo obj =
            match obj with
              | null                 -> 1
              | :? Rational as other -> (this :> IComparable<_>).CompareTo other
              | :? Int64 as other -> (this :> IComparable<_>).CompareTo (Rational.OfInt(other))
              | _                    -> invalidArg "obj" "not a Category"
    interface IEquatable<Rational> with
        member this.Equals other =
            let this, other = Rational.CommonDenominator (this, other)
            this.T = other.T
    override this.Equals obj =
        match obj with
          | :? Rational as other -> (this :> IEquatable<_>).Equals other
              | :? Int64 as other -> (this :> IEquatable<_>).Equals (Rational.OfInt(other))
          | _                    -> false
    override this.GetHashCode () =
        hash this.T

type RatVec =
    { k: int; Elements: Rational list }
    static member Create(rs:Rational list) = {k=rs.Length; Elements=rs}
    member x.IsZero() = x.Elements |> List.forall _.IsZero()
    member x.IsNonZero() = not (x.IsZero())
    member private x.UnaryOp(op) = { x with Elements = x.Elements |> List.map op } 
    member x.Scale(c:Rational) = x.UnaryOp _.Multiply(c)
    member x.UnaryMinus() = x.UnaryOp _.UnaryMinus()
    member x.Add(other) =
        assert(x.k = other.k)
        { x with Elements = List.map2 (fun (x:Rational) y -> x.Add(y)) x.Elements other.Elements }
    override x.ToString() = x.Elements.ToString()
type RatMat =
    { m: int; n: int; ColumnVectors: RatVec list }
    override x.ToString() =
        let rowstring j = String.Join(' ', [0..(x.n-1)] |> Seq.map (fun i -> x.ColumnVectors[i].Elements[j].ToString()))
        String.Join("\n", [0..(x.m-1)] |> Seq.map rowstring)
        
    static member OfColumns(columns:RatVec list) =
        assert(columns |> List.map _.k |> List.distinct |> List.length = 1)
        { m=columns[0].k; n=columns.Length; ColumnVectors=columns }
    member x.GetRow(j) : RatVec =
        { k=x.n; Elements = x.ColumnVectors |> List.map _.Elements[j] }
    member x.RowSwap(i, j) =
        let rec loop i j =
            if j > i then
                loop j i
            else
                assert (i < j)
                let swap (cvx:RatVec) =
                    let ei, ej = cvx.Elements[i], cvx.Elements[j] 
                    { cvx with Elements = cvx.Elements |> List.removeAt j |> List.removeAt i |> List.insertAt i ej |> List.insertAt j ei }
                { x with ColumnVectors = x.ColumnVectors |> List.map swap }
        loop i j
    member x.RowScale(i, c) =
        let scaleColumn col = { col with Elements = let ei = col.Elements[i] in col.Elements |> List.removeAt i |> List.insertAt i (ei.Multiply(c))}
        { x with ColumnVectors = x.ColumnVectors |> List.map scaleColumn }
    member x.AddScaledRow(i, j, c) =
        let processColumn col = { col with Elements = let ei, ej = col.Elements[i], col.Elements[j] in col.Elements |> List.removeAt i |> List.insertAt i (ei.Add(ej.Multiply(c)))}
        { x with ColumnVectors = x.ColumnVectors |> List.map processColumn }
    member z.GaussElimination() =
        // assert (this.m = 2 && this.n = 3)
        let rec loop (z:RatMat) k =
            if k = (z.n - 1) then
                z
            else
                match [k..z.m-1] |> List.tryFind (fun i -> z.ColumnVectors[i].IsNonZero()) with
                | None -> failwithf $"No nonzero column for {z}"
                | Some nonZeroColumnIndex ->
                    let column = z.ColumnVectors[nonZeroColumnIndex]
                    match [k..z.m-1] |> List.tryFind (fun i -> column.Elements[i].IsNonZero()) with
                    | None -> loop z (k + 1)
                    | Some pivotRowIndex ->
                        let toEliminate = column.Elements |> List.indexed |> List.filter (fun (i, r) -> i <> pivotRowIndex && r.IsNonZero())
                        let z = z.RowScale(pivotRowIndex, Rational.One.Divide(column.Elements[pivotRowIndex]))
                        assert (z.ColumnVectors[pivotRowIndex].Elements[k] = Rational.One)
                        let folder (state:RatMat) (i, (r:Rational)) = state.AddScaledRow(i, pivotRowIndex, r.Scale(-1L))
                        let z = toEliminate |> List.fold folder z
                        loop z (k + 1)
        loop z 0
    
type Stone = Stone of Vector * Vector
type Solutions =
    | One of RatVec
    | All of RatVec
    | None
let solve (a:RatVec) (b:RatVec) (c:RatVec) =
    let mat : RatMat = RatMat.OfColumns([a; b; c])
    assert (mat.m = 2 && mat.n = 3)
    let reduced = mat.GaussElimination()
    // printfn $"{mat}"
    // printfn $"-> gauss ->"
    // printfn $"{reduced}"
    let rank = [0..(reduced.m - 1)] |> List.map (fun j -> let r = reduced.GetRow(j) in r.Elements |> List.take (r.k-1)) |> List.filter (fun v -> v |> List.exists _.IsNonZero()) |> List.length
    if rank = 2 then
        let rationalSolution = reduced.ColumnVectors |> List.last
        One rationalSolution
    else
        None
    
let parseLine (line:string) =
    let splitIntComma (s:string) = s.Split(',', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries) |> List.ofArray |> List.map int64
    let sp, sv = line |> split2 '@'
    let p, v = splitIntComma sp |> Vector.FromList, splitIntComma sv |> Vector.FromList
    Stone (p, v)
    
let parse = List.map parseLine

let resultA bounds stones =
    let indexed = stones |> List.indexed
    let distinctPairs = List.allPairs indexed indexed |> List.filter (fun (a, b) -> fst a < fst b) |> List.map (fun (a, b) -> snd a, snd b)
    let hasSolution (sa, sb) =
        let minBound, maxBound = Rational.OfInt(fst bounds), Rational.OfInt(snd bounds)
        let (Stone (ap3, av3)), (Stone (bp3, bv3)) = sa, sb
        let ratvec v : RatVec =
            { k = 2; Elements = [ {T = v.X; N = 1} ; {T = v.Y; N = 1 } ] }

        let ap2, av2, bp2, bv2 = ratvec ap3, ratvec av3, ratvec bp3, ratvec bv3
        // printfn $"HasSolution2 {ap2},{av2},{bp2},{bv2}"
        let c = bp2.Add(ap2.UnaryMinus())
        match solve av2 (bv2.UnaryMinus()) c with
        | One { k=2; Elements=[t1; t2] } when t1 >= Rational.Zero && t2 >= Rational.Zero ->
            let pa = ap2.Add(av2.Scale(t1))
            let pb = bp2.Add(bv2.Scale(t2))
            if pa <> pb then
                printfn $"{pa} <> {pb}"
                assert (pa = pb)
            let x, y = pa.Elements[0], pa.Elements[1]
            if minBound <= x && x <= maxBound && minBound <= y && y <= maxBound then
                // printfn $"Good solution: {x},{y}"
                true
            else
                // printfn $"Solution outside test area: {x},{y}"
                false
        | One { k=2; Elements=[t1; t2] } ->
            // printfn $"Past Solution {t1},{t2}"
            false
        | None -> false
        | unmatched -> failwithf $"Unmatched {unmatched}"
    distinctPairs |> List.filter hasSolution |> List.length
        
        
let run v =
    use _ = measureElapsed day
    let rv (ints:int list) : RatVec =
        { k = ints.Length; Elements = ints |> List.map Rational.OfInt }

    let stestInput = """
19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
"""
    let testInput = stestInput |> multiLineToList |> parse
    if v then
        let m = RatMat.OfColumns([rv [2;-3;-2]; rv [1; -1; 1]; rv [ -1; 2; 2 ]; rv [ 8; -11; -3 ]])
        let mg = m.GaussElimination()
        // printfn $"{mg}"
        verifyq (resultA (7L, 27L) testInput) 2
    let input = inputLines day |> parse
    verify (resultA (200000000000000L, 400000000000000L) input) 14046
    //
    // if v then
    //     verify (resultB testInput) 154
    // verify (resultB input) 0
