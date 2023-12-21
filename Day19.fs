module aoc_2023.Day19
open System
open Interval
open Utility
let day = 19
type WorkflowName = string
type Category =
    | X = 0
    | M = 1
    | A = 2
    | S = 3
type Condition = 
    | Smaller of Category * int
    | Greater of Category * int
    | Empty
type Result =
    | Accept
    | Reject
type Effect =
    | NextWorkflow of WorkflowName
    | Result of Result
type Rule = { Condition: Condition; Effect: Effect }
type Workflow = { Name: WorkflowName; Rules: Rule list }
type Rating = Map<Category, int>
type Spec = { Workflows: Map<WorkflowName, Workflow>; SourceLookup: Map<WorkflowName, Workflow>; Ratings: Rating list }
let parseCategory = function
    | 'x' -> Category.X
    | 'm' -> Category.M
    | 'a' -> Category.A
    | 's' -> Category.S
    | c -> failwith $"Unmatched {c}"
let parseWorkflowLine (line:string) =
    let parseCondition (s:string) =
        let cat = parseCategory s[0]
        let rel = s[1]
        let value = int (s.Substring(2))
        match rel with
        | '<' -> Smaller (cat, value) 
        | '>' -> Greater (cat, value)
        | _ -> failwith $"Unmatched {rel}"
    let parseEffect = function
        | "A" -> Result Accept
        | "R" -> Result Reject
        | n -> NextWorkflow n
    let parseRule (s:string) =
        let condition, effect = s |> split2 ':'
        { Condition = parseCondition condition; Effect = parseEffect effect }
    let splits = line.Split([|'{'; '}'; ','|], StringSplitOptions.RemoveEmptyEntries)
    let name = splits[0]
    let finalEffect = parseEffect (splits |> Array.last)
    let rules = (splits[1..(splits.Length-2)] |> List.ofArray |> List.map parseRule) @ [{ Condition = Empty; Effect = finalEffect }]
    { Name = name; Rules = rules }
let parseRatingsLine (line:string) =
    let values = line.Substring(1, line.Length - 2).Split(',')
    let parseValue s =
        let cat, v = s |> split2 '='
        (parseCategory cat[0]), int v
    values |> Seq.map parseValue |> Map.ofSeq
let parse lines =
    let [workflowLines; ratingLines] = chunkLines lines
    let workflows = workflowLines |> List.map parseWorkflowLine |> Seq.map (fun wf -> (wf.Name, wf)) |> Map.ofSeq
    let ratings = ratingLines |> List.map parseRatingsLine
    let hasNextWorkflow nextName wf = wf.Rules |> List.exists (fun r -> r.Effect = NextWorkflow nextName)
    let getSource name = workflows.Values |> Seq.filter (hasNextWorkflow name) |> Seq.exactlyOne
    { Workflows = workflows
      SourceLookup = workflows.Keys |> Seq.filter ((<>) "in") |> Seq.map getSource |> Seq.map (fun wf -> (wf.Name, wf)) |> Map.ofSeq
      Ratings = ratings }

[<TailCall>]
let rec doesWorkflowApplyLoop workflows rating currentWorkflow =
    let wf = workflows |> Map.find currentWorkflow
    let sat condition =
        match condition with
        | Smaller(category, i) -> rating |> Map.find category < i
        | Greater(category, i) -> rating |> Map.find category > i
        | Empty -> true
    let firstRule = wf.Rules |> List.find (fun r -> sat r.Condition)
    match firstRule.Effect with
    | Result result -> result
    | NextWorkflow next -> doesWorkflowApplyLoop workflows rating next
let itemResultA workflows item =
    match doesWorkflowApplyLoop workflows item "in" with
    | Accept -> item.Values |> Seq.sum
    | Reject -> 0
type Block = Block of Interval list
let fullBlock = Block (List.replicate 4 (Interval.All()))
let blockIntersect (Block x) (Block y) = Block (List.map2 intersect x y)
let blockForCondition = function
    | Smaller (cat, i) -> [0..3] |> List.map (fun c -> if enum<Category> c = cat then Interval.Smaller(i) else Interval.All()) |> Block
    | Greater (cat, i) -> [0..3] |> List.map (fun c -> if enum<Category> c = cat then Interval.Greater(i) else Interval.All()) |> Block
    | Empty -> fullBlock
let blockForNotCondition = function
    | Smaller (cat, i) -> [0..3] |> List.map (fun c -> if enum<Category> c = cat then Interval.Greater(i-1) else Interval.All()) |> Block
    | Greater (cat, i) -> [0..3] |> List.map (fun c -> if enum<Category> c = cat then Interval.Smaller(i+1) else Interval.All()) |> Block
    | Empty -> fullBlock
let rec getCondition spec cache (itemName, itemIndex) =
    match itemName, itemIndex with
    | "in", 0 -> fullBlock
    | _, 0 ->
        let wfSource = spec.SourceLookup |> Map.find itemName
        let indexOfRule = wfSource.Rules |> List.findIndex (fun r -> r.Effect = NextWorkflow itemName)
        let cond = wfSource.Rules[indexOfRule].Condition
        blockIntersect (blockForCondition cond) (getCondition spec cache (wfSource.Name, indexOfRule))
    | _, k ->
        let wfCurrent = spec.Workflows[itemName]
        let notBlocks = wfCurrent.Rules |> List.take k |> List.map _.Condition |> List.map blockForNotCondition |> List.reduce blockIntersect
        blockIntersect notBlocks (getCondition spec cache (itemName, 0))
    
let resultA input =
    input.Ratings |> List.sumBy (itemResultA input.Workflows)
let resultB input =
    let accepts = input.Workflows.Values |> Seq.collect (fun wf -> wf.Rules |> List.indexed |> List.filter (fun (i, r) -> r.Effect = Result Accept) |> List.map (fun (i, r) -> (wf.Name, i)))
    let cache = System.Collections.Generic.Dictionary<_, _>()
    let blocks = accepts |> Seq.map (fun (name, i) -> getCondition input cache (name, i)) |> List.ofSeq
    -1L
let run v =
    use _ = measureElapsed day
    
    let stestInput = """
px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
"""
    if v then
        let testInput = stestInput |> multiLineToList |> parse
        verify (resultA testInput) 19114
    let input = inputLines day |> parse
    verify (resultA input) 487623
    
    if v then
        let testInput = stestInput |> multiLineToList |> parse
        verify (resultB testInput) 167409079868000L
    // let input = inputLines day |> parseB
    // verify (result input) 54662804037719L
