module Day07

open System.IO
open Extensions
open System.Collections.Generic


type Side =
    | Left
    | Right

type Splitter =
    { Position: Position
      Left: Position option
      Right: Position option
      Active: bool }

    static member create pos =
        { Position = pos
          Left = None
          Right = None
          Active = false }

    static member add side child splitter =
        match splitter, side with
        | Some parent, Left -> Some { parent with Left = Some child }
        | Some parent, Right -> Some { parent with Right = Some child }
        | None, _ -> None

    static member activate =
        function
        | Some s -> Some { s with Active = true }
        | None -> None


type Beam =
    { Parent: Position
      Side: Side
      Position: Position }

    static member create side p =
        { Parent = p
          Side = side
          Position = p }

    static member split p =
        [ { Parent = p
            Side = Left
            Position = { p with Column = p.Column - 1 } }
          { Parent = p
            Side = Right
            Position = { p with Column = p.Column + 1 } } ]

let down p = { p with Row = p.Row + 1 }

let rec buildManifold maxRow beams splitters =
    match beams with
    | [] -> splitters
    | b :: bs when b.Position.Row > maxRow -> buildManifold maxRow bs splitters
    | b :: bs ->
        match Map.tryFind b.Position splitters with
        | Some sc when not sc.Active ->
            splitters
            |> Map.change b.Parent (Splitter.add b.Side sc.Position)
            |> Map.change sc.Position Splitter.activate
            |> buildManifold maxRow (Beam.split sc.Position @ bs)
        | Some sc ->
            splitters
            |> Map.change b.Parent (Splitter.add b.Side sc.Position)
            |> buildManifold maxRow bs
        | None -> splitters |> buildManifold maxRow ({ b with Position = down b.Position } :: bs)


let part1 = Map.filter (fun _ v -> v.Active) >> Map.count


let findTimelines start splitters =
    let cache = new Dictionary<Position, uint64>()

    let rec f p =
        if cache.ContainsKey p then
            cache.[p]
        else
            let value =
                match Map.find p splitters with
                | { Left = Some lpos; Right = Some rpos } -> f lpos + f rpos
                | { Left = Some lpos; Right = None } -> f lpos + uint64 1
                | { Left = None; Right = Some rpos } -> uint64 1 + f rpos
                | { Left = None; Right = None } -> uint64 2

            cache.Add(p, value)
            value

    f start

let part2 start = findTimelines start


let run =
    printfn "== Day 07 =="

    let splitters =
        File.ReadLines "inputs/day07.txt"
        |> Seq.toList
        |> List.toGrid
        |> Grid.table
        |> Seq.choose (fun kv -> if kv.Value = '.' then None else Some(kv.Key, kv.Value))
        |> Seq.toList
        |> List.sortBy (fun x -> if snd x = 'S' then 0 else 1)
        |> List.map (fst >> Splitter.create)


    let start = splitters |> List.map (fun s -> s.Position) |> List.head
    let maxRow = splitters |> List.map (fun s -> s.Position.Row) |> List.max

    let manifold =
        splitters
        |> List.map (fun s -> s.Position, s)
        |> Map.ofList
        |> buildManifold maxRow [ start |> Beam.create Left ]

    printfn "Part 1: %d" (part1 manifold)
    printfn "Part 2: %d" (part2 start manifold)
    printfn ""
