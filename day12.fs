module Day12

open Extensions
open System.IO


type Present =
    { Id: int
      Pos: Position
      NumSpaces: int
      NumShapes: int
      Shapes: Set<Position> array}

    static member at pos i p =
        p.Shapes[i] |> Seq.map (fun x -> x + pos) |> set


type Region =
    { Width: int
      Height: int
      Counts: int list }

    static member spaces r =
        seq {
            for i in [1..r.Height-2] do
                for j in [1..r.Width-2] do
                    yield {Row=i; Column=j}
        } |> set

    static member hasSpace presents r =
        let spaces = r.Width * r.Height
        let required =
            List.zip r.Counts presents
            |> List.sumBy(fun (c, p) -> c * p.NumSpaces)
        
        required <= spaces

let printShape s =
    for r in [-1..1] do
        for c in [-1..1] do
            if Set.contains {Row=r; Column=c} s then
                printf "#"
            else
                printf "."
        
        printfn ""
    printfn ""

let rec parse presents regions lines =
    let toShape list =
        let grid = list |> List.rev |> List.toGrid

        let center =
            { Row = grid.Rows / 2
              Column = grid.Columns / 2 }

        grid
        |> Grid.choose (function
            | '#' -> Some true
            | _ -> None)
        |> Grid.keys
        |> List.map (fun p -> p - center)
        |> set

    let rotate90 = Seq.map (fun p -> { Column = -p.Row; Row = p.Column }) >> set

    let rotate180 = Seq.map (fun p -> { Column = -p.Column; Row = -p.Row }) >> set

    let rotate270 = Seq.map (fun p -> { Column = p.Row; Row = -p.Column }) >> set

    let flipH = Seq.map (fun p -> { p with Column = -p.Column }) >> set

    let flipV = Seq.map (fun p -> { p with Row = -p.Row }) >> set

    let transforms = [id; flipH; flipV;
                      rotate90; rotate90 >> flipH; rotate90 >> flipV;
                      rotate180; rotate180 >> flipH; rotate180 >> flipV;
                      rotate270; rotate270 >> flipH; rotate270 >> flipV]

    let rec parseShape shape lines =
        match lines with
        | [] -> toShape shape, []
        | "" :: xs -> toShape shape, xs
        | x :: xs -> parseShape (x :: shape) xs

    match lines with
    | [] -> presents |> List.rev, regions |> List.rev
    | x :: xs when String.contains x 'x' ->
        let r =
            match String.split ":" x with
            | size :: counts :: [] ->
                match String.split "x" size, String.splitRemoveEmpty " " counts with
                | w :: h :: [], xs ->
                    { Width = int w
                      Height = int h
                      Counts = xs |> List.map int }
                | _ -> failwith "Invalid region"
            | _ -> failwith "Invalid region"

        parse presents (r :: regions) xs
    | x :: xs ->
        let id = String.substring x 0 (String.length x - 1) |> int
        let shape, xs' = parseShape [] xs
        let shapes = transforms |> List.map (fun t -> t shape) |> set |> Seq.toArray
        let p =
            { Id = id
              NumSpaces = Set.count shape
              NumShapes = Array.length shapes
              Pos = { Row = 0; Column = 0 }
              Shapes = shapes }

        parse (p :: presents) regions xs'


let canFit p r =
    let spaces = Region.spaces r
    

let run =
    printfn "== Day 12 =="

    let presents, regions =
        File.ReadLines "inputs/day12.txt" |> Seq.toList |> parse [] []

    presents.[0].Shapes |> Seq.iter printShape

    printfn "%A" (regions |> List.map (Region.hasSpace presents))

    printfn "Part 1: %d" 0
    printfn "Part 2: %d" 0
    printfn ""

