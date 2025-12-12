module Day12

open Extensions
open System.IO


type Tiling =
    | Impossible
    | NoInterlock
    | Interlock

type Region =
    { Width: int
      Height: int
      Counts: int list }

    static member analyseTiling presents r =

        let requiredSpaces = List.zip presents r.Counts |> List.sumBy (fun (p, c) -> p * c)

        if requiredSpaces > r.Width * r.Height then
            Impossible
        else
            let width = r.Width / 3
            let height = r.Height / 3

            if List.sum r.Counts <= width * height then
                NoInterlock
            else
                Interlock

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
                    let free =
                        seq {
                            for i in [ 0 .. int h - 1 ] do
                                for j in [ 0 .. int w - 1 ] do
                                    yield { Row = i; Column = j }
                        }
                        |> set

                    { Width = int w
                      Height = int h
                      Counts = xs |> List.map int }
                | _ -> failwith "Invalid region"
            | _ -> failwith "Invalid region"

        parse presents (r :: regions) xs
    | x :: xs ->
        let id = String.substring x 0 (String.length x - 1) |> int
        let shape, xs' = parseShape [] xs

        parse (Set.count shape :: presents) regions xs'


let run =
    printfn "== Day 12 =="

    let presents, regions =
        File.ReadLines "inputs/day12.txt" |> Seq.toList |> parse [] []

    let analysis =
        regions
        |> List.map (Region.analyseTiling presents)
        |> List.countBy id
        |> Map.ofList

    // NB not sure if this is just a property of my problem in particular, but it ends up
    // no complex tiling analysis is needed.

    match Map.tryFind Interlock analysis with
    | Some count -> printfn "%d cases require interlocking...not supported" count
    | None -> printfn "Part 1: %d" (Map.find NoInterlock analysis)

    printfn ""
