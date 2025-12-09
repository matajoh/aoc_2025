module Day09

open System.IO
open Extensions


type Line =
    | Horizontal of uint64 * uint64 * uint64
    | Vertical of uint64 * uint64 * uint64


type Rectangle =
    { Left: uint64
      Top: uint64
      Right: uint64
      Bottom: uint64
      Area: uint64 }

    static member create ((x0, y0),(x1, y1)) =
        if x0 > x1 then
            if y0 > y1 then
                { Left = x1
                  Right = x0
                  Top = y1
                  Bottom = y0
                  Area = (x0 - x1 + 1UL) * (y0 - y1 + 1UL) }
            else
                { Left = x1
                  Right = x0
                  Top = y0
                  Bottom = y1
                  Area = (x0 - x1 + 1UL) * (y1 - y0 + 1UL) }
        else if y0 > y1 then
            { Left = x0
              Right = x1
              Top = y1
              Bottom = y0
              Area = (x1 - x0 + 1UL) * (y0 - y1 + 1UL) }
        else
            { Left = x0
              Right = x1
              Top = y0
              Bottom = y1
              Area = (x1 - x0 + 1UL) * (y1 - y0 + 1UL) }

    static member inside r (x, y) =
        not (x <= r.Left || x >= r.Right || y <= r.Top || y >= r.Bottom)

    static member intersects r line =
        match line with
        | Horizontal(_, _, y) when y <= r.Top || y >= r.Bottom -> false
        | Vertical(_, _, x) when x <= r.Left || x >= r.Right -> false
        | Horizontal(x0, x1, _) when x0 >= r.Right || x1 <= r.Left -> false
        | Vertical(y0, y1, _) when y0 >= r.Bottom || y1 <= r.Top -> false
        | _ -> true


let toTuple list =
    match list with
    | a :: b :: [] -> a, b
    | _ -> failwith "Invalid tuple"

let part1 = Seq.fold (fun a r -> max a r.Area) 0UL

let findLargestInternalRectangle lines maxArea r =
    if r.Area <= maxArea then
        maxArea
    else
        lines
        |> List.fold (fun invalid x -> invalid || Rectangle.intersects r x) false
        |> function
            | true -> maxArea
            | false -> r.Area


let toLine ((x0, y0), (x1, y1)) =
    if x0 = x1 then
        if y0 < y1 then
            Vertical(y0, y1, x0)
        else
            Vertical(y1, y0, x0)
    elif y0 = y1 then
        if x0 < x1 then
            Horizontal(x0, x1, y0)
        else
            Horizontal(x1, x0, y0)
    else
        failwith "Invalid edge"

let part2 corners =
    let lines = corners @ [ List.head corners ] |> List.pairwise |> List.map toLine
    Seq.fold (findLargestInternalRectangle lines) 0UL


let run =
    printfn "== Day 09 =="

    let corners =
        File.ReadAllLines "inputs/day09.txt"
        |> Seq.map (String.split "," >> List.map uint64 >> toTuple)
        |> Seq.toList

    let rectangles =
        corners
        |> List.selfPairs
        |> Seq.map Rectangle.create
        |> Seq.toList

    printfn "Part 1: %d" (part1 rectangles)
    printfn "Part 2: %d" (part2 corners rectangles)
    printfn ""
