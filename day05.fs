module Day05

open System.IO
open Extensions

type Range =
    { Start: uint64
      End: uint64 }

    static member count r = r.End - r.Start + uint64 1

let parseRange line =
    let parts = String.split "-" line

    { Start = uint64 parts.[0]
      End = uint64 parts[1] }

let rec mergeRanges merged ranges =
    match ranges with
    | [] -> merged
    | r :: [] -> r :: merged |> List.rev
    | r0 :: r1 :: rs when r1.Start <= r0.End ->
        mergeRanges
            merged
            ({ Start = r0.Start
               End = max r0.End r1.End }
             :: rs)
    | r :: rs -> mergeRanges (r :: merged) rs

let rec parse ranges ids lines =
    match lines with
    | [] -> ranges |> List.sort |> mergeRanges [], List.sort ids
    | "" :: xs -> parse ranges ids xs
    | x :: xs ->
        if String.exists (fun c -> c = '-') x then
            parse (parseRange x :: ranges) ids xs
        else
            parse ranges (uint64 x :: ids) xs

let rec countFresh count ranges ids =
    match ranges, ids with
    | [], _ -> count
    | _, [] -> count
    | r :: _, i :: is when i < r.Start -> countFresh count ranges is
    | r :: rs, i :: _ when i > r.End -> countFresh count rs ids
    | _, _ :: is -> countFresh (count + 1) ranges is

let part1 = countFresh 0

let part2 = List.sumBy Range.count


let checkOverlap ranges =
    List.allPairs ranges ranges
    |> List.filter (fun (a, b) -> a.End >= b.Start && a.Start < b.End && a <> b)
    |> List.map (fun (a, b) ->
        printfn "(%A, %A) (%A, %A)" a.Start a.End b.Start b.End
        (a, b))
    |> List.length


let run =
    printfn "== Day 05 =="

    let ranges, ids = File.ReadLines "inputs/day05.txt" |> Seq.toList |> parse [] []

    printfn "Part 1: %d" (part1 ranges ids)
    printfn "Part 2: %d" (part2 ranges)
