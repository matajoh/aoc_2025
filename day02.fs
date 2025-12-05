module Day02

open System
open System.IO

let parseRange (range: String) =
    let parts = range.Split('-')
    let start_index = parts.[0] |> uint64
    let end_index = parts.[1] |> uint64
    start_index, end_index


let parse (ranges: String) =
    let parts = ranges.Split(',')
    parts |> Array.map parseRange |> Array.toList


let getFactors value =
    seq { 1 .. value / 2 } |> Seq.filter (fun x -> value % x = 0)


let isRepeat str length =
    let count = String.length str / length
    let substr = str.Substring(0, length)
    str = String.replicate count substr


let rec countRepeats all value last count =
    if value > last then
        count
    else
        let str = value.ToString()
        let length = String.length str

        let isRepeat =
            if all then
                length
                |> getFactors
                |> Seq.fold (fun acc factor -> acc || isRepeat str factor) false
            else
                str.Substring(0, length / 2) = str.Substring(length / 2)

        let count' = if isRepeat then count + value else count
        countRepeats all (value + 1UL) last count'


let part1 ranges =
    ranges |> Seq.sumBy (fun (first, last) -> countRepeats false first last 0UL)


let part2 ranges =
    ranges |> Seq.sumBy (fun (first, last) -> countRepeats true first last 0UL)


let run =
    printfn "== Day 02 =="

    let ranges = File.ReadAllText "inputs/day02.txt" |> parse

    printfn "Part 1: %d" (part1 ranges)
    printfn "Part 2: %d" (part2 ranges)
