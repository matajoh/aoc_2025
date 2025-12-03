module Day03

open System.IO
open System.Collections.Generic

let toDigit c = int c - int '0' |> bigint

let maxJoltage limit batteries =
    let cache = new Dictionary<int * int, bigint>()
    let length = String.length batteries

    let rec f x i =
        let key = x, i

        if cache.ContainsKey key then
            cache.[key]
        else
            let value =
                match x with
                | _ when i = length -> bigint 0
                | 1 -> batteries.[i..] |> Seq.max |> toDigit
                | _ when i + x = length -> batteries.Substring i |> bigint.Parse
                | _ ->
                    let d = toDigit batteries.[i] * bigint 10 ** (x - 1) + f (x - 1) (i + 1)
                    max (f x (i + 1)) d

            cache.Add(key, value)
            value

    f limit 0


let part1 bank = bank |> Seq.sumBy (maxJoltage 2)

let part2 bank = bank |> Seq.sumBy (maxJoltage 12)

let run =
    printfn "== Day 03 =="

    let bank = File.ReadLines("inputs/day03.txt") |> Seq.toList

    printfn "Part 1: %A" (part1 bank)
    printfn "Part 2: %A" (part2 bank)
