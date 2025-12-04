module Day04

open System
open System.IO
open Extensions


let countNeighbors rolls p =
    p
    |> Position.neighbors8
    |> Seq.sumBy (fun p -> if Grid.contains p rolls then 1 else 0)


let notReachable rolls p =
    if countNeighbors rolls p >= 4 then Some p else None

let removeReachable rolls =
    Grid.chooseKeys (notReachable rolls) rolls

let part1 rolls =
    Grid.count rolls - Grid.count (removeReachable rolls)

let rec removeAllReachableRolls c rolls =
    let rolls' = removeReachable rolls
    let c' = Grid.count rolls'
    if c = c' then rolls' else removeAllReachableRolls c' rolls'

let part2 rolls =
    let count = Grid.count rolls
    count - Grid.count (removeAllReachableRolls count rolls)

let run =
    printfn "== Day 04 =="

    let rolls =
        File.ReadLines("inputs/day04.txt")
        |> Seq.toList
        |> List.toGrid
        |> Grid.filter (set [ '@' ])

    printfn "Part 1: %A" (part1 rolls)
    printfn "Part 2: %A" (part2 rolls)
