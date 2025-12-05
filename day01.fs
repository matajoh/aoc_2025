module Day01

open System
open System.IO

type Turn =
    | Left of int
    | Right of int

[<Literal>]
let DialMax = 100

let parseTurn (str: String) =
    if str.StartsWith('L') then
        Left(str.Substring(1) |> int)
    else
        Right(str.Substring(1) |> int)

let turnLeft1 x v c =
    let dv = x % DialMax
    match dv, v with
    | 0, _ -> v, c
    | _ when dv = v -> 0, c + 1
    | _ when dv > v -> v + 100 - dv, c
    | _ -> v - dv, c

let turnRight1 x v c =
    let v' = (v + x) % DialMax
    if v' = 0 then
        v', c + 1
    else
        v', c

let turnLeft2 x v c =
    let dv, c' = x % DialMax, c + x / DialMax

    match dv, v with
    | 0, _ -> v, c'
    | dv, 0 -> DialMax - dv, c'
    | dv, v when dv > v -> v - dv + DialMax, c' + 1
    | dv, v when dv = v -> 0, c' + 1
    | _ -> v - dv, c'

let turnRight2 x v c =
    let dv, c' = x % DialMax, c + x / DialMax

    match dv, v with
    | 0, _ -> v, c'
    | dv, 0 -> dv, c'
    | dv, v when v + dv > DialMax -> v + dv - DialMax, c' + 1
    | dv, v when v + dv = DialMax -> 0, c' + 1
    | _ -> v + dv, c'

let turn mode t =
    if mode = 1 then
        match t with
        | Left x -> turnLeft1 x
        | Right x -> turnRight1 x
    else
        match t with
        | Left x -> turnLeft2 x
        | Right x -> turnRight2 x


let rec countZeros turn turns (value, count) =
    match turns with
    | [] -> count
    | t :: ts ->
        turn t value count |> countZeros turn ts

let part1 turns = countZeros (turn 1) turns (50, 0)

let part2 turns = countZeros (turn 2) turns (50, 0)

let run =
    printfn "== Day 01 =="

    let turns = File.ReadLines "inputs/day01.txt" |> Seq.map parseTurn |> Seq.toList

    printfn "Part 1: %d" (part1 turns)
    printfn "Part 2: %d" (part2 turns)
