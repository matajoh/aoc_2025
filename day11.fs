module Day11

open System.IO
open Algorithms
open Extensions

let parseEdges line =
    match String.split ":" line with
    | src :: dsts :: [] -> String.splitRemoveEmpty " " dsts |> List.map (fun dst -> src, dst)
    | _ -> failwith "Invalid graph"

let part1 serverRack = Graph.countPaths "you" "out" serverRack

let part2 serverRack =
    let fft_dac = Graph.countPaths "fft" "dac" serverRack
    let dac_fft = Graph.countPaths "dac" "fft" serverRack

    let path0 =
        if fft_dac > 0UL then
            let svr_fft = Graph.countPaths "svr" "fft" serverRack
            let dac_out = Graph.countPaths "dac" "out" serverRack
            svr_fft * fft_dac * dac_out
        else
            0UL

    let path1 =
        if dac_fft > 0UL then
            let svr_dac = Graph.countPaths "svr" "dac" serverRack
            let fft_out = Graph.countPaths "fft" "out" serverRack
            svr_dac * dac_fft * fft_out
        else
            0UL

    path0 + path1

let run =
    printfn "== Day 11 =="

    let serverRack =
        File.ReadLines "inputs/day11.txt"
        |> Seq.collect parseEdges
        |> Graph.fromList true

    printfn "Part 1: %d" (part1 serverRack)
    printfn "Part 2: %d" (part2 serverRack)
    printfn ""
