module Day08

open System.IO
open Algorithms
open Extensions

type Vec3 =
    { X: uint64
      Y: uint64
      Z: uint64 }

    static member lengthSquared a b =
        let dx = a.X - b.X
        let dy = a.Y - b.Y
        let dz = a.Z - b.Z
        dx * dx + dy * dy + dz * dz

    static member parse line =
        match String.split "," line |> List.map uint64 with
        | x :: y :: z :: [] -> { X = x; Y = y; Z = z }
        | _ -> failwith ("Invalid vector: " + line)


let computeDistances vs =
    let length = List.length vs

    seq {
        for i in 0 .. length - 1 do
            for j in i + 1 .. length - 1 do
                yield i, j, Vec3.lengthSquared vs.[i] vs.[j]
    }


let rec buildCircuits count distances circuits =
    match distances, count with
    | _, 0 -> DisjointSet.sets circuits
    | [], _ -> failwith "Not enough junction boxes"
    | (i, j, _) :: ds, _ ->
        DisjointSet.union i j circuits
        buildCircuits (count - 1) ds circuits


let part1 count circuits distances =
    circuits
    |> buildCircuits count distances
    |> List.map Set.count
    |> List.sortDescending
    |> List.take 3
    |> List.fold (*) 1


let rec connectAll distances circuits =
    match distances with
    | [] -> failwith "Unable to joint all junction boxes"
    | (i, j, _) :: ds ->
        DisjointSet.union i j circuits

        if DisjointSet.count circuits = 1 then
            i, j
        else
            connectAll ds circuits


let part2 junctionBoxes circuits distances =
    let i, j = connectAll distances circuits
    let x = List.item i junctionBoxes
    let y = List.item j junctionBoxes
    x.X * y.X

let run =
    printfn "== Day 08 =="

    let junctionBoxes =
        File.ReadLines "inputs/day08.txt" |> Seq.map Vec3.parse |> Seq.toList

    let distances =
        junctionBoxes
        |> computeDistances
        |> Seq.toList
        |> List.sortBy (fun (_, _, d) -> d)

    let count = 1000
    let circuits = DisjointSet.create (List.length junctionBoxes)

    printfn "Part 1: %d" (part1 count circuits distances)
    printfn "Part 2: %d" (part2 junctionBoxes circuits (List.skip count distances))
    printfn ""
