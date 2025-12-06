module Day06

open System
open System.IO
open Extensions


type Problem =
    | Sum of uint64 list
    | Product of uint64 list


let toProblem (_, column) =
    match List.rev column with
    | (_, "+") :: xs -> Sum(xs |> List.map (fun (_, x) -> uint64 x))
    | (_, "*") :: xs -> Product(xs |> List.map (fun (_, x) -> uint64 x))
    | _ -> failwith "Invalid operator"


let compute p =
    match p with
    | Sum xs -> List.sum xs
    | Product xs -> List.fold (*) (uint64 1) xs

let part1 =
    File.ReadLines "inputs/day06.txt"
    |> Seq.map (String.splitRemoveEmpty " ")
    |> Seq.toList
    |> List.toGrid
    |> Grid.byColumn
    |> List.map toProblem
    |> List.sumBy compute


let rec parseProblems problems lines =
    match lines, problems with
    | [], _ -> problems
    | x :: xs, _ when String.IsNullOrWhiteSpace x -> parseProblems problems xs
    | x :: xs, _ when x.EndsWith '+' -> parseProblems (Sum [ x.TrimEnd '+' |> uint64 ] :: problems) xs
    | x :: xs, _ when x.EndsWith '*' -> parseProblems (Product [ x.TrimEnd '*' |> uint64 ] :: problems) xs
    | x :: xs, Sum ys :: ps -> parseProblems (Sum(uint64 (x.Trim()) :: ys) :: ps) xs
    | x :: xs, Product ys :: ps -> parseProblems (Product(uint64 (x.Trim()) :: ys) :: ps) xs
    | _ -> failwith "Invalid problem list"


let part2 =
    File.ReadLines "inputs/day06.txt"
    |> array2D
    |> Array2D.transpose
    |> Array2D.toStrings
    |> Seq.toList
    |> parseProblems []
    |> List.sumBy compute

let run =
    printfn "== Day 06 =="

    printfn "Part 1: %d" part1
    printfn "Part 2: %d" part2
