let run i =
    match i with
    | 1 -> Day01.run
    | 2 -> Day02.run
    | 3 -> Day03.run
    | 4 -> Day04.run
    | 5 -> Day05.run
    | 6 -> Day06.run
    | _ -> printfn "Day %i not implemented" i

[<EntryPoint>]
let main argv =
    printfn "## Advent of Code 2025 ##"
    printfn ""

    match argv.Length with
    | 0 -> [ 1..25 ] |> Seq.iter (fun i -> run i)
    | _ -> argv |> Seq.map int |> Seq.iter (fun i -> run i)

    0
