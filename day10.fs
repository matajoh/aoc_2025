module Day10

open System.IO
open Algorithms
open Extensions
open Maths
open System.Collections.Generic

type Machine =
    { Id: int
      Indicators: int
      Buttons: int list list
      Joltage: int list
      NumOutputs: int
      NumButtons: int }

let rec bitsToInt =
    Seq.fold (fun value b -> if b then (value <<< 1) + 1 else value <<< 1) 0


let parseIndicators s =
    let length = String.length s

    s.[1 .. length - 2] |> Seq.rev |> Seq.map ((=) '#') |> bitsToInt

let rec toBits i length bits button =
    match button with
    | [] when i = length -> bits
    | [] -> toBits (i + 1) length (false :: bits) button
    | b :: bs when b = i -> toBits (i + 1) length (true :: bits) bs
    | _ -> toBits (i + 1) length (false :: bits) button

let parseButton numOutputs s =
    let length = String.length s
    s.Substring(1, String.length s - 2) |> String.split "," |> List.map int

let parseJoltage s =
    let length = String.length s
    s.Substring(1, length - 2) |> String.split "," |> List.map int

let parseMachine i line =
    let parts = String.split " " line
    let length = List.length parts
    let indicators = parseIndicators parts.[0]
    let joltage = parseJoltage parts.[length - 1]
    let numOutputs = List.length joltage
    let buttons = parts.[1 .. length - 2] |> List.map (parseButton numOutputs)

    { Id = i
      Indicators = indicators
      Buttons = buttons
      Joltage = joltage
      NumOutputs = numOutputs
      NumButtons = List.length buttons }


let rec countBits count n =
    if n = 0 then count else countBits (count + 1) (n &&& n - 1)


let findMinIndicatorPresses machine =
    let distance _ _ = 1

    let heuristic _ = 0

    let neighbors buttons x = buttons |> Seq.map ((^^^) x)

    let goal y x = y = x

    let buttons =
        machine.Buttons |> List.map (toBits 0 machine.NumOutputs [] >> bitsToInt)

    let astar =
        { Distance = distance
          Heuristic = heuristic
          Neighbors = neighbors buttons
          Goal = goal machine.Indicators }

    AStar.findMinPath 0 astar |> List.length |> (fun x -> x - 1)


let part1 = List.map findMinIndicatorPresses >> List.sum

let partitions count total =
    let cache = new Dictionary<int * int, int list list>()

    let rec f c t =
        if c = 0 then
            []
        elif c = 1 then
            [ [ t ] ]
        else
            let key = c, t

            if cache.ContainsKey key then
                cache.[key]
            else
                let parts =
                    if t = 0 then
                        [ List.init c (fun _ -> 0) ]
                    else
                        [ 0..t ]
                        |> List.collect (fun i -> f (c - 1) (t - i) |> List.map (fun tail -> i :: tail))

                cache.Add(key, parts)
                parts

    f count total

type Equation =
    | Simple of int * Rational
    | Dependent of int * Map<int, Rational> * Rational

let toLinearSystem machine =
    let a = Array2D.create machine.NumOutputs (machine.NumButtons + 1) Rational.Zero

    machine.Buttons
    |> List.iteri (fun c b -> b |> List.iter (fun r -> a[r, c] <- Rational.One))

    machine.Joltage
    |> List.iteri (fun r j -> a[r, machine.NumButtons] <- rational j)

    a |> Matrix.create |> Matrix.gaussElimination

let toEquation system row =
    let columns = Matrix.columns system - 1
    let value = system[row, columns]

    let rec f c eq =
        if c = columns then
            eq
        else
            match system[row, c], eq with
            | v, _ when v = Rational.Zero -> f (c + 1) eq
            | v, None when v = Rational.One -> f (c + 1) (Some(Simple(c, value)))
            | v, Some(Simple(i, value)) -> f (c + 1) (Some(Dependent(i, [ c, v ] |> Map.ofList, value)))
            | v, Some(Dependent(i, js, value)) -> f (c + 1) (Some(Dependent(i, Map.add c v js, value)))
            | _ -> failwith "Invalid system"

    f 0 None

let findFreeVariables machine equations =
    // find a way to make this a sequence
    // (to avoid the allocation)
    // and also add the simple thing so the end result is just the presses for all buttons

    let maxPresses b =
        machine.Buttons[b] |> List.map (fun i -> machine.Joltage.[i]) |> List.min

    let addFreeVariables (freeVariables, seen) eq =
        match eq with
        | Simple _ -> freeVariables, seen
        | Dependent(i, coeffs, v) ->
            let missing =
                coeffs
                |> Map.keys
                |> Seq.filter (fun f -> Set.contains f seen |> not)
                |> Seq.toList

            match missing with
            | [] -> freeVariables, seen
            | a :: [] when Set.isEmpty seen ->
                let i_max = maxPresses i
                let a_coeff = Map.find a coeffs

                let freeVariables' =
                    [ 0..i_max ]
                    |> List.choose (fun ip ->
                        match Rational.toInt ((v - (rational ip)) / a_coeff) with
                        | Some ap when ap >= 0 -> Some(Map.add a ap Map.empty)
                        | _ -> None)

                freeVariables', Set.singleton a
            | a :: b :: [] when Set.isEmpty seen ->
                let i_max = maxPresses i
                let a_max = maxPresses a
                let a_coeff = Map.find a coeffs
                let b_max = maxPresses b
                let b_coeff = Map.find b coeffs

                let freeVariables' =
                    if a_max < b_max then
                        List.allPairs [ 0..i_max ] [ 0..a_max ]
                        |> List.choose (fun (ip, ap) ->
                            match Rational.toInt ((v - (rational ip) - (a_coeff * rational ap)) / b_coeff) with
                            | Some bp when bp >= 0 -> Some([ (a, int64 ap); (b, bp) ] |> Map.ofList)
                            | _ -> None)
                    else
                        List.allPairs [ 0..i_max ] [ 0..b_max ]
                        |> List.choose (fun (ip, bp) ->
                            match Rational.toInt ((v - (rational ip) - (a_coeff * rational bp)) / a_coeff) with
                            | Some ap when ap >= 0 -> Some([ (a, ap); (b, int64 bp) ] |> Map.ofList)
                            | _ -> None)

                freeVariables', [ a; b ] |> set
            | a :: [] ->
                let i_max = maxPresses i
                let a_coeff = Map.find a coeffs

                let freeVariables' =
                    freeVariables
                    |> List.collect (fun fvs ->
                        let fv_sum =
                            fvs
                            |> Map.toSeq
                            |> Seq.sumBy (fun (k, v) ->
                                match Map.tryFind k coeffs with
                                | Some coeff -> coeff * (rational v)
                                | None -> Rational.Zero)

                        [ 0..i_max ]
                        |> List.choose (fun i ->
                            match Rational.toInt ((v - fv_sum - (rational i)) / a_coeff) with
                            | Some ap when ap >= 0 -> Some(Map.add a ap fvs)
                            | _ -> None))

                freeVariables', Set.add a seen
            | a :: b :: [] ->
                let i_max = maxPresses i
                let a_max = maxPresses a
                let a_coeff = Map.find a coeffs
                let b_max = maxPresses b
                let b_coeff = Map.find b coeffs

                let freeVariables' =
                    freeVariables
                    |> List.collect(fun fvs ->
                        let fv_sum =
                            fvs
                            |> Map.toSeq
                            |> Seq.sumBy (fun (k, v) ->
                                match Map.tryFind k coeffs with
                                | Some coeff -> coeff * (rational v)
                                | None -> Rational.Zero)
                        
                        if a_max < b_max then
                            List.allPairs [ 0..i_max ] [ 0..a_max ]
                            |> List.choose (fun (ip, ap) ->
                                match Rational.toInt ((v - (rational ip) - fv_sum - (a_coeff * rational ap)) / b_coeff) with
                                | Some bp when bp >= 0 -> Some([ (a, int64 ap); (b, bp) ] |> Map.ofList)
                                | _ -> None)
                        else
                            List.allPairs [ 0..i_max ] [ 0..b_max ]
                            |> List.choose (fun (ip, bp) ->
                                match Rational.toInt ((v - (rational ip) - fv_sum - (a_coeff * rational bp)) / a_coeff) with
                                | Some ap when ap >= 0 -> Some([ (a, ap); (b, int64 bp) ] |> Map.ofList)
                                | _ -> None))

                freeVariables', [ a; b ] |> set
            | _ ->
                printfn "%A %A" missing seen
                failwith "I really don't want to deal with this"


    equations
    |> List.sortBy (function
        | Dependent(i, fvs, _) -> Map.count fvs, maxPresses i
        | Simple _ -> 0, 0)
    |> List.fold addFreeVariables (List.empty, Set.empty)
    |> fst


let findMinJoltagePresses machine =
    let system = machine |> toLinearSystem
    let equations = [ 0 .. machine.NumOutputs - 1 ] |> List.choose (toEquation system)

    let freeVariables = findFreeVariables machine equations

    printfn "%d: %d free variable sets" machine.Id (List.length freeVariables)

    if List.length freeVariables = 0 then
        equations
        |> List.sumBy (function
            | Simple(_, v) -> v
            | Dependent _ -> failwith "Error")
        |> Rational.toInt
        |> Option.get
    else
        let solve values eq =
            match eq with
            | Simple(_, v) -> Rational.toInt v
            | Dependent(_, fvs, v) ->
                let p =
                    v
                    - (fvs |> Seq.sumBy (fun kv -> (Map.find kv.Key values |> rational) * kv.Value))

                if p < Rational.Zero then None else Rational.toInt p

        let presses (equations: Equation list) values =
            equations
            |> List.fold
                (fun maybe_p eq ->
                    match maybe_p, solve values eq with
                    | None, _ -> None
                    | _, None -> None
                    | Some p, Some eq_p -> Some(p + eq_p))
                (Some(values |> Seq.sumBy (fun kv -> int64 kv.Value)))

        freeVariables |> Seq.choose (presses equations) |> Seq.min


let part2 machines =
    machines |> List.sumBy findMinJoltagePresses

let run =
    printfn "== Day 10 =="

    let machines =
        File.ReadLines "inputs/day10.txt" |> Seq.mapi parseMachine |> Seq.toList

    printfn "Part 1: %d" (part1 machines)
    printfn "Part 2: %d" (part2 machines)
    printfn ""
