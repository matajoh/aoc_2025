module Extensions

open System

module Array2D =
    let tryGet a (r, c) =
        let rows = Array2D.length1 a
        let cols = Array2D.length2 a

        if r < 0 || r >= rows || c < 0 || c >= cols then
            None
        else
            Some(Array2D.get a r c)

    let rowColumnIndices a =
        seq {
            for r in 0 .. Array2D.length1 a - 1 do
                for c in 0 .. Array2D.length2 a - 1 do
                    yield r, c
        }

    let toStrings a =
        seq {
            for r in 0 .. Array2D.length1 a - 1 do
                yield String a.[r, 0..]
        }

    let transpose a =
        let rows = Array2D.length1 a
        let cols = Array2D.length2 a
        let aT = Array2D.create cols rows (Array2D.get a 0 0)

        for r in 0 .. rows - 1 do
            for c in 0 .. cols - 1 do
                Array2D.set aT c r (Array2D.get a r c)

        aT


type Position =
    { Row: int
      Column: int }

    static member neighbors8 p =
        let r = Position.row p
        let c = Position.column p

        seq {
            for rr in r - 1 .. r + 1 do
                for cc in c - 1 .. c + 1 do
                    if rr <> r || cc <> c then
                        yield { Row = rr; Column = cc }
        }

    static member neighbors4 p =
        let r = Position.row p
        let c = Position.column p

        seq {
            { Row = r - 1; Column = c }
            { Row = r; Column = c + 1 }
            { Row = r + 1; Column = c }
            { Row = r; Column = c - 1 }
        }

    static member distance a b =
        abs (a.Row - b.Row) + abs (a.Column - b.Column)

    static member row p = p.Row

    static member column p = p.Column

    static member add a b =
        { Row = a.Row + b.Row
          Column = a.Column + b.Column }

    static member sub a b =
        { Row = a.Row - b.Row
          Column = a.Column - b.Column }

    static member div a b =
        { Row = a.Row / b
          Column = a.Column / b }

    static member negate a =
        { Row = (-a.Row); Column = (-a.Column) }

    static member (+)(a, b) = Position.add a b

    static member (-)(a, b) = Position.sub a b

    static member (~-)(a) = Position.negate a

    static member (/)(a, b) = Position.div a b


type Direction =
    | North
    | East
    | South
    | West

    static member turnRight d =
        match d with
        | North -> East
        | East -> South
        | South -> West
        | West -> North

    static member turnLeft d =
        match d with
        | North -> West
        | West -> South
        | South -> East
        | East -> North

    static member move n p d =
        match d with
        | North -> { p with Row = p.Row - n }
        | East -> { p with Column = p.Column + n }
        | South -> { p with Row = p.Row + n }
        | West -> { p with Column = p.Column - n }

    static member next = Direction.move 1

    static member numTurns a b =
        let toInt d =
            match d with
            | North -> 0
            | East -> 1
            | South -> 2
            | West -> 3

        let diff = abs (toInt a - toInt b)
        if diff = 3 then 1 else diff

    static member Values = [ North; East; South; West ]


type grid<'a> when 'a: comparison =
    { Table: Map<Position, 'a>
      Rows: int
      Columns: int }

module Grid =
    let find key grid = Map.find key grid.Table

    let tryFind key grid = Map.tryFind key grid.Table

    let count grid = Map.count grid.Table

    let contains key grid = Map.containsKey key grid.Table

    let map mapping grid =
        { Rows = grid.Rows
          Columns = grid.Columns
          Table = Map.map mapping grid.Table }

    let choose mapping grid =
        let choose (k, v) =
            match v with
            | Some x -> Some(k, x)
            | None -> None

        let table =
            grid.Table
            |> Map.toList
            |> List.map (fun (k, v) -> k, mapping v)
            |> List.choose choose
            |> Map.ofList

        { Rows = grid.Rows
          Columns = grid.Columns
          Table = table }

    let chooseKeys mapping grid =
        let choose (k, v) =
            match k with
            | Some x -> Some(x, v)
            | None -> None

        let table =
            grid.Table
            |> Map.toList
            |> List.map (fun (k, v) -> mapping k, v)
            |> List.choose choose
            |> Map.ofList

        { Rows = grid.Rows
          Columns = grid.Columns
          Table = table }

    let add k v g = { g with Table = Map.add k v g.Table }

    let filter (vals: Set<'a>) grid =
        let f _ x = Set.contains x vals

        { grid with
            Table = Map.filter f grid.Table }

    let filterOut (vals: Set<'a>) grid =
        let f _ x = not (Set.contains x vals)

        { grid with
            Table = Map.filter f grid.Table }

    let findValue x grid =
        grid.Table |> Map.toList |> List.filter (fun (_, y) -> y = x) |> List.map fst

    let byRow grid =
        grid.Table
        |> Map.toList
        |> List.groupBy (fst >> Position.row)
        |> List.map (fun (r, xs) -> r, (xs |> List.map (fun (p, x) -> Position.column p, x)))

    let byColumn grid =
        grid.Table
        |> Map.toList
        |> List.groupBy (fst >> Position.column)
        |> List.map (fun (c, xs) -> c, (xs |> List.map (fun (p, x) -> Position.row p, x)))

    let isInside grid p =
        not (p.Row < 0 || p.Row >= grid.Rows || p.Column < 0 || p.Column >= grid.Columns)

    let keys grid = Map.keys grid.Table |> Seq.toList

    let create rows columns table =
        { Rows = rows
          Columns = columns
          Table = table }

    let table grid = grid.Table

module List =
    let print list =
        List.map
            (fun x ->
                printfn "%A" x
                x)
            list

    let binarySearch compare value list =
        let rec search left right =
            if left = right then
                ~~~left
            else
                let mid = (left + right) / 2
                let test = List.item mid list
                let cmp = compare value test

                if cmp < 0 then
                    search left mid
                elif cmp > 0 then
                    if right = mid + 1 then ~~~right else search mid right
                else
                    mid

        search 0 (List.length list - 1)

    let arange min max =
        seq {
            for i in min..max do
                yield i
        }
        |> Seq.toList

    let powrange x count =
        seq {
            let mutable x' = 1UL

            for _ in 1..count do
                yield x'
                x' <- x' * x
        }
        |> Seq.toList

    let toGrid list =
        let rows = List.length list
        let columns = list |> List.map Seq.length |> List.max

        let table =
            list
            |> List.map (Seq.indexed >> Seq.toList)
            |> List.indexed
            |> List.collect (fun (r, xs) -> xs |> List.map (fun (c, x) -> { Row = r; Column = c }, x))
            |> Map.ofList

        Grid.create rows columns table

    let permutations list =
        let rec inserts e =
            function
            | [] -> [ [ e ] ]
            | x :: xs as list -> (e :: list) :: (inserts e xs |> List.map (fun xs' -> x :: xs'))

        List.fold (fun accum x -> List.collect (inserts x) accum) [ [] ] list


module String =
    let split (separator: String) (string: String) = string.Split separator |> Array.toList

    let splitRemoveEmpty (separator: String) (string: String) =
        string.Split(separator, StringSplitOptions.RemoveEmptyEntries) |> Array.toList

    let trim (string: String) = string.Trim()


type DisjointSet =
    { Parent: int array
      Rank: int array }

    static member create n =
        { Parent = [| 0 .. n - 1 |]
          Rank = Array.zeroCreate n }

    static member find x d =
        let rec f i =
            if d.Parent.[i] <> i then
                Array.set d.Parent i (f d.Parent.[i])

            Array.get d.Parent i

        f x

    static member inSet i j d =
        DisjointSet.find i d = DisjointSet.find j d

    static member union x y d =
        let xroot = DisjointSet.find x d
        let yroot = DisjointSet.find y d

        if xroot <> yroot then
            if d.Rank.[xroot] < d.Rank.[yroot] then
                Array.set d.Parent xroot yroot
            elif d.Rank[xroot] > d.Rank.[yroot] then
                Array.set d.Parent yroot xroot
            else
                Array.set d.Parent yroot xroot
                Array.set d.Rank xroot (d.Rank.[xroot] + 1)

    static member count d =
        [ 0 .. Array.length d.Parent - 1 ]
        |> List.map (fun x -> DisjointSet.find x d)
        |> set
        |> Set.count

    static member sets d =
        let rec f values sets =
            match values with
            | [] -> sets |> Map.values |> Seq.toList
            | (v, p) :: vs ->
                match Map.tryFind p sets with
                | Some s -> sets |> Map.add p (Set.add v s) |> f vs
                | None -> sets |> Map.add p (Set.singleton v) |> f vs

        let values =
            [ 0 .. Array.length d.Parent - 1 ]
            |> List.map (fun i -> i, DisjointSet.find i d)

        f values Map.empty
