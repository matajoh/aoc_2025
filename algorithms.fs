module Algorithms

open System


type astar<'S> when 'S: comparison =
    { Distance: 'S -> 'S -> int
      Heuristic: 'S -> int
      Neighbors: 'S -> 'S seq
      Goal: 'S -> bool }


type state<'S> when 'S: comparison =
    { Queue: Set<int * 'S>
      GScore: Map<'S, int>
      CameFrom: Map<'S, 'S list> }


module private State =
    let create s =
        { Queue = Set.singleton (0, s)
          GScore = Map.ofList [ s, 0 ]
          CameFrom = Map.empty }

    let addNeighbor x y gs fs s =
        { s with
            Queue = Set.add (fs, y) s.Queue
            GScore = Map.add y gs s.GScore
            CameFrom = Map.add y [ x ] s.CameFrom }

    let addAlternatePath x y s =
        let xs = Map.find y s.CameFrom

        { s with
            CameFrom = Map.add y (x :: xs) s.CameFrom }

    let dequeue state =
        let x = Set.minElement state.Queue

        x,
        { state with
            Queue = Set.remove x state.Queue }

    let gscore x state = Map.find x state.GScore

    let reconstructPaths x s =
        let rec f path y =
            match Map.tryFind y s.CameFrom with
            | Some parents -> parents |> Seq.collect (f (y :: path))
            | None -> seq { y :: path }

        f [] x


module AStar =
    let findMinPaths start astar =
        let addNeighbors x xGS s y =
            let yGS = xGS + astar.Distance x y

            let addNeighbor () =
                let yFS = yGS + astar.Heuristic y
                State.addNeighbor x y yGS yFS s

            match Map.tryFind y s.GScore with
            | None -> addNeighbor ()
            | Some gs when yGS < gs -> addNeighbor ()
            | Some gs when yGS = gs -> State.addAlternatePath x y s
            | _ -> s

        let rec f s =
            if Set.isEmpty s.Queue then
                Seq.empty
            else
                let (_, x), s' = State.dequeue s
                let gs = State.gscore x s

                if astar.Goal x then
                    State.reconstructPaths x s
                else
                    astar.Neighbors x |> Seq.fold (addNeighbors x gs) s' |> f

        State.create start |> f

    let tryFindMinPath start astar = Seq.tryHead (findMinPaths start astar)

    let findMinPath start astar =
        match tryFindMinPath start astar with
        | Some path -> path
        | None -> failwith "Unable to find path to goal"


type prefixTreeNode =
    { Children: Map<char, prefixTreeNode>
      Prefix: string
      IsLeaf: bool }


module PrefixTree =
    let findPrefixes str node =
        let rec f ps s n =
            match s with
            | [] -> ps
            | x :: xs ->
                match Map.tryFind x n.Children with
                | Some n' -> if n'.IsLeaf then f (n'.Prefix :: ps) xs n' else f ps xs n'
                | None -> ps

        f [] str node

    let build strings =
        let rec f p s =
            let children =
                s
                |> List.filter (List.isEmpty >> not)
                |> List.groupBy List.head
                |> List.map (fun (c, group) -> (c, f (c :: p) (group |> List.map List.tail)))
                |> Map.ofList

            { Children = children
              Prefix = p |> List.rev |> List.toArray |> String
              IsLeaf = List.exists List.isEmpty s }

        f [] strings


type kdTree<'p> =
    { Distance: 'p -> 'p -> int
      Select: int -> 'p -> int
      MaxDimension: int
      MaxPerNode: int }


type kdTreeNode<'p> =
    { Left: Option<kdTreeNode<'p>>
      Right: Option<kdTreeNode<'p>>
      Dimension: int
      Split: int
      Points: 'p list }


module KDTree =
    let isLeaf n = n.Dimension = -1

    let build points kdtree =
        let rec f points =
            match points with
            | [] -> None
            | _ when List.length points < kdtree.MaxPerNode ->
                Some
                    { Left = None
                      Right = None
                      Dimension = (-1)
                      Split = 0
                      Points = points }
            | _ ->
                let d, _, s =
                    seq {
                        for i in 0 .. kdtree.MaxDimension do
                            let dim = points |> List.map (kdtree.Select i) |> List.sort
                            yield i, List.last dim - List.head dim, dim.[List.length dim / 2]
                    }
                    |> Seq.maxBy (fun (_, b, _) -> b)

                let left = points |> List.filter (fun p -> kdtree.Select d p < s) |> f
                let right = points |> List.filter (fun p -> kdtree.Select d p >= s) |> f

                Some
                    { Left = left
                      Right = right
                      Dimension = d
                      Split = s
                      Points = [] }

        f points |> Option.get

    let search kdtree r d p =
        let rec f ns frontier =
            match frontier with
            | [] -> List.concat ns
            | None :: xs -> f ns xs
            | (Some x) :: xs ->
                if isLeaf x then
                    let n = x.Points |> List.filter (fun p' -> kdtree.Distance p p' <= d)
                    f (n :: ns) xs
                else
                    let pi = kdtree.Select x.Dimension p

                    if pi < x.Split then
                        if x.Split - pi <= d then
                            f ns (x.Left :: x.Right :: xs)
                        else
                            f ns (x.Left :: xs)
                    else if pi - x.Split <= d then
                        f ns (x.Right :: x.Left :: xs)
                    else
                        f ns (x.Right :: xs)

        f [] [ Some r ]


type graphNode =
    { Index: int
      Name: string
      Degree: int }


type graph =
    { Nodes: graphNode array
      Edges: Set<int> array }


module Graph =
    open System.Collections.Generic
    let size g = Array.length g.Nodes


    let edges g i = g.Edges.[i]


    let name g i = g.Nodes.[i].Name

    let degree g i = g.Nodes.[i].Degree


    let hasEdge g (i, j) = g.Edges.[i].Contains(j)


    let degeneracyOrdering g =
        g.Nodes |> Seq.sortBy (fun n -> n.Degree) |> Seq.map (fun n -> n.Index)


    let fromList list =
        let lookup = new Dictionary<string, int>()
        let degree = new Dictionary<int, int>()
        let edges = new List<int * int>()

        let addNode s =
            if lookup.ContainsKey(s) then
                let i = lookup.[s]
                degree.[i] <- 1 + degree.[i]
                i
            else
                let i = lookup.Count
                lookup.[s] <- i
                degree.[i] <- 1
                i

        for a, b in list do
            let i = addNode a
            let j = addNode b
            edges.Add((i, j))
            edges.Add((j, i))

        let nodes =
            lookup
            |> Seq.map (fun kv -> kv.Key, kv.Value)
            |> Seq.toList
            |> Seq.sortBy snd
            |> Seq.map (fun (n, i) ->
                { Name = n
                  Index = i
                  Degree = degree.[i] })
            |> Seq.toArray

        let edgeLookup =
            edges
            |> Seq.groupBy fst
            |> Seq.sortBy fst
            |> Seq.map (fun (_, js) -> (js |> Seq.map snd |> set))
            |> Seq.toArray

        { Nodes = nodes; Edges = edgeLookup }


    let clique3 g i =
        let ie = (edges g i)

        seq {
            for j in ie do
                for k in Set.intersect ie (edges g j) do
                    yield [ i; j; k ]
        }
        |> Seq.map (List.sort)
        |> Seq.map (fun x -> x.[0], x.[1], x.[2])


    let maximalCliques g =

        let rec bronKerbosch2 R P X =
            seq {
                if Set.isEmpty P && Set.isEmpty X then
                    yield R
                else
                    let mutable P' = P
                    let mutable X' = X
                    let u = (Set.union P X) |> Seq.maxBy (degree g)

                    for v in Set.difference P (edges g u) do
                        let vn = edges g v
                        yield! bronKerbosch2 (Set.add v R) (Set.intersect P' vn) (Set.intersect X' vn)
                        P' <- Set.remove v P'
                        X' <- Set.add v X'
            }

        let rec bronKerbosch3 g =
            let mutable P = [ 0 .. size g - 1 ] |> set
            let mutable X = Set.empty

            seq {
                for v in degeneracyOrdering g do
                    let vn = edges g v
                    yield! bronKerbosch2 (Set.singleton v) (Set.intersect P vn) (Set.intersect X vn)
                    P <- Set.remove v P
                    X <- Set.add v X
            }

        bronKerbosch3 g


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
