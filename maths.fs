module Maths

open System.Numerics
open System.Collections.Generic

let rec gcd a b = if b = 0UL then a else gcd b (a % b)

let rec gcdBigInt a b =
    if b = BigInteger.Zero then a else gcdBigInt b (a % b)

let lcm a b = a * (b / gcd a b)

let lcmBigInt a b = a * (b / gcdBigInt a b)

let quadratic a b c x =
    let a' = uint64 a
    let b' = uint64 b
    let c' = uint64 c
    let x' = uint64 x
    a' * x' * x' + b' * x' + c'

let modulo a b = if a < 0 then b + (a % b) else a % b

let argmax (f: 'a -> 'b when 'b: comparison) a =
    a
    |> List.map (fun i -> i, f i)
    |> List.fold (fun (acc, max) (i, v) -> if v > max then i, v else acc, max) (List.head a, List.head a |> f)
    |> fst

type Sign =
    | Plus
    | Minus
    | Zero

    static member from n =
        if n < 0L then Minus
        elif n > 0L then Plus
        else Zero


[<CustomComparison>]
[<CustomEquality>]
type Rational =
    { Num: BigInteger
      Den: BigInteger
      S: Sign }

    static member Zero =
        { Num = BigInteger.Zero
          Den = BigInteger.One
          S = Zero }

    static member One =
        { Num = BigInteger.One
          Den = BigInteger.One
          S = Plus }

    static member toInt r =
        match r.Den = BigInteger.One, r.S with
        | true, Plus -> Some(int64 r.Num)
        | true, Minus -> Some -(int64 r.Num)
        | true, Zero -> Some 0L
        | _ -> None

    static member toDecimal r =
        match r.S with
        | Plus -> decimal r.Num / decimal r.Den
        | Minus -> -(decimal r.Num / decimal r.Den)
        | Zero -> 0m

    override this.ToString() =
        if this.Den = BigInteger.One then
            if this.S = Minus then
                sprintf "-%A" this.Num
            else
                this.Num.ToString()
        else if this.S = Plus then
            sprintf "%A/%A" this.Num this.Den
        elif this.S = Minus then
            sprintf "-%A/%A" this.Num this.Den
        else
            "0"

    override this.Equals other = compare this (other :?> Rational) = 0

    override this.GetHashCode() =
        let r = (this.Num, this.Den)
        r.GetHashCode()

    static member invert r = { r with Num = r.Den; Den = r.Num }

    static member private simplify r =
        assert (r.Den <> BigInteger.Zero)

        if r.Num = BigInteger.Zero then
            { r with
                Den = BigInteger.One
                S = Zero }
        elif r.Num = BigInteger.One || r.Num = BigInteger.One then
            r
        else
            match gcdBigInt r.Num r.Den with
            | div when div = BigInteger.One -> r
            | div ->
                { r with
                    Num = r.Num / div
                    Den = r.Den / div }

    static member private den r0 r1 =
        match r0.Den, r1.Den with
        | d0, d1 when d0 = BigInteger.One -> d1
        | d0, d1 when d1 = BigInteger.One -> d0
        | d0, d1 -> lcmBigInt d0 d1

    static member (+)(a, b) =
        match a.S, b.S with
        | Zero, Zero -> Rational.Zero
        | Zero, _ -> b
        | _, Zero -> a
        | s0, s1 ->
            let den = Rational.den a b
            let an = (den / a.Den) * a.Num
            let bn = (den / b.Den) * b.Num

            if s0 = s1 then
                { Num = an + bn; Den = den; S = s0 } |> Rational.simplify
            elif an > bn then
                { Num = an - bn; Den = den; S = s0 } |> Rational.simplify
            elif an < bn then
                { Num = bn - an; Den = den; S = s1 } |> Rational.simplify
            else
                Rational.Zero

    static member (-)(a, b) = a + -b

    static member (~-) r =
        match r.S with
        | Zero -> r
        | Plus -> { r with S = Minus }
        | Minus -> { r with S = Plus }

    static member (*)(a, b) =
        match a.S, b.S with
        | Zero, _ -> Rational.Zero
        | _, Zero -> Rational.Zero
        | s0, s1 ->
            let lhs = { a with Den = b.Den } |> Rational.simplify
            let rhs = { b with Den = a.Den } |> Rational.simplify
            let sign = if s0 = s1 then Plus else Minus

            { Num = lhs.Num * rhs.Num
              Den = lhs.Den * rhs.Den
              S = sign }
            |> Rational.simplify

    static member (/)(a, b) = a * (Rational.invert b)

    static member Abs r = { r with S = Plus }

    interface System.IComparable with
        member a.CompareTo other =
            let b =
                match other with
                | :? Rational as r -> r
                | :? uint64 as n ->
                    { Num = bigint n
                      Den = BigInteger.One
                      S = Plus }
                | :? int64 as n ->
                    { Num = bigint (abs n)
                      Den = BigInteger.One
                      S = Sign.from n }
                | _ -> failwith "Invalid comparison"

            match a.S, b.S with
            | Plus, Minus -> 1
            | Minus, Plus -> -1
            | Plus, Zero -> 1
            | Minus, Zero -> -1
            | Zero, Plus -> -1
            | Zero, Minus -> 1
            | Zero, Zero -> 0
            | _ ->
                let den = Rational.den a b
                let an = (den / a.Den) * a.Num
                let bn = (den / b.Den) * b.Num
                if a.S = Plus then compare an bn else compare bn an

    member this.Sign =
        match this.S with
        | Plus -> 1
        | Minus -> -1
        | Zero -> 0

let inline rational n =
    let v = int64 n

    if v < 0L then
        { Num = bigint (abs v)
          Den = BigInteger.One
          S = Minus }
    elif v > 0L then
        { Num = bigint v
          Den = BigInteger.One
          S = Plus }
    else
        Rational.Zero

let array2DToString a =
    let rows = Array2D.length1 a
    let cols = Array2D.length2 a
    let vals =
        seq {
        for r in 0..rows-1 do
            for c in 0..cols-1 do
                yield Array2D.get a r c
        } |> Seq.toList
    let hasNegative = vals |> List.exists (fun v -> v < 0m)

    let s =
        vals
        |> List.map (fun d ->
            if d >= 0m && hasNegative then
                " " + d.ToString()
            else
                d.ToString())

    let pad = s |> List.map String.length |> List.max
    let padded = s |> List.map (fun p -> p.PadRight(pad))

    let rows = Array2D.length1 a
    let cols = Array2D.length2 a

    sprintf
        "[%s]"
        (seq {
            for r in 0 .. rows - 1 do
                sprintf
                    "[%s]"
                    (seq {
                        for c in 0 .. cols - 1 do
                            yield padded[r * cols + c]
                     }
                     |> String.concat ", ")
         }
         |> String.concat ",\n ")

type Vector2 =
    { X: decimal
      Y: decimal }

    static member create(a: decimal array) = { X = a[0]; Y = a[1] }
    static member zero = { X = 0m; Y = 0m }
    static member (+)(a, b) = { X = a.X + b.X; Y = a.Y + b.Y }
    static member (-)(a, b) = { X = a.X - b.X; Y = a.Y - b.Y }
    static member (*)(a, b) = { X = a.X * b.X; Y = a.Y * b.Y }
    static member (*)(a: decimal, b) = { X = a * b.X; Y = a * b.Y }
    static member dot a b = a.X * b.X + a.Y * b.Y

    static member toString v =
        sprintf "[%s, %s]" (v.X.ToString()) (v.Y.ToString())


type Matrix2 =
    { A: decimal
      B: decimal
      C: decimal
      D: decimal }

    static member (*)(lhs: decimal, rhs: Matrix2) =
        { A = lhs * rhs.A
          B = lhs * rhs.B
          C = lhs * rhs.C
          D = lhs * rhs.D }

    static member (+)(lhs: Matrix2, rhs: Matrix2) =
        { A = lhs.A + rhs.A
          B = lhs.B + rhs.B
          C = lhs.C + rhs.C
          D = lhs.D + rhs.D }

    static member (-)(lhs: Matrix2, rhs: Matrix2) =
        { A = lhs.A - rhs.A
          B = lhs.B - rhs.B
          C = lhs.C - rhs.C
          D = lhs.D - rhs.D }

    static member (*)(lhs: Matrix2, rhs: Matrix2) =
        { A = lhs.A * rhs.A + lhs.B * rhs.C
          B = lhs.A * rhs.B + lhs.B * rhs.D
          C = lhs.C * rhs.A + lhs.D * rhs.C
          D = lhs.C * rhs.B + lhs.D * rhs.D }

    static member (*)(lhs: Matrix2, rhs: Vector2) =
        { X = lhs.A * rhs.X + lhs.B * rhs.Y
          Y = lhs.C * rhs.X + lhs.D * rhs.Y }

    static member determinant m = m.A * m.D - m.B * m.C

    static member inverse m =
        let det = Matrix2.determinant m

        if det = 0m then
            None
        else
            Some((1m / det) * { A = m.D; B = -m.B; C = -m.C; D = m.A })

    static member transpose m = { m with B = m.C; C = m.B }

    static member trace m = m.A + m.D

    static member identity = { A = 1m; B = 0m; C = 0m; D = 1m }

    static member zero = { A = 0m; B = 0m; C = 0m; D = 0m }

    static member create(a, b, c, d) = { A = a; B = b; C = c; D = d }

    static member create(a: decimal array) =
        assert (Array.length a = 4)

        { A = a[0]
          B = a[1]
          C = a[2]
          D = a[3] }

    static member create(a: decimal list) =
        assert (List.length a = 4)

        { A = a[0]
          B = a[1]
          C = a[2]
          D = a[3] }

    static member create(a: decimal array2d) =
        assert (Array2D.length1 a = 2 && Array2D.length2 a = 2)

        { A = a[0, 0]
          B = a[0, 1]
          C = a[1, 0]
          D = a[1, 1] }

    static member toArray m =
        [ [ m.A; m.B ]; [ m.C; m.D ] ] |> array2D

    static member toString m = m |> Matrix2.toArray |> array2DToString

type Vector3 =
    { X: decimal
      Y: decimal
      Z: decimal }

    static member create(a: decimal array) = { X = a[0]; Y = a[1]; Z = a[2] }

    static member zero = { X = 0m; Y = 0m; Z = 0m }

    static member toString v =
        sprintf "[%s, %s, %s]" (v.X.ToString()) (v.Y.ToString()) (v.Z.ToString())

    static member (+)(a, b) =
        { X = a.X + b.X
          Y = a.Y + b.Y
          Z = a.Z + b.Z }

    static member (-)(a, b) =
        { X = a.X - b.X
          Y = a.Y - b.Y
          Z = a.Z - b.Z }

    static member (*)(a, b) =
        { X = a.X * b.X
          Y = a.Y * b.Y
          Z = a.Z * b.Z }

    static member (*)(a: decimal, b) =
        { X = a * b.X
          Y = a * b.Y
          Z = a * b.Z }

    static member dot a b = a.X * b.X + a.Y * b.Y + a.Z * b.Z

    static member cross a b =
        { X = a.Y * b.Z - a.Z - b.Y
          Y = a.Z * b.X - a.X * b.Z
          Z = a.X * b.Y - a.Y * b.X }

    static member min a b =
        { X = min a.X b.X
          Y = min a.Y b.Y
          Z = min a.Z b.Z }

    static member xy v = { X = v.X; Y = v.Y }
    static member xz v = { X = v.X; Y = v.Z }
    static member yz v = { X = v.Y; Y = v.Z }

type Matrix3 =
    { A: decimal
      B: decimal
      C: decimal
      D: decimal
      E: decimal
      F: decimal
      G: decimal
      H: decimal
      I: decimal }

    static member (*)(lhs: decimal, rhs: Matrix3) =
        { A = lhs * rhs.A
          B = lhs * rhs.B
          C = lhs * rhs.C
          D = lhs * rhs.D
          E = lhs * rhs.E
          F = lhs * rhs.F
          G = lhs * rhs.G
          H = lhs * rhs.H
          I = lhs * rhs.I }

    static member (+)(lhs: Matrix3, rhs: Matrix3) =
        { A = lhs.A + rhs.A
          B = lhs.B + rhs.B
          C = lhs.C + rhs.C
          D = lhs.D + rhs.D
          E = lhs.E + rhs.E
          F = lhs.F + rhs.F
          G = lhs.G + rhs.G
          H = lhs.H + rhs.H
          I = lhs.I + rhs.I }

    static member (-)(lhs: Matrix3, rhs: Matrix3) =
        { A = lhs.A - rhs.A
          B = lhs.B - rhs.B
          C = lhs.C - rhs.C
          D = lhs.D - rhs.D
          E = lhs.E - rhs.E
          F = lhs.F - rhs.F
          G = lhs.G - rhs.G
          H = lhs.H - rhs.H
          I = lhs.I - rhs.I }

    static member (*)(lhs: Matrix3, rhs: Matrix3) =
        { A = lhs.A * rhs.A + lhs.B * rhs.D + lhs.C * rhs.G
          B = lhs.A * rhs.B + lhs.B * rhs.E + lhs.C * rhs.H
          C = lhs.A * rhs.C + lhs.B * rhs.F + lhs.C * rhs.I
          D = lhs.D * rhs.A + lhs.E * rhs.D + lhs.F * rhs.G
          E = lhs.D * rhs.B + lhs.E * rhs.E + lhs.F * rhs.H
          F = lhs.D * rhs.C + lhs.E * rhs.F + lhs.F * rhs.I
          G = lhs.G * rhs.A + lhs.H * rhs.D + lhs.I * rhs.G
          H = lhs.G * rhs.B + lhs.H * rhs.E + lhs.I * rhs.H
          I = lhs.G * rhs.C + lhs.H * rhs.F + lhs.I * rhs.I }

    static member (*)(lhs: Matrix3, rhs: Vector3) =
        { X = lhs.A * rhs.X + lhs.B * rhs.Y + lhs.C * rhs.Z
          Y = lhs.D * rhs.X + lhs.E * rhs.Y + lhs.F * rhs.Z
          Z = lhs.G * rhs.X + lhs.H * rhs.Y + lhs.I * rhs.Z }

    static member determinant m =
        m.A * m.E * m.I + m.B * m.F * m.G + m.C * m.D * m.H
        - m.C * m.E * m.G
        - m.B * m.D * m.I
        - m.A * m.F * m.H

    static member inverse m =
        let det = Matrix3.determinant m

        if det = 0m then
            None
        else
            Some(
                (1m / det)
                * { A = m.E * m.I - m.F * m.H
                    B = -(m.B * m.I - m.C * m.H)
                    C = m.B * m.F - m.C * m.E
                    D = -(m.D * m.I - m.F * m.G)
                    E = (m.A * m.I - m.C * m.G)
                    F = -(m.A * m.F - m.C * m.D)
                    G = (m.D * m.H - m.E * m.G)
                    H = -(m.A * m.H - m.B * m.G)
                    I = m.A * m.E - m.B * m.D }
            )

    static member transpose m =
        { m with
            B = m.D
            D = m.B
            C = m.G
            G = m.C
            F = m.H
            H = m.F }

    static member trace m = m.A + m.E + m.I

    static member identity =
        { A = 1m
          B = 0m
          C = 0m
          D = 0m
          E = 1m
          F = 0m
          G = 0m
          H = 0m
          I = 1m }

    static member zero =
        { A = 0m
          B = 0m
          C = 0m
          D = 0m
          E = 0m
          F = 0m
          G = 0m
          H = 0m
          I = 0m }

    static member create(a: decimal array) =
        assert (Array.length a = 9)

        { A = a[0]
          B = a[1]
          C = a[2]
          D = a[3]
          E = a[4]
          F = a[5]
          G = a[6]
          H = a[7]
          I = a[8] }

    static member create(a: decimal list) =
        assert (List.length a = 9)

        { A = a[0]
          B = a[1]
          C = a[2]
          D = a[3]
          E = a[4]
          F = a[5]
          G = a[6]
          H = a[7]
          I = a[8] }

    static member create(a: decimal array2d) =
        assert (Array2D.length1 a = 3 && Array2D.length2 a = 3)

        { A = a[0, 0]
          B = a[0, 1]
          C = a[0, 2]
          D = a[1, 0]
          E = a[1, 1]
          F = a[1, 2]
          G = a[2, 0]
          H = a[2, 1]
          I = a[2, 2] }

    static member toArray m =
        [ [ m.A; m.B; m.C ]; [ m.D; m.E; m.F ]; [ m.G; m.H; m.I ] ] |> array2D

    static member toString m = m |> Matrix3.toArray |> array2DToString


type Vector =
    { A: Rational array }

    static member zero length =
        { A = Array.create length Rational.Zero }

    static member onehot length i =
        { A = Array.init length (fun j -> if j = i then Rational.One else Rational.Zero) }

    static member create a = { A = a }

    static member get v i = v.A[i]

    static member set v i x = v.A[i] <- x

    member this.Item
        with get i = Vector.get this i
        and set i v = Vector.set this i v

    static member length v = Array.length v.A

    static member (+)(a, b) =
        assert (Vector.length a = Vector.length b)
        Array.init (Vector.length a) (fun i -> a[i] + b[i]) |> Vector.create

    static member (-)(a, b) =
        assert (Vector.length a = Vector.length b)
        Array.init (Vector.length a) (fun i -> a[i] - b[i]) |> Vector.create

    static member (*)(a: Rational, b: Vector) =
        Array.init (Vector.length b) (fun i -> a * b[i]) |> Vector.create

    static member (*)(a: Vector, b: Vector) =
        assert (Vector.length a = Vector.length b)
        Array.init (Vector.length a) (fun i -> a[i] * b[i]) |> Vector.create

    static member dot a b =
        assert (Vector.length a = Vector.length b)
        a.A |> Array.zip b.A |> Seq.map (fun (x, y) -> x * y) |> Seq.sum

    static member toString v =
        v.A |> Array.map (fun x -> x.ToString()) |> String.concat ", " |> sprintf "[%s]"


type Matrix =
    { A: Rational array2d }

    static member create a = { A = a }

    static member get m r c = m.A[r, c]

    static member set m r c v = m.A[r, c] <- v

    member this.Item
        with get (r, c) = Matrix.get this r c
        and set (r, c) v = Matrix.set this r c v

    static member rows m = Array2D.length1 m.A
    static member columns m = Array2D.length2 m.A

    member this.Rows = Matrix.rows this
    member this.Columns = Matrix.columns this

    static member isSquare m = Matrix.rows m = Matrix.columns m

    static member sameSize a b =
        not (Matrix.rows a <> Matrix.rows b || Matrix.columns a <> Matrix.columns b)

    static member determinant m =
        assert (Matrix.isSquare m)

        let sub a n i =
            seq {
                for r in 1..n do
                    yield
                        seq {
                            for c in 0..n do
                                if c <> i then
                                    yield Array2D.get a r c
                        }
            }
            |> array2D

        let rec f n (a: Rational array2d) =
            if n = 2 then
                a[0, 0] * a[1, 1] - a[0, 1] * a[1, 0]
            elif n = 3 then
                a[0, 0] * a[1, 1] * a[2, 2]
                + a[0, 1] * a[1, 2] * a[2, 0]
                + a[0, 2] * a[1, 0] * a[2, 1]
                - a[0, 2] * a[1, 1] * a[2, 0]
                - a[0, 1] * a[1, 0] * a[2, 2]
                - a[0, 0] * a[1, 2] * a[2, 1]
            else
                [ 0 .. n - 1 ]
                |> List.map (fun i -> i, sub a (n - 1) i)
                |> List.map (fun (i, sm) -> i, f (n - 1) sm)
                |> List.map (fun (i, d) -> if i % 2 = 0 then m[0, i] * d else -m[0, i] * d)
                |> List.sum

        f (Matrix.rows m) m.A

    static member trace m =
        assert (Matrix.isSquare m)
        [ 0 .. Matrix.rows m - 1 ] |> List.sumBy (fun i -> Matrix.get m i i)

    static member transpose m =
        Array2D.init (Matrix.columns m) (Matrix.rows m) (fun r c -> m[c, r])
        |> Matrix.create

    static member (*)(a: Rational, b: Matrix) =
        Array2D.init (Matrix.rows b) (Matrix.columns b) (fun r c -> a * b[r, c])
        |> Matrix.create

    static member (+)(a: Matrix, b: Matrix) =
        assert (Matrix.sameSize a b)

        Array2D.init (Matrix.rows b) (Matrix.columns b) (fun r c -> a[r, c] + b[r, c])
        |> Matrix.create

    static member (-)(a: Matrix, b: Matrix) =
        assert (Matrix.sameSize a b)

        Array2D.init (Matrix.rows b) (Matrix.columns b) (fun r c -> a[r, c] - b[r, c])
        |> Matrix.create

    static member (*)(a: Matrix, b: Matrix) =
        assert (Matrix.columns a = Matrix.rows b)
        let rows = Matrix.rows a
        let cols = Matrix.columns b
        let n = Matrix.columns a

        Array2D.init rows cols (fun r c -> [ 0 .. n - 1 ] |> List.sumBy (fun i -> a[r, i] * b[i, c]))
        |> Matrix.create

    static member (*)(a: Matrix, b: Vector) =
        assert (Matrix.columns a = Vector.length b)
        let result = Array.create (Vector.length b) Rational.Zero

        for r in 0 .. Matrix.rows a - 1 do
            for c in 0 .. Matrix.columns a - 1 do
                result[r] <- result[r] + a[r, c] * b[c]

        result |> Vector.create

    static member identity n =
        Array2D.init n n (fun r c -> if r = c then Rational.One else Rational.Zero)
        |> Matrix.create

    static member zeros r c =
        Array2D.create r c Rational.Zero |> Matrix.create


    static member gaussElimination mat =
        let m = Matrix.rows mat
        let n = Matrix.columns mat
        let a = mat.A

        let swap r0 r1 =
            for i in 0 .. n - 1 do
                let v = a[r0, i]
                a[r0, i] <- a[r1, i]
                a[r1, i] <- v

        let rec f h k =
            if h = m || k = n then
                // zero out the upper triangular
                let pivots = new Dictionary<int, int>()
                for r in 0..m-1 do
                    let mutable f = Rational.Zero
                    for c in r..n-1 do
                        if f = Rational.Zero && a[r,c] <> Rational.Zero then
                            f <- Rational.invert a[r,c]
                            pivots.Add(c, r)
                        
                        a[r,c] <- f * a[r,c]

                for c in 0..n-1 do
                    if pivots.ContainsKey c then
                        let pivot = pivots.[c]
                        for r in 0..pivot-1 do
                            let f = a[r, c]
                            if f <> Rational.Zero then
                                for i in c..n-1 do
                                    a[r, i] <- a[r, i] - f * a[pivot, i]

                a |> Matrix.create
            else
                let i_max = seq {h .. m-1} |> Seq.maxBy (fun i -> a[i,k] |> abs)
                if a[i_max, k] = Rational.Zero then
                    f h (k+1)
                else
                    swap h i_max
                    for i in h+1..m-1 do
                        let f = a[i,k] / a[h,k]
                        a[i,k] <- Rational.Zero
                        for j in k + 1..n-1 do
                            a[i,j] <- a[i,j] - a[h,j] * f
                    
                    f (h + 1) (k + 1)
        
        f 0 0

    static member tryInverse mat =
        // Gauss-Jordan Elimination
        let m = Matrix.rows mat
        let n = m * 2
        let a = Array2D.create m n Rational.Zero
        a[0 .. m - 1, 0 .. m - 1] <- mat.A
        a[0 .. m - 1, m..] <- (Matrix.identity n).A

        let swap r0 r1 =
            for i in 0 .. n - 1 do
                let v = a[r0, i]
                a[r0, i] <- a[r1, i]
                a[r1, i] <- v

        let rec gaussElimination h k =
            if h = m || k = n then
                // zero out the upper triangle
                for r in 0 .. m - 1 do
                    let f = Rational.invert a[r, r]

                    for c in r .. n - 1 do
                        a[r, c] <- a[r, c] * f

                for c in 1 .. m - 1 do
                    for r in 0 .. c - 1 do
                        let f = a[r, c]

                        if f <> Rational.Zero then
                            for i in c .. n - 1 do
                                a[r, i] <- a[r, i] - f * a[c, i]

                Ok(a[0 .. m - 1, m..] |> Matrix.create)
            else
                // find the pivot
                let i_max = [ h .. m - 1 ] |> argmax (fun i -> abs a[i, k])

                if a[i_max, k] = Rational.Zero then
                    Error(sprintf "No pivot in column %d" k)
                else
                    swap h i_max
                    // zero out the lower triangle
                    for i in h + 1 .. m - 1 do
                        let f = a[i, k] / a[h, k]
                        a[i, k] <- Rational.Zero

                        for j in k + 1 .. n - 1 do
                            a[i, j] <- a[i, j] - a[h, j] * f

                    gaussElimination (h + 1) (k + 1)

        gaussElimination 0 0

    static member inverse m =
        match Matrix.tryInverse m with
        | Ok m' -> m'
        | Error msg -> failwith msg

    static member toArray m = m.A

    static member toString m =
        m.A |> Array2D.map Rational.toDecimal |> array2DToString

type Ray2 =
    { Start: Vector2
      Direction: Vector2 }

    static member valid l (p: decimal * decimal) =
        let dx = fst p - l.Start.X
        let dy = snd p - l.Start.Y
        sign dx = sign l.Direction.X && sign dy = sign l.Direction.Y

    static member intersect l0 l1 =
        let a0 = l0.Direction.Y
        let b0 = -l0.Direction.X
        let a1 = l1.Direction.Y
        let b1 = -l1.Direction.X

        let w = a0 * b1 - a1 * b0

        if w = 0m then
            None
        else
            let c0 = -a0 * l0.Start.X - b0 * l0.Start.Y
            let c1 = -a1 * l1.Start.X - b1 * l1.Start.Y
            let x = b0 * c1 - b1 * c0
            let y = a1 * c0 - a0 * c1
            Some(x / w, y / w)

type Ray3 =
    { Start: Vector3
      Direction: Vector3 }

    member this.X = this.Start.X
    member this.Y = this.Start.Y
    member this.Z = this.Start.Z
    member this.DX = this.Direction.X
    member this.DY = this.Direction.Y
    member this.DZ = this.Direction.Z

    static member xy l =
        { Start = Vector3.xy l.Start
          Direction = Vector3.xy l.Direction }
        : Ray2

    static member at l t = l.Start + t * l.Direction

    static member toString v =
        sprintf "%s @ %s" (Vector3.toString v.Start) (Vector3.toString v.Direction)

type Rectangle2 =
    { Left: decimal
      Top: decimal
      Right: decimal
      Bottom: decimal }

    static member create x y w h =
        { Left = x
          Top = y
          Right = x + w
          Bottom = y + h }

    static member inside r (x, y) =
        not (x < r.Left || x > r.Right || y < r.Top || y > r.Bottom)