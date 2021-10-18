module day03

let splitter (string:string) =
    string.Split ' '
        |> Array.map (String.filter System.Char.IsNumber)
        |> Array.filter (fun a -> a.Length <> 0)
        |> Array.map int

let isPossibleTriangleList (arr: int list):bool = 
    match List.sort arr with
        | [a; b; c] -> 
            c < (a + b)
        | _ -> failwith "not a triangle"
    
let isPossibleTriangle (arr: int[]):bool = 
    match Array.sort arr with
        | [|a; b; c|] -> 
            c < (a + b)
        | _ -> failwith "not a triangle"

let parse (input:string) = 
    input.Split '\n'
        |> Array.map splitter
        |> Array.filter (fun a -> a.Length <> 0)



//https://hackage.haskell.org/package/base-4.15.0.0/docs/src/Data-OldList.html#transpose
//transpose               :: [[a]] -> [[a]]
//transpose []             = []
//transpose ([]   : xss)   = transpose xss
//transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])

let rec transpose2 (input:'a list list):'a list list = 
    match input with 
            | [] -> []
            | [] :: xss -> transpose2 xss
            | (x :: xs) :: xss ->
                (x :: [ for hs in xss do if hs.Length > 0 then hs.Head ]) 
                    :: transpose2 (xs :: [for ts in xss do if ts.Length > 0 then ts.Tail])

// 3by3 matrix
let transpose (input:int[][])=
    let mutable result = [];
    for j in 0..2  do
        let mutable inner = [];
        for i in 0..2 do
            inner <- inner @ [input.[i].[j]]
        result <- result @ [inner]
    result 

let transpose3 (input:int list list)=
    [for j in 0..2  do
        yield
            [for i in 0..2 do
                yield input.[i].[j]]]
        
let toPart2 (input:int[][]) =
    Array.chunkBySize 3 input
        |> Array.map (Array.map List.ofArray)
        |> Array.map List.ofArray
        |> List.ofArray
        |> List.map transpose2
        |> List.fold List.append []

let solve (input:string) =
    let triangles = 
        parse input

    //yet i don’t get the right answer
    let possibleTriangles = 
            triangles
            |> Array.filter isPossibleTriangle

    possibleTriangles.Length

    
let solve_2 (input:string) =
    let triangles = 
        parse input
            |> toPart2
    let possibleTriangles =
        triangles
            |> List.filter isPossibleTriangleList

    possibleTriangles.Length


let run = 
    let input = System.IO.File.ReadAllText "Days/day03.txt"

    solve input
        |> printfn "%d"

    solve_2 input
        |> printfn "%d"




