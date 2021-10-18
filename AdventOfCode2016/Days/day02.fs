module day02

type Button =
    | Char of ch:char
    | Nothing

let code = [|
    [|'1'; '2'; '3'|];
    [|'4'; '5';'6'|];
    [|'7'; '8'; '9'|]
|]

let code_2: char option[][] = [|
    [|None; None; None; None; None; None; None|];
    [|None; None; None; Some '1'; None; None; None|];
    [|None; None; Some '2'; Some '3'; Some '4'; None; None|];
    [|None; Some '5'; Some '6';  Some '7'; Some '8'; Some '9'; None|]
    [|None; None; Some 'A'; Some 'B' ; Some 'C'; None; None|];
    [|None; None; None; Some 'D'; None; None; None|];
    [|None; None; None; None; None; None; None|];
|]

let row_col_to_code_2 (row, col) = 
    code_2.[row].[col].Value

let move_on_pad (row, col) ch =

    let (i, j) = match ch with 
                    | 'U' -> (-1, 0)
                    | 'D' -> (1, 0)
                    | 'L' -> (0, -1)
                    | 'R' -> (0, 1)
                    | token  -> (0, 0) // let’s just ignore it 

    let (row, col) = (row + i, col + j)


    // can’t be under 0, nor over 2
    (min (max row 0) 2, min (max col 0) 2)
    
let move_on_pad_2 (row, col) ch =

    let (i, j) = match ch with 
                    | 'U' -> (-1, 0)
                    | 'D' -> (1, 0)
                    | 'L' -> (0, -1)
                    | 'R' -> (0, 1)
                    | token  -> printfn $"unexpected token: >>{System.Char.GetNumericValue(token)}<<"
                                (0, 0)

    let (i, j) = (row + i, col + j)

    let (row, col) = if code_2.[i].[j].IsSome then
                        (i, j) // do move
                     else
                        (row, col) // don’t move 

    // can’t be under 0, nor over 2
    (row, col)
    

let folder state (value:string) = 
    let (arr, (row, col)) = state

    let (i, j) = value.ToCharArray ()
                |> Array.fold move_on_pad (row, col)

    
    (arr @ [(i, j)], (i, j))

let folder_2 state (value:string) = 
    let (arr, (row, col)) = state

    let (i, j) = value.ToCharArray ()
                |> Array.fold move_on_pad_2 (row, col)

    
    (arr @ [(i, j)], (i, j))

let row_col_to_code (row, col) = 
    code.[row].[col]

let solve (input:string) = 

    let result = input.Split '\n'
                    |> Array.filter (fun str -> str.Length <> 0)
                    |> Array.fold folder ([], (1, 1))
                    |> fst // get first element of tuple
                    |> List.map row_col_to_code
                    |> System.String.Concat
                    
    result

let solve_2 (input:string) = 

    let result = input.Split '\n'
                    |> Array.filter (fun str -> str.Length <> 0)
                    |> Array.fold folder_2 ([], (3, 1))
                    |> fst // get first element of tuple
                    |> List.map row_col_to_code_2
                    |> System.String.Concat
                    
    result

let run = 
    let example_1 = "ULL\nRRDDD\nLURDL\nUUUUD"    
                    |> solve
    let example_2 = "ULL\nRRDDD\nLURDL\nUUUUD"    
                    |> solve_2
    assert (example_1 = "1985")
    assert (example_2 = "5DB3")
    printfn "example works"

    let input = System.IO.File.ReadAllText "Days/day02.txt"

    input 
        |> solve
        |> printfn "result: %s" 

    input 
        |> solve_2
        |> printfn "result 2: %s" 
    ()


         

