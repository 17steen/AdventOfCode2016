module day06

open System.IO
open System.Diagnostics



let solve (input:string []) =
    
    input
    |> Array.map (fun a -> a.ToCharArray())
    |> Array.transpose
    |> Seq.map (fun seq ->
        seq
        |>
            Seq.fold (fun map ch -> 
                Map.change ch (fun opt ->
                    match opt with
                        | Some amount -> amount + 1
                        | None -> 1
                    |> Some
                ) map
            ) Map.empty

        |> Map.toSeq
        |> Seq.sortByDescending snd
        |> Seq.head 
        |> fst
    )
    |> System.String.Concat

let solve_2 (input:string []) =
    
    input
    |> Array.map (fun a -> a.ToCharArray())
    |> Array.transpose
    |> Seq.map (fun seq ->
        seq
        |>
            Seq.fold (fun map ch -> 
                Map.change ch (fun opt ->
                    match opt with
                        | Some amount -> amount + 1
                        | None -> 1
                    |> Some
                ) map
            ) Map.empty

        |> Map.toSeq
        |> Seq.sortBy snd
        |> Seq.head 
        |> fst
    )
    |> System.String.Concat

let run:string =
    let example = 
        @"eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar"
    
    example
        |> helpers.lines
        |> solve
        |> (=) "easter"
        |> Debug.Assert

    example
        |> helpers.lines
        |> solve_2
        |> (=) "advent"
        |> Debug.Assert

    let input = 
        File.ReadAllLines "Days/day06.txt"

    $"{solve input} {solve_2 input}"

