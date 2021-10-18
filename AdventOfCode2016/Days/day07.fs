module day07

open FSharp.Collections

let rec hasABBA (str:string)=
    if str.Length < 4 then
        false
    else
        match str |> Seq.toList with 
            | a :: b :: c :: d :: _ 
                when  a = d && b = c && a <> b -> 
                    true
            | _ ->
                hasABBA (str.Substring 1)

// there can be multiple hypernet sequences
let supportsTLS (ip:string) = 
    let parts = 
        ip.Split [|'['; ']'|]

    let grouped =
        parts
        |> Seq.chunkBySize 2 //smart
        |> Seq.transpose // group them together
        |> Seq.toList

    match grouped with 
        | [normal; hypernet] -> 
            Seq.exists (hasABBA) normal &&
            not  (Seq.exists (hasABBA) hypernet)
        | _ -> failwith "what"

let rec getABA (supernet:string):(char * char) list =
    if supernet.Length < 3 then
        []
    else
        match supernet |> Seq.toList with 
            | a :: b :: c :: _ 
                when  a = c && a <> b -> 
                    (a, b) :: getABA(supernet.Substring 1)
            | _ -> 
                getABA (supernet.Substring 1)
    

// DOESN?T WORK
let rec doTheThing (abas:(char*char) list)(babs:(char*char)list):bool =
    let toBAB (a, b) = 
        (b, a)

    if Seq.length abas = 0 then
        true
    else
        let head = 
            List.head abas
        let idx =
            babs
            |> List.tryFindIndex (fun bab -> bab = (toBAB head))

        let removeAt index list =
            list |> List.indexed |> List.filter (fun (i, _) -> i <> index) |> List.map snd  

        if idx.IsNone then 
            false
        else
            let abas =
                removeAt (List.findIndex ((=) head) abas) abas
            let babs =
                removeAt idx.Value babs
            doTheThing abas babs
    
let supportsSSL (ip:string) =
    let parts = 
        ip.Split [|'['; ']'|]

    let grouped =
        parts
        |> Seq.chunkBySize 2 
        |> Seq.transpose 
        |> Seq.toList

    let toBAB (a, b) = 
        (b, a)

    match grouped with 
        | [normal; hypernet] -> 
            let abas =
                hypernet
                |> Seq.map getABA
                |> Seq.concat
            let babs =
                normal
                |> Seq.map getABA
                |> Seq.concat

            //abas
            //|> Seq.forall  (fun aba -> 
            //    babs
            //    |> Seq.contains (toBAB aba)
            //) && Seq.length abas > 0

            doTheThing (Seq.toList abas) (Seq.toList babs)

        | _ -> failwith "compiler warning 😐😐😐"


let solve_2 (lines: string seq):int =
    let funny =
        lines
        |> Seq.filter supportsSSL

    Seq.length funny


let solve (lines: string seq):int =
    let funny =
        lines
        |> Seq.filter supportsTLS

    Seq.length funny


let run:string =
    let input =
        System.IO.File.ReadAllLines "Days/day07.txt"

    let example = 
        @"aba[bab]xyz
xyx[xyx]xyx
aaa[kek]eke
zazbz[bzb]cdb" |> helpers.lines
    
    example
    |> Seq.filter supportsSSL
    |> Seq.iter (printfn "-> %s")
    

    let result =
        solve input
    let result_2 =
        solve_2 input
    
    $"part one: {result} part two: {result_2}"

