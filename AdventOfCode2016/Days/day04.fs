module day04

/// <summary>
/// returns 0 if invalid
/// </summary>
let toRoomId (name:string) =
    //aaaaa-bbb-z-y-x-123[abxyz]
    let parts = name.Split '-'
    let len = parts.Length
    let letters = Seq.take (len - 1) parts 
                    |> System.String.Concat

    let rest = parts.[len - 1] // 123[abxyz]

    let (id, checkSum) = 
        match rest.Split '[' with
            | [|a; b|] -> (int a, b.TrimEnd ']')
            | _ -> failwith "dumb"

    let letterFrequency (state:Map<char, int>) (ch:char):Map<char, int> =
            state |>
                Map.change ch (fun opt -> 
                    match opt with
                        | Some(count) -> Some(count + 1)
                        | None -> Some(1)
                ) 

    let counts = letters
                     |> Seq.fold letterFrequency (Map []) // current letter count

    let compare (ch1:char, len1:int) (ch2:char, len2:int) =
        let res = len2 - len1
        if res = 0 then
            ch1.CompareTo ch2
        else
            res

    let generatedChecksum =
        counts
            |> Map.toSeq
            |> Seq.sortWith compare
            |> Seq.map fst
            |> Seq.take 5 //failure point
            |> Seq.toArray
            |> System.String

    if generatedChecksum = checkSum then
        id
    else
        0

let solve (input: string seq) =
    let result = 
        input
            |> Seq.map toRoomId
            |> Seq.reduce (+)
    result


let shiftLetter (amount:int) (ch:char):char =
    assert(System.Char.IsLetter ch)

    let numericValue = 
        match (int ch) - (int 'a') with
                        | num when num >= 0 && num <= 26 -> num
                        | _ -> failwith "unexpected letter"
    let shifted = 
        (numericValue + amount) % 26

    let result =
        char (shifted + (int 'a'))
    result
    


let decypher (name:string):string *int =
    //aaaaa-bbb-z-y-x-123[abxyz]
    let parts = name.Split '-'
    let len = parts.Length

    let rest = parts.[len - 1] // 123[abxyz]


    let (id, _checkSum) = 
        match rest.Split '[' with
            | [|a; b|] -> (int a, b.TrimEnd ']')
            | _ -> failwith "dumb"

    let letters = Seq.take (len - 1) parts 
                    |> Seq.map (String.map (shiftLetter id)) 
                    //|> System.String.Join ' '
    let decryptedString = 
        System.String.Join(' ', letters)

    (decryptedString, id)


//part2 begins here
let solve_2 (input: string seq):int = 
    let decyphered = 
        input
            |> Seq.map decypher
            |> Seq.filter (fun (str,_) -> str.Contains "object storage")

    let result =
        Seq.head decyphered

    snd result

let run =
    let example = 
        @"aaaaa-bbb-z-y-x-123[abxyz]
a-b-c-d-e-f-g-h-987[abcde]
not-a-real-room-404[oarel]
totally-real-room-200[decoy]".Split "\r\n" // i really hate this

    let exampleResult = solve example
    assert(exampleResult = 1514)

    assert((decypher "qzmt-zixmtkozy-ivhz-343[abcde]" |> fst) = "very encrypted name")

    let input = System.IO.File.ReadAllLines "Days/day04.txt" 

    $"part1: {solve input} part2: {solve_2 input}"

