module day05

//slow…
let hash(str:string):byte[] = 
    use md5 = System.Security.Cryptography.MD5.Create()
    let bytes = System.Text.Encoding.ASCII.GetBytes str
    let hash = md5.ComputeHash bytes

    hash

let hashToString bytes =
    System.BitConverter.ToString(bytes).Replace("-", "")

let isCorrect (hash:byte[]) =
        hash
            |> Array.take 2  //00_00_0x
            |> Array.forall ((=) (byte 0))
        && ((hash.[2] &&& (byte 0xF0)) = (byte 0))


let generateHashesFromCode (code:string) =
    Seq.initInfinite id
        |> Seq.map (sprintf "%s%d" code)
        |> Seq.map hash
        |> Seq.filter isCorrect
        |> Seq.map hashToString

let generatePassword (hashes: string seq) = 
    hashes
        |> Seq.map (Seq.item 5) // get sixth letter
        |> Seq.take 8 
        |> Seq.toArray
        |> System.String.Concat

let charToDigit ch =
    if System.Char.IsDigit ch |> not then 
        None
    else
        (int ch) - (int '0') 
            |> Some

let rec makeString (array:char option [])(strings: string seq):string =
    if Array.exists Option.isNone array then
        let string = Seq.head strings
        let (pos, ch) = (charToDigit string.[5], string.[6])

        match pos with 
            | Some pos when pos < array.Length 
                            && pos >= 0  
                            // can’t be already occupied
                            && (Option.isNone array.[pos]) -> 
                let newArray = 
                    Array.init (array.Length) (fun i -> if i = pos then Some(ch) else array.[i])
                makeString newArray (Seq.tail strings)
            | _ -> 
                makeString array (Seq.tail strings)
    else
        array
            |> Array.map Option.get
            |> System.String.Concat

let generatePassword2 (hashes: string seq) = 
    hashes
        |> makeString (Array.create 8 None)

let run:string =
    let input = System.IO.File.ReadAllLines("Days/day05.txt").[0]


    let hashes = 
        generateHashesFromCode input
            |> Seq.cache // this means the first eight won’t have to be computed for part 2


    $"part one: {generatePassword hashes}, part two: {generatePassword2 hashes}"

