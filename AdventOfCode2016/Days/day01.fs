module day01

type State = {
    direction: int
    x: int
    y: int
}

//type Point(x:double, y:double) = 
//    member this.x = x
//    member this.y = y
//    new() = Point(0., 0.)
//
//    static member (+) (a:Point,b:Point) = 
//        Point(a.x + b.x, a.y + b.y)
//    static member (-) (a:Point,b:Point) = 
//        Point(a.x - b.x, a.y - b.y)

type Point = {
    x: double
    y: double
}
with
    static member (+) (a:Point, b:Point) =
        {x = a.x + b.x; y = a.y + b.y}
    static member (+) (a:Point, (x, y)) =
        {x = a.x + x; y = a.y + y}
    static member Default = { x = 0.; y = 0. }
    override this.ToString() = 
        $"({this.x}, {this.y})"
        

type State2 = {
    direction: int
    point: Point
    past: Point list
    alreadyPast: Point list
}
with
    static member Default = {direction = 0; point = Point.Default; past = []; alreadyPast = []}

type Turn = Left | Right

let toTurn (input:string)=
    match input.ToCharArray() |> List.ofArray with
        | [] -> failwith "empty input"
        | _ :: [] -> failwith "no distnce"
        | head :: tail -> 
            let number =  tail 
                        |> System.String.Concat 
                        |> int
            match head with
                | 'R'   -> (Right, number)
                | 'L'   -> (Left, number)
                | _     -> failwith "wrong turn character"


let move (state:State) (value:Turn * int):State = 

    let (turn, amount) = value

    let toAdd = 
        match turn with
        | Right -> 1
        | Left -> 3

    let newDir = (state.direction + toAdd) % 4

    let (x, y) = match newDir with
                        | 0 -> (0, amount)  // North
                        | 1 -> (amount, 0)  // East
                        | 2 -> (0, -amount) // South
                        | 3 -> (-amount, 0) // West
                        | _ -> failwith "can’t happen"

    {direction = newDir; x = state.x + x; y = state.y + y}

type Line(a:Point,b:Point) =
    member this.a = a
    member this.b = b
    new() = Line(Point.Default, Point.Default)

    override this.ToString() = 
        $"{this.a} --> {this.b}"

    // https://stackoverflow.com/a/1968345
    member this.intersect (other:Line):Option<Point> =
        let s1_x = this.b.x - this.a.x 
        let s1_y = this.b.y - this.a.y 

        let s2_x = other.b.x - other.a.x 
        let s2_y = other.b.y - other.a.y

        let s = (-s1_y * (this.a.x - other.a.x) + s1_x * (this.a.y - other.a.y)) / (-s2_x * s1_y + s1_x * s2_y)
        let t = ( s2_x * (this.a.y - other.a.y) - s2_y * (this.a.x - other.a.x)) / (-s2_x * s1_y + s1_x * s2_y)

        if (s >= 0. && s <= 1. && t >= 0. && t <= 1.) then
            let new_x = this.a.x + (t * s1_x);
            let new_y = this.a.y + (t * s1_y);
            Some({x = new_x; y = new_y})
        else
            None


let move_2 (state:State2) (value:Turn * int):State2 = 
    let (turn, amount) = value

    let toAdd = 
        match turn with
        | Right -> 1
        | Left -> 3

    let newDir = (state.direction + toAdd) % 4

    let (x, y) = match newDir with
                        | 0 -> (0, amount)  // North
                        | 1 -> (amount, 0)  // East
                        | 2 -> (0, -amount) // South
                        | 3 -> (-amount, 0) // West
                        | _ -> failwith "can’t happen"

    let newPoint = state.point + (double x, double y)


    let maybeIntersection a1 a2 =
        let lineA = Line(a1, a2)
        let lineB = Line(newPoint, state.point)

        lineA.intersect lineB // returns an option with intersection


    let intersections = if state.past.Length >= 1 then
                            List.map2 maybeIntersection (List.take (state.past.Length - 1) state.past) (state.past.Tail) |> List.choose id
                        else
                            []

    {
        direction = newDir; 
        point = newPoint; 
        past = state.point :: state.past;
        alreadyPast = state.alreadyPast @ (
                        match intersections with 
                            | point :: _ -> [point]
                            | _ -> []
        );
    }


let solve_2 (input:string):int =
    let state = input.Split ", "
                |> Array.map toTurn
                |> Array.fold move_2 State2.Default

    let p = state.alreadyPast.Head


    abs(p.x) + abs(p.y) |> int
                    

let solve (input:string):int =
    let state = input.Split ", " 
                |> Array.map toTurn
                |> Array.fold move {direction = 0; x = 0; y = 0}

    abs(state.x) + abs(state.y)

let run =
    solve "R5, L5, R5, R3" |> printfn "example part 1, should be 12:  %d"

    let input = System.IO.File.ReadAllText("Days/day01.txt")

    solve input
        |> printfn "part 1 result %d"



    "R8, R4, R4, R8"
        |> solve_2
        |> printfn "part 2 example result %d"

    input
        |> solve_2
        |> printf "part 2 result %d"


