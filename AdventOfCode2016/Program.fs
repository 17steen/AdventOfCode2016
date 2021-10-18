
[<EntryPoint>]
let main args = 
    printfn "Hello from F#"
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let output = day07.run 
    sw.Stop()

    // one tick every 1/10µs
    let µs = (double sw.ElapsedTicks) / ((double System.Diagnostics.Stopwatch.Frequency) / 1000. / 1000.)

    sw.Elapsed.Milliseconds 
        |> printfn "this took %d ms"

    µs 
        |> printfn "this took %f µs"

    printfn "output: %s" output

    0
