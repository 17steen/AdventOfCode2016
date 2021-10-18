module helpers

let lines (str:string):string [] =
    str.Split([|"\r\n"; System.Environment.NewLine|], System.StringSplitOptions.None);  
