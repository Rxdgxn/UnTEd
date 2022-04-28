open System
open System.IO

let mutable x = 0
let mutable y = 0
let mutable ended = false
let mutable text = [||]
let mutable move = false
let mutable saved = true

let display() =
    Console.Clear()
    if move then
        Console.ForegroundColor <- ConsoleColor.Green
        printf "MODE: CONTROL   "
        Console.ForegroundColor <- ConsoleColor.White
        if saved then printfn "(saved)\n" else printfn "(not saved)\n"
    else
        Console.ForegroundColor <- ConsoleColor.Red
        printf "MODE: EDITOR   "
        Console.ForegroundColor <- ConsoleColor.White
        if saved then printfn "(saved)\n" else printfn "(not saved)\n"

let isNumber(s: string) = 
    s |> Seq.forall Char.IsDigit

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let shiftArray(selected_text: string) =
    text <- Array.append text [|""|]
    for i in text.Length - 1 .. -1  .. y + 1 do text.[i] <- text.[i-1]
    text.[y] <- selected_text.[0 .. x - 1]
    text.[y+1] <- selected_text.[x .. selected_text.Length - 1]

    if text.[y+1] = "" then y <- y + 1

let printC() =
    let KEYWORDS = [|"int"; "float"; "double"; "void"; "if"; "else"; "for"; "while"|]
    for line in text do
        let mutable cw = ""
        for i in 0 .. line.Length - 1 do
            if not(line.[i] = ' ') && not(i = line.Length - 1) then
                cw <- cw + string line.[i]
            else
                if (i = line.Length - 1) then cw <- cw + string line.[i]

                if Array.contains (cw.Trim()) KEYWORDS then Console.ForegroundColor <- ConsoleColor.Yellow
                elif isNumber (cw.Trim()) then Console.ForegroundColor <- ConsoleColor.DarkCyan
                //elif cw.[0] = '\"' && cw.[cw.Length - 1] = '\"' then Console.ForegroundColor <- ConsoleColor.Green;
                printf "%s " cw 
                Console.ForegroundColor <- ConsoleColor.White
                cw <- ""
        printfn ""

[<EntryPoint>]
let main argv =
    
    let lines = readLines argv.[0]
    for l in lines do
        text <- Array.append text [|l|];

    Console.ForegroundColor <- ConsoleColor.White
    Console.Clear()

    while not(ended) do
        let mutable selected_text = text.[y]
        if x >= selected_text.Length then x <- selected_text.Length
        display()
        printC()
        Console.SetCursorPosition(x, y + 2)

        let k = Console.ReadKey().KeyChar
        saved <- false
        match int(k) with
        | 119 -> (
                    if move then 
                        if y > 0 then 
                            y <- y - 1
                    else
                        text.[y] <- selected_text.[0 .. x - 1] + "w" + selected_text.[x .. selected_text.Length - 1]; x <- x + 1
                )
        | 115 -> (
                    if move then
                        if y < text.Length - 1 then
                            y <- y + 1 
                    else 
                        text.[y] <- selected_text.[0 .. x - 1] + "s" + selected_text.[x .. selected_text.Length - 1]; x <- x + 1 
                )
        | 100 -> (
                    if move then 
                        if x < selected_text.Length then
                            x <- x + 1 
                    else 
                        text.[y] <- selected_text.[0 .. x - 1] + "d" + selected_text.[x .. selected_text.Length - 1]; x <- x + 1
                )
        | 97  -> (
                    if move then
                        if x > 0 then
                            x <- x - 1 
                    else 
                        text.[y] <- selected_text.[0 .. x - 1] + "a" + selected_text.[x .. selected_text.Length - 1]; x <- x + 1 
                )
        | 8   -> if not move then // Backspace
                    if x > 0 then text.[y] <- selected_text.[0 .. x - 2] + selected_text.[x .. selected_text.Length - 1]; x <- x - 1
                    else (
                        if y > 0 then
                            x <- text.[y-1].Length
                            text.[y-1] <- text.[y-1] + selected_text
                            text <- text |> Array.filter ((<>) text.[y])
                            y <- y - 1
                    )
        | 26  -> move <- not move  // Ctrl + Z
        | 13  -> if not(move) then shiftArray selected_text  // Enter
        | 19  -> (
                    let mutable content = ""
                    for l in text do
                        content <- content + l + "\n"
                    File.WriteAllText(Directory.GetCurrentDirectory() + "\\" + argv.[0], content)
                    saved <- true
                )
        | 18 -> Console.Clear(); exit(1)
        | c   -> if not move then text.[y] <- selected_text.[0 .. x - 1] + string(char(c)) + selected_text.[x .. selected_text.Length - 1]; x <- x + 1
            
    0