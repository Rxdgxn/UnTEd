open System
open System.IO

let mutable x = 0
let mutable y = 0
let mutable ended = false
let mutable text = [|"Hello there"; "General Kenobi"|]
let mutable move = false

let display(content: string[]) =
    Console.Clear()
    if move then
        Console.ForegroundColor <- ConsoleColor.Green
        printfn "MODE: CONTROL\n"
        Console.ForegroundColor <- ConsoleColor.White
    else
        Console.ForegroundColor <- ConsoleColor.Red
        printfn "MODE: EDITOR\n"
        Console.ForegroundColor <- ConsoleColor.White
    for l in content do
        printfn "%s" l

let shiftArray(selected_text: string) =
    text <- Array.append text [|""|]
    for i in text.Length - 1 .. -1  .. y + 1 do text.[i] <- text.[i-1]
    text.[y] <- selected_text.[0 .. x - 1]
    text.[y+1] <- selected_text.[x .. selected_text.Length - 1]

    if text.[y+1] = "" then y <- y + 1


[<EntryPoint>]
let main argv =
    
    Console.ForegroundColor <- ConsoleColor.White
    Console.Clear()

    while not(ended) do
        let mutable selected_text = text.[y]
        if x >= selected_text.Length then x <- selected_text.Length
        display text
        Console.SetCursorPosition(x, y + 2)
        let k = Console.ReadKey().KeyChar
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
        | 8   -> if not move then text.[y] <- selected_text.[0 .. x - 2] + selected_text.[x .. selected_text.Length - 1]; x <- x - 1  // Backspace
        | 26  -> move <- not move  // Ctrl + Z
        | 13  -> if not(move) then shiftArray selected_text  // Enter
        | 19  -> (
                    Console.Clear()
                    printfn "Enter your filename: "
                    let f = Console.ReadLine()
                    let mutable content = ""
                    
                    for l in text do
                        content <- content + l + "\n"
                    
                    File.WriteAllText(Directory.GetCurrentDirectory() + "\\" + f, content)
                )
        | 18 -> Console.Clear(); exit(1)
        | c   -> if not move then text.[y] <- selected_text.[0 .. x - 1] + string(char(c)) + selected_text.[x .. selected_text.Length - 1]; x <- x + 1

    0