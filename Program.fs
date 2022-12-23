open System
open System.IO

let mutable x = 0
let mutable y = 0
let mutable ended = false
let mutable text = [||]
let mutable move = false
let mutable saved = true
let wh = Console.WindowHeight - 6;
let mutable wp = 0  // win pos
let mutable csp = 0 // cursor pos

let display() =
    Console.Clear()
    if move then
        Console.ForegroundColor <- ConsoleColor.Green
        printf "MODE: CONTROL  "
        Console.ForegroundColor <- ConsoleColor.White
        if saved then printfn "(saved)\n" else printfn "(not saved)\n"
    else
        Console.ForegroundColor <- ConsoleColor.Red
        printf "MODE: EDITOR   "
        Console.ForegroundColor <- ConsoleColor.White
        if saved then printfn "(saved)\n" else printfn "(not saved)\n"

let isNumber(s: string) = 
    s |> Seq.forall Char.IsDigit

let readLines (filePath: string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine()
}

let shiftArray(selected_text: string) =
    text <- Array.append text [|""|]
    for i in text.Length - 1 .. -1  .. y + 1 do text.[i] <- text.[i-1]
    text.[y] <- selected_text.[0 .. x - 1]
    text.[y+1] <- selected_text.[x .. selected_text.Length - 1]

    if text.[y+1] = "" then y <- y + 1

let print(keywords: string[]) =
    display()
    for i in wp .. Math.Min(wp + wh, text.Length - 1) do
        let line = text.[i]
        let mutable cw = ""
        for i in 0 .. line.Length - 1 do
            if not(line.[i] = ' ') && not(i = line.Length - 1) then
                cw <- cw + string line.[i]
            else
                if (i = line.Length - 1) then cw <- cw + string line.[i]

                if Array.contains (cw.Trim()) keywords then Console.ForegroundColor <- ConsoleColor.Yellow
                elif isNumber (cw.Trim()) then Console.ForegroundColor <- ConsoleColor.DarkCyan
                printf "%s " cw 
                Console.ForegroundColor <- ConsoleColor.White
                cw <- ""
        printfn ""

[<EntryPoint>]
let main argv =
    
    let mutable keywords = [||]
    let extension = argv.[0].Split '.'
    match extension.[extension.Length - 1] with
        | x when (x = "c" || x = "cs" || x = "cpp" || x = "h" || x = "hpp") -> keywords <- [|"void"; "int"; "float"; "double"; "if"; "else"; "while"; "for"; "#include"; "#define"; "struct"; "class"; "static"; "private"; "public"|]
        | "py" -> keywords <- [|"if"; "else"; "while"; "True"; "False"; "int"; "str"|]
        | "rs" -> keywords <- [|"let"; "mut"; "i8"; "u8"; "i16"; "u16"; "i32"; "u32"; "i64"; "u64"; "i128"; "u128"; "usize"; "if"; "else"; "while"; "loop"; "struct"; "impl"; "fn"; "vec!"|]
        | _ -> keywords <- [||]

    let lines = readLines argv.[0]
    for l in lines do
        text <- Array.append text [|l|];

    Console.WriteLine(text.Length)

    Console.ForegroundColor <- ConsoleColor.White
    Console.Clear()
    Console.WriteLine(wh)
    while not(ended) do
        let mutable selected_text = text.[y]
        if x >= selected_text.Length then x <- selected_text.Length
        print keywords
        Console.SetCursorPosition(x, y + 2)

        let k = Console.ReadKey().KeyChar
        saved <- false
        match int(k) with
        | 119 -> (
                    if move then
                        if y > 0 then y <- y - 1
                        if csp > 0 then csp <- csp - 1
                        else
                            if wp > 0 then
                                wp <- wp - 1
                    else
                        text.[y] <- selected_text.[0 .. x - 1] + "w" + selected_text.[x .. selected_text.Length - 1]; x <- x + 1
                )
        | 115 -> (
                    if move then
                        if y < Math.Min(wh, text.Length - 1) then y <- y + 1
                        if csp < wh then csp <- csp + 1
                        else
                            if wp < text.Length - wh && text.Length > wh then
                                wp <- wp + 1
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
        Console.Clear()
    0
