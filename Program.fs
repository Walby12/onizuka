open System
open System.IO

type TokenType =
    | Ident of string
    | Interger of int
    | EOF

type TopLevel = {
    Src: string
    mutable Index: int
    mutable Line: int
    mutable Token: TokenType
}

let readFile (path: string) =
    try
        let text = File.ReadAllText(path)
        Some text
    with
    | :? FileNotFoundException -> 
        printfn "File %s not found!" path
        None
    | err -> 
        printfn "An error occurred: %s" err.Message
        None

let rec tokenize (tl: TopLevel) =
    if tl.Index < tl.Src.Length then
        let c = tl.Src[tl.Index]

        match c with
        | '\n' ->
            tl.Line <- tl.Line + 1
            tl.Index <- tl.Index + 1
            tokenize tl
        | c when Char.IsWhiteSpace(c) ->
            tl.Index <- tl.Index + 1
            tokenize tl
        | c when Char.IsDigit(c) ->
            let mutable buff = ""
            while tl.Index < tl.Src.Length && Char.IsDigit(tl.Src[tl.Index]) && not (Char.IsWhiteSpace(tl.Src[tl.Index])) do
                buff <- buff + (string tl.Src[tl.Index])
                tl.Index <- tl.Index + 1
                
            tl.Token <- Interger (int buff)
        | c when Char.IsLetter(c) ->
            let mutable buff = ""
            while tl.Index < tl.Src.Length && Char.IsLetterOrDigit(tl.Src[tl.Index]) && not (Char.IsWhiteSpace(tl.Src[tl.Index])) do
                buff <- buff + (string tl.Src[tl.Index])
                tl.Index <- tl.Index + 1
                
            tl.Token <- Ident (buff)
        | _ ->
            printfn "ERROR at line %d: Unknow char '%c'" tl.Line c
            exit 1
    else
        tl.Token <- EOF
        
let rec parse (tl: TopLevel) =
    tokenize tl
    match tl.Token with
    | EOF -> 
        printfn "FINISHED"
    | Ident op ->
        match op with
        | "mov" -> printfn "NICE mov"
        | _ ->
            printfn "ERROR at line %d: Unknow op '%s'" tl.Line op
    | Interger i ->
        printfn "Found standalone integer: %d" i
        parse tl

[<EntryPoint>]
let main _ =
    match readFile "test.on" with
    | Some res ->
        let tl = {
            Src = res
            Index = 0
            Line = 1
            Token = Ident ("START")
        }
        parse tl
    | None -> exit 1

    0
