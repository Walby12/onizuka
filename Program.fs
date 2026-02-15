open System
open System.IO

type TokenType =
    | Ident of string
    | Interger of int

type TopLevel = {
    Src: string
    mutable Index: int
    mutable Line: int
    mutable Tokens: ResizeArray<TokenType>
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
                
            tl.Tokens.Add(Interger (int buff))
            tokenize tl
        | c when Char.IsLetter(c) ->
            let mutable buff = ""
            while tl.Index < tl.Src.Length && Char.IsLetterOrDigit(tl.Src[tl.Index]) && not (Char.IsWhiteSpace(tl.Src[tl.Index])) do
                buff <- buff + (string tl.Src[tl.Index])
                tl.Index <- tl.Index + 1
                
            tl.Tokens.Add(Ident (buff))
            tokenize tl
        | _ ->
            printfn "ERROR at line %d: Unknow char '%c'" tl.Line c
            exit 1
   
[<EntryPoint>]
let main _ =
    match readFile "test.on" with
    | Some res ->
        let tl = {
            Src = res
            Index = 0
            Line = 1
            Tokens = ResizeArray<TokenType>()
        }
        tokenize tl
        printfn "Tokens: %+A" tl.Tokens
    | None -> exit 1

    0
