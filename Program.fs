open System
open System.IO

type TokenType =
    | Ident of string
    | Interger of int
    | Col
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
        | ',' ->
            tl.Index <- tl.Index + 1
            tl.Token <- Col
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

let expectTok (tl: TopLevel) (t: TokenType) =
    tokenize tl
    if not (tl.Token = t) then
        printfn "ERROR at line %d: Expected %+A but got %+A" tl.Line t tl.Token
        exit 1

let expectIdent (tl: TopLevel) =
    tokenize tl
    match tl.Token with
    | Ident n -> n
    | other -> 
        printfn "ERROR line %d: Expected Ident, got %A" tl.Line other
        exit 1

let parseMov (tl: TopLevel) =
    let r1 = expectIdent tl
    expectTok tl Col
    let r2 = expectIdent tl
    printfn "r1: %s, r2: %s" r1 r2
        
let rec parse (tl: TopLevel) =
    tokenize tl
    match tl.Token with
    | EOF -> 
        printfn "FINISHED"
    | Ident op ->
        match op with
        | "mov" -> parseMov tl
        | _ ->
            printfn "ERROR at line %d: Unknow op '%s'" tl.Line op
    | Interger i ->
        printfn "TODO INTEGER"
        exit 1

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
