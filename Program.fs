open System
open System.IO

let registers = [|"r1"; "r2"; "r3"; "r4"; "r5"; "r6"; "r7"; "r8"|];

type TokenType =
    | Ident of string
    | Integer of int
    | Col
    | EOF

type TopLevel = {
    Src: string
    mutable Index: int
    mutable Line: int
    mutable Token: TokenType
    mutable Regs: int option[]
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
                
            tl.Token <- Integer (int buff)
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
        printfn "ERROR at line %d: Expected Ident, got %A" tl.Line other
        exit 1

let parseMov (tl: TopLevel) =
    let r1 = expectIdent tl
    if not(Array.contains r1 registers) then
        printfn "ERROR at line %d: Unknow register '%s'\nList of registers %+A" tl.Line r1 registers
        exit 1

    let indexRegLeft = int (Char.GetNumericValue(r1.[r1.Length - 1]))        
    
    expectTok tl Col
    tokenize tl
    match tl.Token with
    | Ident r2 ->
        if not(Array.contains r2 registers) then
            printfn "ERROR at line %d: Unknow register '%s'\nList of registers %+A" tl.Line r1 registers
            exit 1

        let indexRegRight = int (Char.GetNumericValue(r2.[r2.Length - 1]))
        tl.Regs[indexRegRight] <- tl.Regs[indexRegLeft]
    | Integer n ->
        tl.Regs[indexRegLeft] <- Some n
    | _ ->
        printfn "ERROR at line %d: Expected a number or a register but got: %+A" tl.Line tl.Token
        exit 1
        
let rec parse (tl: TopLevel) =
    tokenize tl
    match tl.Token with
    | EOF -> 
        printfn "Finished"
    | Ident op ->
        match op with
        | "mov" ->
            parseMov tl
            parse tl
        | _ ->
            printfn "ERROR at line %d: Unknow op '%s'" tl.Line op
            exit 1
    | _ ->
        printfn "ERROR at line %d: Unknow op '%+A'" tl.Line tl.Token
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
            Regs = Array.create registers.Length None
        }
        parse tl
    | None -> exit 1

    0
