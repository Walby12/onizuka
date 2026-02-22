open System
open System.IO
open Argu

type Arguments =
    | [<AltCommandLine("-v")>] Version
    | [<MainCommand>] File of path:string
        
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Version -> "print the current onizuka version."
            | File _ -> "the .on file to execute."

let registers = [|"r1"; "r2"; "r3"; "r4"; "r5"; "r6"; "r7"; "r8"; "r9"; "r10"|];

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

let getIdx (regName: string) =
    int (regName.Substring(1)) - 1

let parseMov (tl: TopLevel) =
    let r1 = expectIdent tl
    if not(Array.contains r1 registers) then
        printfn "ERROR at line %d: Unknow register '%s'\nList of registers %+A" tl.Line r1 registers
        exit 1

    let indexRegLeft = getIdx r1        
    
    expectTok tl Col
    tokenize tl
    match tl.Token with
    | Ident r2 ->
        if not(Array.contains r2 registers) then
            printfn "ERROR at line %d: Unknow register '%s'\nList of registers %+A" tl.Line r1 registers
            exit 1

        let indexRegRight = getIdx r2
        match tl.Regs[indexRegRight] with
        | Some _ -> tl.Regs[indexRegLeft] <- tl.Regs[indexRegRight]
        | None ->
            printfn "ERROR at line %d: Register %s is empty" tl.Line r2
            exit 1
    | Integer n ->
        tl.Regs[indexRegLeft] <- Some n
    | _ ->
        printfn "ERROR at line %d: Expected a number or a register but got: %+A" tl.Line tl.Token
        exit 1

let parseDump (tl: TopLevel) =
    let reg = expectIdent tl
    if not(Array.contains reg registers) then
        printfn "ERROR at line %d: Unknow register '%s'\nList of registers %+A" tl.Line reg registers
        exit 1

    let indexReg = getIdx reg
    match tl.Regs[indexReg] with
    | Some n -> printfn "%d" n
    | None ->
        printfn "ERROR at line %d: Register %s is empty" tl.Line reg
        exit 1

let parsePop (tl: TopLevel) =
    let reg = expectIdent tl
    if not(Array.contains reg registers) then
        printfn "ERROR at line %d: Unknow register '%s'\nList of registers %+A" tl.Line reg registers
        exit 1

    let indexReg = getIdx reg
    match tl.Regs[indexReg] with
    | Some _ -> tl.Regs[indexReg] <- None
    | None ->
        printfn "ERROR at line %d: Cannot pop register %s because it is already empty" tl.Line reg
        exit 1

let parseAdd (tl: TopLevel) =
    let r1 = expectIdent tl
    if not(Array.contains r1 registers) then
        printfn "ERROR at line %d: Unknow register '%s'\nList of registers %+A" tl.Line r1 registers
        exit 1

    expectTok tl Col
    
    let indexRegLeft = getIdx r1
    match tl.Regs[indexRegLeft] with
    | Some n1 ->
        tokenize tl
        
        match tl.Token with
        | Ident r2 ->
            if not(Array.contains r2 registers) then
                printfn "ERROR at line %d: Unknow register '%s'\nList of registers %+A" tl.Line r2 registers
                exit 1

            let indexRegRight = getIdx r2
            match tl.Regs[indexRegRight] with
            | Some n2 ->
                tl.Regs[indexRegLeft] <- Some (n1 + n2) 
            | None ->
                printfn "ERROR at line %d: Cannot perform add on register %s because it is empty" tl.Line r2
                exit 1
        | Integer n ->
            tl.Regs[indexRegLeft] <- Some (n1 + n)
        | _ ->
            printfn "ERROR at line %d: Excpected a number or a register but got %+A" tl.Line tl.Token
            exit 1
    | None ->
        printfn "ERROR at line %d: Cannot perform add on register %s because it is empty" tl.Line r1
        exit 1

let parseSub (tl: TopLevel) =
    let r1 = expectIdent tl
    if not(Array.contains r1 registers) then
        printfn "ERROR at line %d: Unknow register '%s'\nList of registers %+A" tl.Line r1 registers
        exit 1

    expectTok tl Col
    
    let indexRegLeft = getIdx r1
    match tl.Regs[indexRegLeft] with
    | Some n1 ->
        tokenize tl
        
        match tl.Token with
        | Ident r2 ->
            if not(Array.contains r2 registers) then
                printfn "ERROR at line %d: Unknow register '%s'\nList of registers %+A" tl.Line r2 registers
                exit 1

            let indexRegRight = getIdx r2
            match tl.Regs[indexRegRight] with
            | Some n2 ->
                tl.Regs[indexRegLeft] <- Some (n1 - n2) 
            | None ->
                printfn "ERROR at line %d: Cannot perform sub on register %s because it is empty" tl.Line r2
                exit 1
        | Integer n ->
            tl.Regs[indexRegLeft] <- Some (n1 - n)
        | _ ->
            printfn "ERROR at line %d: Excpected a number or a register but got %+A" tl.Line tl.Token
            exit 1
    | None ->
        printfn "ERROR at line %d: Cannot perform sub on register %s because it is empty" tl.Line r1
        exit 1

let parseCmp (tl: TopLevel) =
    let r1 = expectIdent tl
    if not(Array.contains r1 registers) then
        printfn "ERROR at line %d: Unknow register '%s'\nList of registers %+A" tl.Line r1 registers
        exit 1

    expectTok tl Col
    
    let indexRegLeft = getIdx r1
    match tl.Regs[indexRegLeft] with
    | Some n1 ->
        tokenize tl
        
        match tl.Token with
        | Ident r2 ->
            if not(Array.contains r2 registers) then
                printfn "ERROR at line %d: Unknow register '%s'\nList of registers %+A" tl.Line r2 registers
                exit 1

            let indexRegRight = getIdx r2
            match tl.Regs[indexRegRight] with
            | Some n2 ->
                if n1 = n2 then
                    tl.Regs[indexRegLeft] <- Some 0
                else
                    tl.Regs[indexRegLeft] <- Some 1 
            | None ->
                printfn "ERROR at line %d: Cannot perform cmp on register %s because it is empty" tl.Line r2
                exit 1
        | Integer n ->
            if n1 = n then
                tl.Regs[indexRegLeft] <- Some 0
            else
                 tl.Regs[indexRegLeft] <- Some 1
        | _ ->
            printfn "ERROR at line %d: Excpected a number or a register but got %+A" tl.Line tl.Token
            exit 1
    | None ->
        printfn "ERROR at line %d: Cannot perform cmp on register %s because it is empty" tl.Line r1
        exit 1
    

let rec parse (tl: TopLevel) =
    tokenize tl
    match tl.Token with
    | EOF -> 
        ()
    | Ident op ->
        match op with
        | "mov" ->
            parseMov tl
            parse tl
        | "dump" ->
            parseDump tl
            parse tl
        | "pop" ->
            parsePop tl
            parse tl
        | "add" ->
            parseAdd tl
            parse tl
        | "sub" ->
            parseSub tl
            parse tl
        | "cmp" ->
            parseCmp tl
            parse tl
        | _ ->
            printfn "ERROR at line %d: Unknow op '%s'" tl.Line op
            exit 1
    | _ ->
        printfn "ERROR at line %d: Unexpected token: '%+A'" tl.Line tl.Token
        exit 1

[<EntryPoint>]
let main argv =    
    let parser = ArgumentParser.Create<Arguments>(programName = "onizuka")
    
    try
        let results = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)

        if results.Contains Version then
            printfn "Onizuka Version 0.1"
            0 
        else
            match results.TryGetResult File with
            | Some path ->
                match readFile path with
                | Some res ->
                    let path = results.GetResult File
            
                    match readFile path with
                    | Some res ->
                        let tl = {
                            Src = res
                            Index = 0
                            Line = 1
                            Token = Ident ("START")
                            Regs = Array.create registers.Length None
                        }
                        parse tl
                        0
                    | None -> 1
                | None -> 1
            | None ->
                printfn "Usage: onizuka <file.on> [options]"
                printfn "%s" (parser.PrintUsage())
                1
    with ex ->
        printfn "%s" ex.Message
        1
