open System
open System.IO
open Argu
open Onizuka
open Onizuka.Lexer
open Onizuka.Parser
open Onizuka.Interpreter

type Arguments =
    | [<AltCommandLine("-v")>] Version
    | [<MainCommand>] File of path:string
        
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Version -> "print the current onizuka version."
            | File _ -> "the .on file to execute."

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
                    let info = Parser.analyzeSource res
                    for d in info.Diagnostics do
                        printfn "%s at line %d: %s" d.Kind d.Line d.Message
                    Interpreter.run info
                | None -> 1
            | None ->
                printfn "Usage: onizuka <file.on> [options]"
                printfn "%s" (parser.PrintUsage())
                1
    with ex ->
        printfn "%s" ex.Message
        1
