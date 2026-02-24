namespace Onizuka

open System

type TokenType =
    | Ident of string
    | Integer of int
    | Col
    | DoubleCol
    | Label of string
    | EOF

type TopLevel = {
    Src: string
    mutable Index: int
    mutable Line: int
    mutable Token: TokenType
    mutable Regs: int option[]
    mutable Labels: Map<string, int>
    mutable ReturnStack: int list
}

module Lexer =
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
            | ':' ->
                tl.Index <- tl.Index + 1
                tl.Token <- DoubleCol
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

                if tl.Index < tl.Src.Length && tl.Src[tl.Index] = ':' then
                    tl.Index <- tl.Index + 1
                    tl.Token <- Label (buff)
                else
                    tl.Token <- Ident (buff)
            | _ ->
                printfn "ERROR at line %d: Unknow char '%c'" tl.Line c
                exit 1
        else
            tl.Token <- EOF

    let collectLabels (tl: TopLevel) =
        let rec scan () =
            tokenize tl
            match tl.Token with
            | Label name -> 
                tl.Labels <- tl.Labels.Add(name, tl.Index)
                scan ()
            | EOF -> 
                tl.Index <- 0
                tl.Line <- 1
            | _ -> scan ()
        scan ()

