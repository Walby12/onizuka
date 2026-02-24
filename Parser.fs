namespace Onizuka

open System

open Onizuka

open Onizuka.Lexer

module Parser =
    type Operand =
        | Reg of string
        | Imm of int

    type HaltOperand =
        | HaltReg of string
        | HaltImm of int

    type Instruction =
        | Mov of string * Operand
        | Dump of string
        | Pop of string
        | Add of string * Operand
        | Sub of string * Operand
        | Cmp of string * Operand
        | Jz of Operand * string
        | Jnz of Operand * string
        | Call of string
        | Ret
        | Halt of HaltOperand

    type ProgramAst = Instruction list

    let registers = [|"r1"; "r2"; "r3"; "r4"; "r5"; "r6"; "r7"; "r8"; "r9"; "r10"|]

    let getIdx (regName: string) =
        int (regName.Substring(1)) - 1

    let expectTok (tl: TopLevel) (t: TokenType) =
        Lexer.tokenize tl
        if not (tl.Token = t) then
            printfn "ERROR at line %d: Expected %+A but got %+A" tl.Line t tl.Token
            exit 1

    let expectIdent (tl: TopLevel) =
        Lexer.tokenize tl
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

        let indexRegLeft = getIdx r1        
        
        expectTok tl Col
        Lexer.tokenize tl
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
            Lexer.tokenize tl
            
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
            Lexer.tokenize tl
            
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
            Lexer.tokenize tl
            
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
        

    let parseJz (tl: TopLevel) =
        Lexer.tokenize tl
        let valueToCheck = 
            match tl.Token with
            | Ident reg -> 
                let idx = getIdx reg
                match tl.Regs.[idx] with
                | Some n -> n
                | None ->
                    printfn "ERROR at line %d: Cannot perfom jz with register '%s' because it is empty" tl.Line reg
                    exit 1
            | Integer n -> n
            | _ ->
                printfn "ERROR at line %d: Expected a register or an integer but got: %+A" tl.Line tl.Token
                exit 1

        expectTok tl Col
        let targetLabel = expectIdent tl
        
        if valueToCheck = 0 then
            match tl.Labels.TryFind targetLabel with
            | Some pos -> tl.Index <- pos
            | None -> 
                printfn "ERROR: Unknown label '%s'" targetLabel
                exit 1

    let parseJnz (tl: TopLevel) =
        Lexer.tokenize tl
        let valueToCheck = 
            match tl.Token with
            | Ident reg -> 
                let idx = getIdx reg
                match tl.Regs.[idx] with
                | Some n -> n
                | None ->
                    printfn "ERROR at line %d: Cannot perfom jnz with register '%s' because it is empty" tl.Line reg
                    exit 1
            | Integer n -> n
            | _ ->
                printfn "ERROR at line %d: Expected a register or an integer but got: %+A" tl.Line tl.Token
                exit 1

        expectTok tl Col
        let targetLabel = expectIdent tl
        
        if not(valueToCheck = 0) then
            match tl.Labels.TryFind targetLabel with
            | Some pos -> tl.Index <- pos
            | None -> 
                printfn "ERROR: Unknown label '%s'" targetLabel
                exit 1

    let parseCall (tl: TopLevel) =
        expectTok tl DoubleCol
        let targetLabel = expectIdent tl

        match tl.Labels.TryFind targetLabel with
        | Some pos ->
            tl.ReturnStack <- tl.Index :: tl.ReturnStack
            tl.Index <- pos
        | None ->
            printfn "ERROR at line %d: Unknow label: '%s'" tl.Line targetLabel
            exit 1

    let parseRet (tl: TopLevel) =
        match tl.ReturnStack with
        | [] ->
            printfn "ERROR at line %d: Tried to return but the label was never called" tl.Line
            exit 1
        | head :: tail ->
            tl.Index <- head
            tl.ReturnStack <- tail

    let parseHalt (tl: TopLevel) =
        Lexer.tokenize tl

        match tl.Token with
        | Ident reg ->
            if not(Array.contains reg registers) then
                printfn "ERROR at line %d: Unknow register '%s'\nList of registers %+A" tl.Line reg registers
                exit 1

            let indexReg = getIdx reg
            match tl.Regs[indexReg] with
            | Some n ->
                exit n
            | None ->
                printfn "ERROR at line %d: Cannot perform halt with register %s because it is empty" tl.Line reg
                exit 1
        | Integer n ->
            exit n
        | _ ->
            printfn "ERROR at line %d: Expected a register or an integer but got %+A" tl.Line tl.Token
            exit 1

    let rec parse (tl: TopLevel) =
        Lexer.tokenize tl
        match tl.Token with
        | EOF -> ()
        | Label _ -> parse tl
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
            | "jz" ->
                parseJz tl
                parse tl
            | "jnz" ->
                parseJnz tl
                parse tl
            | "call" ->
                parseCall tl
                parse tl
            | "ret" ->
                parseRet tl
                parse tl
            | "halt" ->
                parseHalt tl
            | _ ->
                printfn "ERROR at line %d: Unknow instruction '%s'" tl.Line op
        | _ ->
            printfn "ERROR at line %d: Unexpected token: '%+A'" tl.Line tl.Token
            exit 1

    let parseToAst (src: string) : ProgramAst =
        let tl = {
            Src = src
            Index = 0
            Line = 1
            Token = EOF
            Regs = Array.create registers.Length None
            Labels = Map.empty
            ReturnStack = []
        }

        let instructions = System.Collections.Generic.List<Instruction>()

        let expectTokAst (t: TokenType) =
            Lexer.tokenize tl
            if not (tl.Token = t) then
                printfn "ERROR at line %d: Expected %+A but got %+A" tl.Line t tl.Token
                exit 1

        let expectIdentAst () =
            Lexer.tokenize tl
            match tl.Token with
            | Ident n -> n
            | other ->
                printfn "ERROR at line %d: Expected Ident, got %A" tl.Line other
                exit 1

        let expectRegisterAst () =
            let name = expectIdentAst ()
            if not (Array.contains name registers) then
                printfn "ERROR at line %d: Unknow register '%s'\nList of registers %+A" tl.Line name registers
                exit 1
            name

        let parseOperandAst () =
            Lexer.tokenize tl
            match tl.Token with
            | Ident r ->
                if not (Array.contains r registers) then
                    printfn "ERROR at line %d: Unknow register '%s'\nList of registers %+A" tl.Line r registers
                    exit 1
                Reg r
            | Integer n ->
                Imm n
            | other ->
                printfn "ERROR at line %d: Expected a number or a register but got: %+A" tl.Line other
                exit 1

        let rec parseLoop () =
            Lexer.tokenize tl
            match tl.Token with
            | EOF -> ()
            | Label _ ->
                parseLoop ()
            | Ident op ->
                match op with
                | "mov" ->
                    let dst = expectRegisterAst ()
                    expectTokAst Col
                    let srcOp = parseOperandAst ()
                    instructions.Add(Mov (dst, srcOp))
                    parseLoop ()
                | "dump" ->
                    let reg = expectRegisterAst ()
                    instructions.Add(Dump reg)
                    parseLoop ()
                | "pop" ->
                    let reg = expectRegisterAst ()
                    instructions.Add(Pop reg)
                    parseLoop ()
                | "add" ->
                    let dst = expectRegisterAst ()
                    expectTokAst Col
                    let opnd = parseOperandAst ()
                    instructions.Add(Add (dst, opnd))
                    parseLoop ()
                | "sub" ->
                    let dst = expectRegisterAst ()
                    expectTokAst Col
                    let opnd = parseOperandAst ()
                    instructions.Add(Sub (dst, opnd))
                    parseLoop ()
                | "cmp" ->
                    let dst = expectRegisterAst ()
                    expectTokAst Col
                    let opnd = parseOperandAst ()
                    instructions.Add(Cmp (dst, opnd))
                    parseLoop ()
                | "jz" ->
                    let cond = parseOperandAst ()
                    expectTokAst Col
                    let target = expectIdentAst ()
                    instructions.Add(Jz (cond, target))
                    parseLoop ()
                | "jnz" ->
                    let cond = parseOperandAst ()
                    expectTokAst Col
                    let target = expectIdentAst ()
                    instructions.Add(Jnz (cond, target))
                    parseLoop ()
                | "call" ->
                    expectTokAst DoubleCol
                    let target = expectIdentAst ()
                    instructions.Add(Call target)
                    parseLoop ()
                | "ret" ->
                    instructions.Add(Ret)
                    parseLoop ()
                | "halt" ->
                    Lexer.tokenize tl
                    match tl.Token with
                    | Ident r ->
                        if not (Array.contains r registers) then
                            printfn "ERROR at line %d: Unknow register '%s'\nList of registers %+A" tl.Line r registers
                            exit 1
                        instructions.Add(Halt (HaltReg r))
                    | Integer n ->
                        instructions.Add(Halt (HaltImm n))
                    | other ->
                        printfn "ERROR at line %d: Expected a register or an integer but got %+A" tl.Line other
                        exit 1
                | _ ->
                    printfn "ERROR at line %d: Unknow instruction '%s'" tl.Line op
                    exit 1
            | other ->
                printfn "ERROR at line %d: Unexpected token: '%+A'" tl.Line other
                exit 1

        parseLoop ()
        instructions |> Seq.toList

