namespace Onizuka

open System

open Onizuka.Parser

module Interpreter =
    type RuntimeState =
        {
            Regs: int option[]
            mutable Pc: int
            mutable ReturnStack: int list
        }

    let private getRegIdx (reg: string) =
        Parser.getIdx reg

    let private expectRegValue (state: RuntimeState) (line: int) (reg: string) =
        let idx = getRegIdx reg
        match state.Regs[idx] with
        | Some v -> v
        | None ->
            printfn "ERROR at line %d: Register %s is empty" line reg
            Environment.Exit 1
            0

    let private expectRegExists (line: int) (reg: string) =
        if not (Array.contains reg Parser.registers) then
            printfn "ERROR at line %d: Unknow register '%s'\nList of registers %+A" line reg Parser.registers
            Environment.Exit 1

    let private evalOperand (state: RuntimeState) (line: int) (op: Operand) =
        match op with
        | Imm n -> n
        | Reg r ->
            expectRegExists line r
            expectRegValue state line r

    let private resolveLabel (labels: Map<string, int>) (line: int) (name: string) =
        match labels.TryFind name with
        | Some pc -> pc
        | None ->
            printfn "ERROR at line %d: Unknown label '%s'" line name
            Environment.Exit 1
            0

    let run (info: ProgramInfo) : int =
        let ast = info.Ast |> List.toArray
        let labels = info.Labels

        let labelStarts =
            labels
            |> Seq.map (fun kvp -> kvp.Value)
            |> Seq.distinct
            |> Seq.sort
            |> Seq.toArray

        let startPc =
            match labels.TryFind "main" with
            | Some pc -> pc
            | None -> 0

        let stopPc =
            if startPc = 0 && not (labels.ContainsKey "main") then
                if labelStarts.Length = 0 then ast.Length else labelStarts[0]
            else
                let next =
                    labelStarts
                    |> Array.tryFind (fun pc -> pc > startPc)
                defaultArg next ast.Length

        let state =
            {
                Regs = Array.create Parser.registers.Length None
                Pc = startPc
                ReturnStack = []
            }

        let rec loop () =
            if state.Pc < 0 || state.Pc > ast.Length then
                printfn "ERROR: Program counter out of bounds (%d)" state.Pc
                Environment.Exit 1

            if state.Pc = ast.Length then
                0
            elif state.ReturnStack = [] && state.Pc >= stopPc then
                0
            else
                let instr, line = ast[state.Pc]

                let nextPc () =
                    state.Pc <- state.Pc + 1
                    loop ()

                match instr with
                | Mov (dst, src) ->
                    expectRegExists line dst
                    let idx = getRegIdx dst
                    let v =
                        match src with
                        | Imm n -> n
                        | Reg r ->
                            expectRegExists line r
                            expectRegValue state line r
                    state.Regs[idx] <- Some v
                    nextPc ()

                | Dump reg ->
                    expectRegExists line reg
                    let v = expectRegValue state line reg
                    printfn "%d" v
                    nextPc ()

                | Pop reg ->
                    expectRegExists line reg
                    let idx = getRegIdx reg
                    match state.Regs[idx] with
                    | Some _ ->
                        state.Regs[idx] <- None
                        nextPc ()
                    | None ->
                        printfn "ERROR at line %d: Cannot pop register %s because it is already empty" line reg
                        Environment.Exit 1
                        0

                | Add (dst, opnd) ->
                    expectRegExists line dst
                    let idx = getRegIdx dst
                    match state.Regs[idx] with
                    | None ->
                        printfn "ERROR at line %d: Cannot perform add on register %s because it is empty" line dst
                        Environment.Exit 1
                        0
                    | Some n1 ->
                        let n2 = evalOperand state line opnd
                        state.Regs[idx] <- Some (n1 + n2)
                        nextPc ()

                | Sub (dst, opnd) ->
                    expectRegExists line dst
                    let idx = getRegIdx dst
                    match state.Regs[idx] with
                    | None ->
                        printfn "ERROR at line %d: Cannot perform sub on register %s because it is empty" line dst
                        Environment.Exit 1
                        0
                    | Some n1 ->
                        let n2 = evalOperand state line opnd
                        state.Regs[idx] <- Some (n1 - n2)
                        nextPc ()

                | Cmp (dst, opnd) ->
                    expectRegExists line dst
                    let idx = getRegIdx dst
                    match state.Regs[idx] with
                    | None ->
                        printfn "ERROR at line %d: Cannot perform cmp on register %s because it is empty" line dst
                        Environment.Exit 1
                        0
                    | Some n1 ->
                        let n2 = evalOperand state line opnd
                        state.Regs[idx] <- Some (if n1 = n2 then 0 else 1)
                        nextPc ()

                | Jz (cond, target) ->
                    let v = evalOperand state line cond
                    if v = 0 then
                        state.Pc <- resolveLabel labels line target
                        loop ()
                    else
                        nextPc ()

                | Jnz (cond, target) ->
                    let v = evalOperand state line cond
                    if v <> 0 then
                        state.Pc <- resolveLabel labels line target
                        loop ()
                    else
                        nextPc ()

                | Call target ->
                    let targetPc = resolveLabel labels line target
                    state.ReturnStack <- (state.Pc + 1) :: state.ReturnStack
                    state.Pc <- targetPc
                    loop ()

                | Ret ->
                    match state.ReturnStack with
                    | [] ->
                        printfn "ERROR at line %d: Tried to return but the label was never called" line
                        Environment.Exit 1
                        0
                    | head :: tail ->
                        state.ReturnStack <- tail
                        state.Pc <- head
                        loop ()

                | Halt hop ->
                    match hop with
                    | HaltImm n -> n
                    | HaltReg r ->
                        expectRegExists line r
                        expectRegValue state line r

        loop ()

