open System.IO

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

type OpCode =
    | Mov of int * int
    | Add of int * int * int 
    | Dump of int
    | Halt

type VM = {
    mutable Regs: int[]
    mutable PC: int
    mutable Running: bool
}

type TopLevel = {
    Src: string[]
    mutable Index: int
    mutable Line: int
}

let rec build_ast (tl: TopLevel) =
    for line in tl.Src do
        let tokens = line.Split([| ' '; '\t' |], System.StringSplitOptions.RemoveEmptyEntries)

        for inst in tokens do
            match inst with
            | "Hello" -> printfn "Hello Back"
            | _ ->
                printfn "ERROR at line %d: Unknow op '%s'" tl.Line inst
                exit 1
        tl.Line <- tl.Line + 1  

let step (vm: VM) (instr: OpCode) =
    match instr with
    | Mov (r, num) -> vm.Regs[r] <- num
    | Add (r1, r2, r3) -> vm.Regs[r1] <- vm.Regs[r2] + vm.Regs[r3]
    | Dump r -> printfn "%d" vm.Regs[r]
    | Halt -> vm.Running <- false

[<EntryPoint>]
let main _ =
    match readFile "test.on" with
    | Some res ->
        let tl = {
            Src = res.Split([| "\r\n"; "\n" |], System.StringSplitOptions.None)
            Index = 0
            Line = 1
        }
        build_ast tl
    | None -> exit 1

    0
