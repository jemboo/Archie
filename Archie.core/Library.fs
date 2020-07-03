namespace Archie.core

module Say =
    let hello name =
        printfn "Hello %s" name

module T =
    let Add5 (x:int) = 
        x+5