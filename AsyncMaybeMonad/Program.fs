// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.Diagnostics

open Monad

[<EntryPoint>]
let main argv = 
    let first = maybe {
        Debug.WriteLine "Started"
        do! Async.Sleep(3000)
        Debug.WriteLine "Done"
        return "First Completed"}
    let x = Some 2
    let second = maybe{
            let! y = x
            return y
        }
    Console.WriteLine first
    Console.ReadLine() |> ignore
    0
