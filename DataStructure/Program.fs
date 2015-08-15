// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open System.IO
open ElementrySort
open MergeSort
open QuickSort
open PriorityQueue
open Graph

[<EntryPoint>]
let main argv = 
    let data = [|2;20;1;11;3;5;7|]
    //let aux = Array.zeroCreate data.Length
    //let result = MergeSort data aux 0 (data.Length - 1)
    //let result = QuickSort data 0 (data.Length - 1)
    //printfn "%A" result

//    let pq = PriorityQueue<int>(10)
//    for i in [1..10] do
//        let num = Console.ReadLine()
//        pq.Insert (Int32.Parse num)
//
//    while not pq.IsEmpty do
//        let value = pq.DelMax()
//        printfn "%A" value

    let path = Path.GetFullPath("Graph.txt")
    ConstructGraph path
    Console.ReadLine() |> ignore
    0 // return an integer exit code
