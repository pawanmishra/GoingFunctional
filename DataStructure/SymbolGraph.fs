module SymbolGraph

open Graph
open FSharp.Data
open System.IO

type SymbolGraph (path:string, sep:char) =
    let mutable map = Map.empty<string, int>
    let mutable keys : string array = Array.zeroCreate map.Count
    let mutable graph = new Graph(map.Count)
    let rec tokenize (tempMap:Map<string,int>) (items:string list) = 
        match items with
        | [] -> map <- tempMap
        | hd::tl when map.ContainsKey hd -> tokenize tempMap tl
        | hd::tl -> 
            let tmp = tempMap.Add(hd, tempMap.Count)
            tokenize tmp tl
    do  
        use txtReader = new StreamReader(path)
        let rec initializeMap (tmpReader:TextReader) = 
            match tmpReader.Peek() >= 0 with
            | false -> ()
            | true -> 
                tmpReader.ReadLine().Split [|sep|] |> Array.toList |> tokenize map
                initializeMap tmpReader
        initializeMap txtReader |> ignore
        keys <- Array.zeroCreate map.Count
        map |> Map.iter (fun x y -> keys.[map.[x]] <- x)
        graph <- new Graph(map.Count)
        use reader = new StreamReader(path)
        let rec createGraph (tempMap:Map<string,int>) (reader:TextReader) =
            match reader.Peek() >= 0 with
            | false -> ()
            | true -> 
                let items = reader.ReadLine().Split [|sep|]
                let vertex = tempMap.[items.[0]]
                for i in [1..items.Length-1] do
                    let v = tempMap.[items.[i]]
                    graph.AddEdge(vertex, v)
        createGraph map reader |> ignore

    member x.Graph with get() = graph
    member x.Contains (s:string) = map.ContainsKey s
    member x.Index (s:string) = map.Item s
    member x.Name (v:int) = keys.[v]
    
let DegreeOfSeparation =
    let sg = SymbolGraph("C:\Pawan\Dev\FSharp\DataStructure\DataStructure\movies_new.csv", '/')
    let graph = sg.Graph
    let index = sg.Index "Gray, Ian (I)"
    let bfs = BreadthFirstPath(sg.Graph, index)
    let target = sg.Index "Thompson, Jack (I)"
    let printPath =
        match bfs.Path(target) with
        | None -> printfn "Not connected"
        | Some v -> 
            for i in v do
                printfn "    %A" (sg.Name i)
    printPath |> ignore    


    