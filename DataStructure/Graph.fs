module Graph

open System
open System.IO

type Graph (v : int) =
    let V = v
    let mutable E = 0
    let Adj : int list array = Array.zeroCreate V
    do Adj |> Array.iteri (fun i x -> Adj.[i] <- [])
    new(reader : TextReader) = Graph(Int32.Parse(reader.ReadLine()))
    
    member x.Vertices with get() = V
    member x.Edge with get() = E
    member x.Adjecent v = Adj.[v]
    member x.AddEdge (v, w) = 
        Adj.[v] <- w::Adj.[v]
        Adj.[w] <- v::Adj.[w]
        E <- E + 1
    
    member x.AddEdge (reader: TextReader) = 
        let tempEdge = Int32.Parse(reader.ReadLine())
        for i in [0..tempEdge-1] do
            let items = reader.ReadLine().Split(' ') |> Array.map (fun x -> Int32.Parse(x)) 
            x.AddEdge (items.[0], items.[1])

[<AbstractClass>]
type Path (graph : Graph, source : int) =
    let HasPath (v, (marked:bool array)) = marked.[v]
    member x.PathTo (v, (edgeTo:int array), (marked:bool array)) :int list option =
        match HasPath (v, marked) with
        | false -> None
        | true -> 
            let rec ComputePath v items =
                match v with
                | x when x <> source -> ComputePath edgeTo.[x] (x::items)
                | s when s = source -> s::items
                | _ -> items
            ComputePath v [] |> Some

type DepthFirstPath (graph : Graph, source : int) =
    inherit Path(graph, source)
    let marked : bool array = Array.zeroCreate graph.Vertices
    let edgeTo : int array = Array.zeroCreate graph.Vertices
    let rec DFS (graph:Graph, v:int) =
        marked.[v] <- true
        graph.Adjecent v |> List.iter (fun x -> match marked.[x] with
                                                   | false ->  edgeTo.[x] <- v; DFS(graph, x); 
                                                   | _ -> ()) 
    do DFS(graph, source)
    member x.Path (v:int) :int list option = base.PathTo (v, edgeTo, marked)

type BreadthFirstPath (graph : Graph, source : int) =
    inherit Path(graph, source)
    let marked : bool array = Array.zeroCreate graph.Vertices
    let edgeTo : int array = Array.zeroCreate graph.Vertices
    let BFS (graph:Graph, v:int) =
        marked.[v] <- true
        let rec Traverse (data:int list) =
            match data with
            | [] -> ()
            | hd::tl -> 
                let tempLst = graph.Adjecent hd |> List.filter (fun i -> not marked.[i]) |> List.map (fun x -> edgeTo.[x] <- hd; marked.[x] <- true; x;)
                Traverse (tl@tempLst) |> ignore
        Traverse [v]
    do BFS(graph, source)
    member x.Path (v:int) :int list option = base.PathTo (v, edgeTo, marked)

let ConstructGraph (path:string) =
    use reader = new StreamReader(path)
    let graph = Graph(reader)
    graph.AddEdge reader
    let dfp = DepthFirstPath(graph, 0)
    let path = dfp.Path(3)
    printfn "DFS %A" path.Value

    let bfs = BreadthFirstPath(graph, 0)
    let bfsPath = bfs.Path(3)
    printfn "BFS : %A" path.Value
        
