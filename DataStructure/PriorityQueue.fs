module PriorityQueue

type PriorityQueue<'T when 'T :> System.IComparable<'T>> (maxN : int) =
    let pq : 'T array = Array.zeroCreate (maxN + 1)
    let mutable N = 0
    
    let Less i j = pq.[i].CompareTo(pq.[j]) < 0
    let Exchange i j = 
        let temp = pq.[i]
        pq.[i] <- pq.[j]
        pq.[j] <- temp
        i

    let rec Swim k =
        match (k > 1 && Less (k/2) k) with
        | true -> Exchange (k/2) k |> Swim;
        | false -> ()
    
    let rec Sink k =
        match 2*k <= N, 2*k with
        | true, j when j < N && Less j (j + 1) -> match Less k (j+1), j+1 with
                                                    | true, x -> Exchange k x |> ignore; Sink x;
                                                    | false, _ -> ()
        | true, j -> Exchange k j |> ignore; Sink j;
        | false, _ -> ()

    member x.IsEmpty with get() = N = 0
    member x.Size with get() = N

    member x.Insert (item:'T) = N <- N + 1; pq.[N] <- item; Swim N;

    member x.DelMax() =
        let max = pq.[1]
        Exchange 1 N |> ignore
        N <- N - 1
        pq.[N+1] <- Unchecked.defaultof<'T>
        Sink 1
        max


