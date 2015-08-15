module QuickSort

let private Partition (data: int array) low high =
    let mutable i = low
    let mutable j = high + 1
    let pivot = data.[low]
    let less x y = x <= y
    let exchange x y = 
        let temp = data.[x]; 
        data.[x] <- data.[y]; 
        data.[y] <- temp; 
        y
    let rec proc_i s =
                match s+1 with
                | x when less data.[x] pivot && x <> high -> proc_i x;
                | _ -> s+1 
    let rec proc_j t =
                match t-1 with
                | x when less pivot data.[x] && x <> low -> proc_j x; 
                | _ -> t-1
    let rec partition_data s t =
         match (proc_i s), (proc_j t) with
            | x , y when x >= y -> y
            | x , y -> exchange x y |> partition_data x 
    j <- partition_data i j
    exchange low j

let rec private Sort (data: int array) low high =
    match low, high with
    | _ , _ when high <= low -> ()
    | _ -> 
        let j = Partition data low high
        Sort data low (j - 1)
        Sort data (j + 1) high
        ()

let QuickSort (data: int array) low high =
    Sort data low high
    printfn "%A" data