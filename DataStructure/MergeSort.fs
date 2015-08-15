module MergeSort

let private Merge (data: int array) (aux : int array) low mid high =
    let mutable i = low
    let mutable j = mid + 1
    for k in low..high do aux.[k] <- data.[k]
    for k in [low..high] do
        data.[k] <- match k with
                    | _ when (i > mid) -> j <- j + 1; aux.[j - 1]
                    | _ when j > high -> i <- i + 1; aux.[i - 1]
                    | _ when aux.[j] < aux.[i] -> j <- j + 1; aux.[j - 1]; 
                    | _ -> i <- i + 1; aux.[i - 1]; 

let rec private Sort (data: int array) (aux : int array) low high =
    match low, high with 
    | _,_ when high <= low -> ()
    | _ -> 
        let mid = low + (high - low) / 2
        Sort data aux low mid
        Sort data aux (mid+1) high
        Merge data aux low mid high
        ()

let MergeSort (data: int array) (aux : int array) low high =
    Sort data aux low high
    printfn "%A" data
    