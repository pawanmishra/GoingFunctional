module ElementrySort

let SelectionSort (data: int array) =
    let N = data.GetLength(0)
    for i in [0..N-1] do
        let mutable min = i
        for j in [i+1..N-1] do
            min <- if data.[j] < data.[min] then j else min
        let temp = data.[i]
        data.[i] <- data.[min]
        data.[min] <- temp
    data
    
let InsertionSort (data:int array) =
    let N = data.GetLength(0)
    for i in [1..N-1] do
        for j = i downto 1 do
            if data.[j] < data.[j - 1] then
                let temp = data.[j]
                data.[j] <- data.[j - 1]
                data.[j - 1] <- temp
    data