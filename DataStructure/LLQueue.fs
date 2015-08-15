module LLQueue

type Node<'T> = { Value : 'T; mutable Next : Node<'T> option }

type LLStack<'T>() = 
    inherit System.Object()

    let mutable Last : Node<'T> option = None
    let mutable First : Node<'T> option = None
    let mutable N = 0

    member x.Size with get() = N
    member x.IsEmpty with get() = match First with
                                    | Some _ -> false
                                    | None -> true

    member x.Enqueue item =
        let oldLast = Last
        Last <- Some { Value = item; Next = None }
        match x.IsEmpty with
        | true -> First <- Last
        | false -> oldLast.Value.Next <- Last
        N <- N + 1

    member x.DeQueue =
        match First with
        | Some x ->
            First <- x.Next
            N <- N - 1
            ()
        | None -> ()
        match x.IsEmpty with
        | true -> 
            Last <- None
            ()
        | _ -> ()