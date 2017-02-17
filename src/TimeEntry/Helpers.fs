namespace TimeEntry

module Option = 
    type MaybeBuilder() = 
        member this.Return x = Some x
        member this.Bind(x,f) = Option.bind f x 
    let maybe = new MaybeBuilder()

module Result =

    type FailureMessage = string
    
    type Result<'T> = 
        | Success of 'T 
        | Failure of FailureMessage

    let succeed x = Success x  

    let fail = Failure
    let map f res = 
        match res with
            | Success x -> Success (f x)
            | Failure e -> Failure e

    let apply fRes xRes =
        match fRes, xRes with
            | Success f, Success x -> Success (f x)
            | Failure e, Success _ -> Failure e
            | Success _, Failure e -> Failure e
            | Failure e, Failure e' -> Failure (e + "\n" + e')

    let (<!>) = map
    let (<*>) = apply

    let bind f xRes =
        match xRes with
            | Success x -> f x
            | Failure msg -> Failure msg  

    let (>>=) f g = f >> map g

    let (>=>) f g = f >> bind g

    //https://fsharpforfunandprofit.com/posts/recipe-part2/
    ///apply either a success function or a failure function
    let either successFunc failureFunc = 
        function
            | Success x   -> successFunc x
            | Failure msg -> failureFunc msg 

    /// convert two one-track functions into a two-track function
    let bimap successFunc failureFunc =
        either (successFunc >> succeed) (failureFunc >> fail)

    let tee f x = 
        f x |> ignore
        x

    let tryCatch f exnHandler x = 
        try 
            f x |> succeed
        with
            | ex -> exnHandler ex |> fail

    let plus addSuccess addFailure switch1 switch2 x =
        match  (switch1 x, switch2 x) with
            | Success s1, Success s2    -> addSuccess s1 s2 |> Success
            | Failure f1, Success _     -> Failure f1
            | Success _ , Failure f2    -> Failure f2
            | Failure f1, Failure f2    -> addFailure f1 f2 |> Failure

    ///combinator of 2 validation functions
    let (&&&) v1 v2 = 
        let addSuccess r1 r2 = r1 // return first 
        let addFailure f1 f2 = f1 + "; " + f2 //concat failure msg
        plus addSuccess addFailure v1 v2
        
    let flatten  = 
        function 
            | Success (Success x) -> (Success x)
            | Success (Failure msg) -> Failure msg 
            | Failure msg -> Failure msg
    let failIfNullString (input:string) = 
        if System.String.IsNullOrEmpty(input) then
            Failure "Null string is not allowed"
        else
            Success input

    let failIfMissing msg = 
        function
            | Some x -> Success x
            | None   -> Failure msg 

    let failIfFalse x msg = 
        function
            | true  -> Success x
            | false -> Failure msg 

    let failIfNotInList l x msg = 
        List.contains x l
        |> failIfFalse x msg

    let failIfEmpty msg l  = 
        List.isEmpty l
        |> not
        |> failIfFalse l msg

    let failIfNotInMap (d:Map<_,_>) x msg = 
        Map.containsKey x d 
        |> failIfFalse d msg

    let switchResOpt xResOpt =
        match xResOpt with
            | Some (Success x)   -> Success (Some x)
            | None               -> Success (None)
            | Some (Failure msg) -> Failure msg


    let unwrapResOpt xResOpt = 
        match xResOpt with
            | Some (Success x)   -> Some x
            | None               -> None
            | Some (Failure msg) -> None


    //https://fsharpforfunandprofit.com/posts/elevated-world-4/#traverse    
    let rec traverse f list = 
        let cons head tail = head::tail
        match list with
            | [] -> succeed []
            | head::tail -> succeed cons <*> (f head) <*> (traverse f tail)
    
    //https://fsharpforfunandprofit.com/posts/elevated-world-4/#sequence
    let sequence x = traverse id x

    type ResultBuilder() =
        member this.Bind(m, f) = bind f m
        member this.Return(x) = succeed x

        member this.ReturnFrom(x) = x

        member this.Zero() = this.Return()

        member this.Combine(m1, f) = this.Bind(m1, f)

    let result = ResultBuilder()

module Conversions = 
    
    let boolToSbyte = function
        | true -> 1y
        | false -> 0y

    let sbyteTobool = function
        | 0y -> false
        | _  -> true