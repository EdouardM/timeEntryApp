namespace TimeEntry

module Option = 
    type MaybeBuilder() = 
        member this.Return x = Some x
        member this.Bind(x,f) = Option.bind f x 
    let maybe = new MaybeBuilder()

module Result =

    type FailureMessage = string
    type Result<'T> = | Success of 'T | Failure of FailureMessage

    let retn x = Success x  

    let flatten  = 
        function 
            | Success (Success x) -> (Success x)
            | Success (Failure msg) -> Failure msg 
            | Failure msg -> Failure msg
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
    
    let failIfMissing msg = 
        function
            | Some x -> Success x
            | None   -> Failure msg 


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
            | [] -> retn []
            | head::tail -> retn cons <*> (f head) <*> (traverse f tail)
    
    //https://fsharpforfunandprofit.com/posts/elevated-world-4/#sequence
    let sequence x = traverse id x

    type ResultBuilder() =
        member this.Bind(m, f) = bind f m
        member this.Return(x) = retn x

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