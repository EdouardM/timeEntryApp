namespace TimeEntry
module Helpers =
    open System

    type FailureMessage = string
    type Result<'T> = | Success of 'T | Failure of FailureMessage


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
