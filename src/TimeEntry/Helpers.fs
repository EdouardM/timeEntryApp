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


module ConstrainedString = 
    open Helpers
    type ValidString = ValidString of string

    let createString50 length fieldname (s: string) = 
        match s with
            | s when s = null -> Failure <| sprintf "%s cannot be null." s 
            | s when s.Length <= length -> Success (ValidString s)
            | s -> Failure <| sprintf "Your input is too long. %d characters maximum." length 