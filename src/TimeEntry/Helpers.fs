namespace TimeEntry
module Result =
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
//https://fsharpforfunandprofit.com/posts/designing-with-types-more-semantic-types/

    open Result
        /// An interface that all wrapped strings support
    type IWrappedString = 
        abstract Value : string

    /// Create a wrapped value option
    /// 1) canonicalize the input first
    /// 2) If the validation succeeds, return Success of the given constructor
    /// 3) If the validation fails, return Failure
    /// Null values are never valid.
    let create canonicalize validate ctor (s:string) = 
        if s = null 
        then Failure "Input cannot be null"
        else
            let s' = canonicalize s
            validate s'

    /// Apply the given function to the wrapped value
    let apply f (s:IWrappedString) = 
        s.Value |> f 

    /// Get the wrapped value
    let value s = apply id s

    /// Equality test
    let equals left right = 
        (value left) = (value right)
        
    /// Comparison
    let compareTo left right = 
        (value left).CompareTo (value right)


    /// Canonicalizes a string before construction
    /// * converts all whitespace to a space char
    /// * trims both ends
    let singleLineTrimmed s =
        System.Text.RegularExpressions.Regex.Replace(s,"\s"," ").Trim()

    /// A validation function based on length
    let lengthValidator len (s:string) =
        if s.Length <= len then Success s 
        else Failure <| sprintf "Your input is longer than exepected. Limit: %d" len

    /// A string of length 100
    type String100 = String100 of string with
        interface IWrappedString with
            member this.Value = let (String100 s) = this in s

    /// A constructor for strings of length 100
    let string100 = create singleLineTrimmed (lengthValidator 100) String100

    /// A string of length 100
    type String4 = String4 of string with
        interface IWrappedString with
            member this.Value = let (String4 s) = this in s

    /// A constructor for strings of length 100
    let string4 = create singleLineTrimmed (lengthValidator 100) String100
     