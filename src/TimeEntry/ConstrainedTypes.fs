namespace TimeEntry
open System
open TimeEntry.Result

module ConstrainedTypes = 
//https://fsharpforfunandprofit.com/posts/designing-with-types-more-semantic-types/
        /// An interface that all wrapped strings support
    type IWrappedString = 
        abstract Value: string

    /// Create a wrapped value option
    /// 1) canonicalize the input first
    /// 2) If the validation succeeds, return Success of the given constructor
    /// 3) If the validation fails, return Failure
    /// Null values are never valid.
    let create canonicalize validate ctor (s:string) = 
        if isNull s
        then Failure "Input cannot be null"
        else
            let s' = canonicalize s
            validate s'
            |> Result.map ctor

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
        else Failure <| sprintf "Your input: %s is longer than exepected. Limit: %d"  s len

    /// A string of length 100
    type String4 = String4 of string with
        interface IWrappedString with
            member this.Value = value this

    /// A constructor for strings of length 100
    let string4 = create singleLineTrimmed (lengthValidator 4) String4

    type String5 = String5 of string with
        interface IWrappedString with
            member this.Value = value this

    let string5 = create singleLineTrimmed (lengthValidator 5) String5

    type String6 = String6 of string with
        interface IWrappedString with
            member this.Value = value this
    let string6 = create singleLineTrimmed (lengthValidator 6) String6
    
    type String8 = String8 of string with
        interface IWrappedString with
            member this.Value = value this
    let string8 = create singleLineTrimmed (lengthValidator 8) String8
    

    /// A string of length 100
    type String10 = String10 of string with
        interface IWrappedString with
            member this.Value = value this

    /// A constructor for strings of length 10
    let string10 = create singleLineTrimmed (lengthValidator 10) String10

    type String50 = String50 of string with
        interface IWrappedString with
            member this.Value = value this
    let string50 = create singleLineTrimmed (lengthValidator 50) String50
