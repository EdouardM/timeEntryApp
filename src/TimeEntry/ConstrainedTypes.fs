namespace TimeEntry
open System
open TimeEntry.Result
open System.Text.RegularExpressions

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

    /// A validation function based on maximum length
    let maxLengthValidator len (s:string) =
        if s.Length <= len then Success s 
        else Failure <| sprintf "Your input: %s is longer than expected (Limit: %d)"  s len

    /// A validation function based on maximum length
    let exactLengthValidator len (s:string) =
        if s.Length = len then Success s 
        else Failure <| sprintf "Your input: %s does not have the expected length (Length : %d)"  s len

    /// A validation function allowing only alphnumerical characters
    let regexMatchValidator pattern msg (s: string)  = 
        let nb = 
            Regex.Matches(s, pattern )
            |> Seq.cast<Match>
            |> Seq.length
        if nb = 0 then Success s
        else Failure msg

    let alphanumCharacterValidator = 
        let pattern =  "[^0-9a-zA-Z]+" 
        regexMatchValidator pattern "Only alphanumerical characters are allowed."

    let onlyNumCharacterValidator = 
        let pattern =  "[^0-9]+" 
        regexMatchValidator pattern "Only numerical characters are allowed."


    /// A string of length 3
    type String3 = String3 of string with
        interface IWrappedString with
            member this.Value = value this

    /// A constructor for strings of length 4
    let stringExact3 = create singleLineTrimmed (exactLengthValidator 3 >=> alphanumCharacterValidator) String3

    /// A string of length 4
    type String4 = String4 of string with
        interface IWrappedString with
            member this.Value = value this

    /// A constructor for strings of length 4
    let stringMax4 = create singleLineTrimmed (maxLengthValidator 4 >=> alphanumCharacterValidator) String4

    type String5 = String5 of string with
        interface IWrappedString with
            member this.Value = value this

    let stringExact5 = create singleLineTrimmed (exactLengthValidator 5 >=> alphanumCharacterValidator) String5 
    let stringMax5 = create singleLineTrimmed (maxLengthValidator 5 >=> alphanumCharacterValidator) String5

    type String6 = String6 of string with
        interface IWrappedString with
            member this.Value = value this
    
    let stringMax6 = create singleLineTrimmed (maxLengthValidator 6) String6
    
    type String8 = String8 of string with
        interface IWrappedString with
            member this.Value = value this
    let stringMax8 = create singleLineTrimmed (maxLengthValidator 8 >=> alphanumCharacterValidator) String8
    

    /// A string of length 10
    type String10 = String10 of string with
        interface IWrappedString with
            member this.Value = value this

    /// A constructor for strings of length 10
    let stringMax10     = create singleLineTrimmed (maxLengthValidator 10 >=> alphanumCharacterValidator) String10
    let stringExact10   = create singleLineTrimmed (exactLengthValidator 10 >=> alphanumCharacterValidator) String10
    let stringExactNum10 = create singleLineTrimmed (exactLengthValidator 10 >=> onlyNumCharacterValidator) String10 

    type String50 = String50 of string with
        interface IWrappedString with
            member this.Value = value this
    let stringMax50 = create singleLineTrimmed (maxLengthValidator 50 >=> alphanumCharacterValidator) String50


    type String200 = String200 of string with
        interface IWrappedString with
            member this.Value = value this
    let stringMax200 = create singleLineTrimmed (maxLengthValidator 200) String200