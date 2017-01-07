#load "./Helpers.fs" 
#load "./ConstrainedTypes.fs"
#load "./DomainTypes.fs"
#load "./Constructors.fs"
#load "./Database.fs"

open TimeEntry.Result
open TimeEntry.ConstrainedTypes
open TimeEntry.Conversions
open TimeEntry.DomainTypes
open TimeEntry.Constructors
open TimeEntry.DataBase

open System.Text.RegularExpressions

let site =  createSite ["F21"; "F22"] "F22"

let sf = createShopfloor ["F221A"; "F221B"] "F221B"

let sfinfo = createShopfloorInfo <!> site <*> sf

let DbSf = toDBShopfloorInfo <!> sfinfo


let specialCharacterValidator (s: string) = 
    let nb = 
        Regex.Matches(s, "[^0-9a-zA-Z]+")
        |> Seq.cast<Match>
        |> Seq.length
    if nb = 0 then Success s
    else Failure <| sprintf "Only alpahnumerical characters are allowed: %s"  s

specialCharacterValidator "abdo"
specialCharacterValidator "$abdo"
specialCharacterValidator "#epdp"

