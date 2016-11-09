module TimeEntry
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
 
//Domain Model:
type TimeType = 
        | Machine
        | Labour

type RecordStatus = | Entered | Validated

type NbPeople = NbPeople of float

type Site  = Site of string

let create ty name validList id = 
    match List.exists (fun s -> s = id ) validList with
        | true -> Success (ty id)
        | false -> Failure <| sprintf "Can't find %s: %s" name id 

let createSite = create Site "site"

type SiteAccess = | All | Site of Site

type ShopFloor  = ShopFloor of string

let createShopfloor = create ShopFloor "shopfloor"

type WorkCenter  = WorkCenter of string

let createWorkCenter = create WorkCenter "workcenter"

type Duration = 
    {
        StartTime   : DateTime
        EndTime     : DateTime
    }
    with
        member x.Duration = x.EndTime - x.StartTime 

let createDuration (startTime: DateTime) (endTime: DateTime) =
    if     startTime.Year >= 2010 
        && startTime.Year <= 2100 
        && endTime.Year >= 2010
        && endTime.Year <= 2100
        && startTime <= endTime 
    then 
        Success { StartTime = startTime; EndTime = endTime } 
    else 
        Failure "Start time must be before End time"

type TimeEntry = 
    | MachineOnly of Duration
    | MachineAndLabour of Duration * NbPeople
    | LabourOnly of Duration * NbPeople

let createTimeEntry timeType nbPeople duration =
    match timeType, nbPeople with
        | Machine, 0. -> MachineOnly duration
        | Labour,  nbPeople -> LabourOnly (duration, NbPeople nbPeople)
        | Machine, nbPeople -> MachineAndLabour (duration, NbPeople nbPeople) 
    

//Domain model (pure)
type TimeRecord =
    {
        Site        : Site
        ShopFloor   : ShopFloor
        WorkCenter  : WorkCenter
        TimeEntry   : TimeEntry
        Status      : RecordStatus 
    }

let createTimeRecord site shopfloor workcenter timeEntry =
    { Site = site; ShopFloor = shopfloor; WorkCenter = workcenter; TimeEntry = timeEntry; Status = Entered}

type UserName = Login of string

type AuthLevel = | Entry | Maintenance | Admin

type User =
    {
        Site        : SiteAccess
        Name        : UserName
        Level       : AuthLevel
    }

// Use Types
type EntryRequest = { User: User; Entry : TimeRecord }

type EntryResponse = { Id: int; Request: EntryRequest}

//Use Case 1: Entry of time
// Db failure or JSon failure
type AddEntryData = EntryRequest -> Result<EntryResponse>


module DataBase =
    //DataBase model (pure)
    type DBTimeRecord =
        {
            Site        : Site
            ShopFloor   : ShopFloor
            WorkCenter  : WorkCenter
            TimeType    : TimeType
            Duration    : Duration
            NbPeople    : NbPeople
        }

    let toDB  = ()

module DTO =

    type TimeRecordDTO =
        {
            Site        : string
            ShopFloor   : string
            WorkCenter  : string
            TimeType    : string
            StartTime   : DateTime
            EndTime     : DateTime
            NbPeople    : float
        }
    
    let fromDTO 
        validSites
        validShopFloors
        validWorkCenters
        (dto: TimeRecordDTO) =
            let siteResult = createSite validSites dto.Site
            let shopfloorResult  = createShopfloor validShopFloors dto.ShopFloor
            let workcenterResult = createWorkCenter validWorkCenters dto.WorkCenter
            let timetypeResult = 
                match dto.TimeType with
                    | "Machine" -> Success Machine
                    | "Labour"  -> Success Labour
                    | ty         -> Failure <| sprintf "Wrong time type: %s" ty

            let durationResult = createDuration dto.StartTime dto.EndTime

            let timeEntryResult = createTimeEntry <!> timetypeResult <*> (Success dto.NbPeople) <*> durationResult
            let timeRecordResult = createTimeRecord <!> siteResult <*> shopfloorResult <*> workcenterResult <*> timeEntryResult
            timeRecordResult



