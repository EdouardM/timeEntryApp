namespace TimeEntry
module DomainTypes =
    open System
    open Helpers
    
    //Domain Model:
    type TimeType = 
            | Machine
            | Labour

    type RecordStatus = | Entered | Validated

    type NbPeople = NbPeople of float
    

    ///Create a valid number of people:
    ///Number of people can only be integer or half of integer and positive
    let createNbPeople nb = 
        if nb >= 0. then
            let r = floor nb
            let d = nb - r
            if d >= 0.5 then Success (NbPeople (r + 0.5))
            else Success(NbPeople r)
        else Failure <| sprintf "Number of people can't be negative.\nNb people: %.2f" nb

    type Site  = Site of string
    
    ///Helper function to create domain types values validated against a list of valid input
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
        then
            if startTime <= endTime 
            then 
                Success { StartTime = startTime; EndTime = endTime } 
            else 
                Failure <| sprintf "Start time must be before End time.\nStart time: %A\tEnd time: %A" startTime endTime
        else
            Failure <| sprintf "Date year must be within the range from 2010 to 2100.\nStart Year: %d\End Year: %d" startTime.Year endTime.Year

    type TimeEntry = 
        | MachineOnly of Duration
        | MachineAndLabour of Duration * NbPeople
        | LabourOnly of Duration * NbPeople
    let createTimeEntry timeType nbPeople duration =
        match timeType, nbPeople with
            | Machine, (NbPeople 0.)  -> MachineOnly duration
            | Labour,  nbPeople       -> LabourOnly (duration, nbPeople)
            | Machine, nbPeople       -> MachineAndLabour (duration, nbPeople) 


    type WorkOrder = WorkOrder of string
    type Event = Event of string 
    let createWorkOrder = create WorkOrder "workorder"
    let createEvent = create Event "event"


    type TimeAllocation = 
        //productive time record against work Order
        | WorkOrder of WorkOrder
        //improductive time recorded against event
        | Event of Event * TimeType

    //Domain model (pure)
    type TimeRecord =
        {
            Site        : Site
            ShopFloor   : ShopFloor
            WorkCenter  : WorkCenter
            TimeEntry   : TimeEntry
            Allocation  : TimeAllocation
            Status      : RecordStatus
        }
    
    let createTimeRecord site shopfloor workcenter allocation timeEntry =
        { Site = site; ShopFloor = shopfloor; WorkCenter = workcenter; TimeEntry = timeEntry; Allocation = allocation; Status = Entered}

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

        ///Function to convert one Domain Record into records to insert into Database
        let toDB  (time : TimeRecord) = 
            match time.TimeEntry with
                //List of one record: Machine time
                | MachineOnly duration -> 
                    [ {   Site = time.Site; ShopFloor = time.ShopFloor; WorkCenter = time.WorkCenter; 
                        TimeType = Machine; Duration = duration; NbPeople = NbPeople 0. } ]
                //List of two records: Machine & Labour time
                | MachineAndLabour (duration, nb) -> 
                    [ { Site = time.Site; ShopFloor = time.ShopFloor; WorkCenter = time.WorkCenter;
                        TimeType = Machine; Duration = duration; NbPeople = NbPeople 0. } ;
                    {   Site = time.Site; ShopFloor = time.ShopFloor; WorkCenter = time.WorkCenter; 
                        TimeType = Labour ; Duration = duration; NbPeople = nb } ]
                //List of one record: Labour time
                | LabourOnly (duration, nb) -> 
                    [ {   Site = time.Site; ShopFloor = time.ShopFloor; WorkCenter = time.WorkCenter; 
                        TimeType = Labour; Duration = duration; NbPeople = nb } ]

    module DTO =
        ///Domain to store data deserialized from JSON format
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
                let nbPeopleResult = createNbPeople dto.NbPeople

                let timeEntryResult = createTimeEntry <!> timetypeResult <*> nbPeopleResult <*> durationResult
                let timeRecordResult = createTimeRecord <!> siteResult <*> shopfloorResult <*> workcenterResult <*> timeEntryResult
                timeRecordResult