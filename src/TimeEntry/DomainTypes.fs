namespace TimeEntry

module DomainTypes =
    open System
    open TimeEntry.Result
    open TimeEntry.ConstrainedString
    
    //Domain Model:
    // http://fsharpforfunandprofit.com/ddd/

    type TimeType = 
            | MachineTime
            | LabourTime

    type RecordStatus = | Entered | Validated

    type NbPeople = NbPeople of float32
    
    type Site  = Site of string
    
    type ShopFloor  = ShopFloor of string

    type ShopFloorInfo = 
        {
            Site : Site
            ShopFloor : ShopFloor
        }

    type WorkCenter  = WorkCenter of string
    
    type Hour = Hour of uint32

    type WorkCenterInfo = 
        {
            WorkCenter      : WorkCenter
            ShopFloorInfo   : ShopFloorInfo
            StartHour       : Hour
            EndHour         : Hour
        }
    
    type Duration = 
        {
            StartTime   : DateTime
            EndTime     : DateTime
        }
        with
            member x.Duration = x.EndTime - x.StartTime
            member x.ToHr =  
                let minutes = (float x.Duration.TotalMinutes)
                System.Math.Round(minutes / 60., 4)

    type TimeEntry = 
        | MachineOnly of Duration
        | MachineAndLabour of Duration * NbPeople
        | LabourOnly of Duration * NbPeople

    type WorkOrder = WorkOrder of string

    type ItemCode  = ItemCode of string

    type ItemType  = ItemType of string
    
    type WorkOrderStatus =
        | Open
        | Closed

    type TimeHr = TimeHr of float32

    type WorkOrderEntry =
        {
            WorkOrder           : WorkOrder
            WorkCenter          : WorkCenter              
            ItemCode            : ItemCode
            TotalMachineTimeHr  : TimeHr
            TotalLabourTimeHr   : TimeHr
            Status              : WorkOrderStatus
        }

    type WorkOrderEntryId = uint32

    type Machine = Machine of string

    //In case of BreakDown we record addiional information
    type EventInfo = 
        { 
            Machine     : Machine 
            Cause       : string
            Solution    : string
            Comments    : string 
        }   

    type Event = 
            | ZeroPerson    of string
            | WithInfo      of string
            | WithoutInfo   of string 
 

    type EventEntry = 
        | EventWithInfo         of Event * EventInfo 
        | EventWithoutInfo      of Event 
        | EventZeroPerson       of Event

    type EventEntryId = uint32
                     
    type TimeAllocation = 
        //productive time record against work Order
        | WorkOrderEntry of WorkOrderEntry
        //improductive time recorded against event
        | EventEntry of EventEntry

    //Domain model (pure)
    type TimeRecord =
        {
            Site            : Site
            ShopFloor       : ShopFloor
            WorkCenter      : WorkCenter
            TimeEntry       : TimeEntry
            Allocation      : TimeAllocation
            Status          : RecordStatus
        }
    
    type TimeRecordId = uint32
(*
    let updateRecordFromJSON updatetimerecord user timestamp log jsonObj  =
        let timerecordId = jsonObj.Id
        let dto = jsonObj.DTO
        let timerecord = fromDTO dto
        log user timestamp timerecord

        let dbRes = updatetimerecord user timestamp timerecordId timerecord
        match dbRes with
            | Success res -> HTTP 200 //ok
            | Failure msg -> HTTP 404 //failure

*)
    (* 
        Types for User information
    *)

    type UserName = Login of string

    type AuthLevel = | Entry | Maintenance | Admin

    type SiteAccess = | All | SiteOnly of Site


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