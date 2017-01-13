namespace TimeEntry

module DomainTypes =
    open System
    open TimeEntry.Result
    open TimeEntry.ConstrainedString
    
    //Domain Model:
    // http://fsharpforfunandprofit.com/ddd/

    (* DEFINE ONE SITE *)
    type Site  = Site of string
    
     (* DEFINE ONE SHOPFLOOR *)
    type ShopFloor  = ShopFloor of string

    type ShopFloorInfo = 
        {
            Site : Site
            ShopFloor : ShopFloor
        }

    (* DEFINE ONE WORKCENTER *)
    
    ///Type modeling hours of a day
    type Hour = Hour of uint32

    type WorkCenter  = WorkCenter of string
    type WorkCenterInfo = 
        {
            WorkCenter      : WorkCenter
            ShopFloorInfo   : ShopFloorInfo
            StartHour       : Hour
            EndHour         : Hour
        }
    
    (* DEFINE ONE MACHINE *)
    type Machine = Machine of string

    type MachineInfo =
        {
            ShopFloorInfo   : ShopFloorInfo
            Machine         : Machine
        }


    (* DEFINE ONE ACTIVITY *)
    type ShopfloorAccess  = 
        | AllShopFloors  
        | ShopFloorList of ShopFloor list

    type WorkCenterAccess = 
        | AllWorkCenters 
        | WorkCenterList of WorkCenter list

    type RecordLevel = 
        | ShopFloorLevel  of ShopfloorAccess
        | WorkCenterLevel of WorkCenterAccess

    type TimeType = 
            | MachineTime
            | LabourTime
            with
                override x.ToString() =
                        match x with
                            | MachineTime -> "machine"
                            | LabourTime  -> "labour"  
  
    type ActivityCode = ActivityCode of string
    

    type ExtraInfo = 
            | WithInfo 
            | WithoutInfo
            with
                override x.ToString() =
                    match x with
                        | WithInfo      -> "withinfo"
                        | WithoutInfo   -> "withoutinfo"

    type ActivityLink = 
            | Linked   of ActivityCode
            | NotLinked

    type Activity =
        {
            Site            : Site
            Code            : ActivityCode
            RecordLevel     : RecordLevel
            TimeType        : TimeType
            ActivityLink    : ActivityLink
            ExtraInfo       : ExtraInfo
        }
        

    type ActivityDetails =
        {
            Machine     :   Machine
            Cause       :   string
            Solution    :   string 
            Comments    :   string
        }

    type ActivityInfo = 
        | Normal         of ActivityCode
        | Detailed       of ActivityCode  * ActivityDetails

    type ActivityInfoId = uint32

    (* DEFINE ONE USER *)
    type SiteAccess = | AllSites | SiteList of Site list

    type AuthLevel = | User | KeyUser | Admin

    type Login = Login of string
    type UserName = UserName of string
    type UserInfo =
        {
            Login       : Login
            Name        : UserName
            SiteAccess  : SiteAccess
            Level       : AuthLevel
        }

    (* DEFINE ONE WORKORER *)
    type WorkOrder = WorkOrder of string

    type ItemCode  = ItemCode of string

    type WorkOrderStatus =
        | Open
        | Closed

    //Unit of time recorded in the system: decimal of time
    type TimeHr = TimeHr of float32

    type WorkOrderInfo =
        {
            WorkOrder           : WorkOrder
            WorkCenter          : WorkCenter              
            ItemCode            : ItemCode
            TotalMachineTimeHr  : TimeHr
            TotalLabourTimeHr   : TimeHr
            Status              : WorkOrderStatus
        }

    (* DEFINE TYPES TO RECORD TIME *)
    type Duration =     
        {
            StartTime   : DateTime
            EndTime     : DateTime
        }
        with
            member x.Duration = x.EndTime - x.StartTime
            member x.ToTimeHr() =  
                let minutes = (float x.Duration.TotalMinutes)
                System.Math.Round(minutes / 60., 4)
                |> float32
                |> TimeHr
    
    type NbPeople = NbPeople of float32
    
    type TimeEntryMode = 
        | MachineOnly of Duration
        | MachineAndLabour of Duration * NbPeople
        | LabourOnly of Duration * NbPeople


    type TimeAttribution = 
        | WorkOrderEntry of WorkOrderInfo
        | ActivityEntry of ActivityInfo

    type RecordStatus = 
        | Entered
        | Validated

    type TimeRecord =
        {
            Site            : Site
            ShopFloor       : ShopFloor
            WorkCenter      : WorkCenter option
            Duration        : Duration
            TimeType        : TimeType
            Attribution     : TimeAttribution
            Status          : RecordStatus
        }
    
    //Model id of record in Database
    type TimeRecordId = uint32

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
                     
    //Model the creation of one time Record
    //type RecordTime = UserInfo -> Site -> TimeAttribution -> TimeEntryMode -> ShopFloor -> WorkCenter option -> Duration * NbPerson -> TimeRecord

    // Use Types
    //type EntryRequest = { User: UserInfo; Entry : TimeRecord }

    //type EntryResponse = { Id: int; Request: EntryRequest}

    //Use Case 1: Entry of time
    // Db failure or JSon failure
    //type AddEntryData = EntryRequest -> Result<EntryResponse>