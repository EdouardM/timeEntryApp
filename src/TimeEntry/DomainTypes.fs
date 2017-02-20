namespace TimeEntry

//Use this attribute to make domain types visibles in all other modules in the project
[<AutoOpen>]
module DomainTypes =
    open System
    open TimeEntry.Result
    open TimeEntry.ConstrainedTypes
    
    //Domain Model:
    // http://fsharpforfunandprofit.com/ddd/

    (* TYPES FOR TIME *)
    ///Type modeling hours of a day
    type Hour = Hour of uint32

    ///Type modeling Minutes 
    type Minute = Minute of uint32

    ///Type modeling Seconds
    type Second = Second of uint32

    type Time = 
        {
            Hour    : Hour
            Minutes : Minute
            Seconds : Second
        }

    (* RECORD ACTIVE STATUS IN DB *)
    type ActiveStatus = 
        | Active
        | Inactive
        | All

    (* DEFINE ONE SITE *)
    type Site  = Site of String3
        with
            override this.ToString() = 
                let (Site (String3 s)) = this
                s
    
     (* DEFINE ONE SHOPFLOOR *)
    type ShopFloor  = ShopFloor of String5
        with
            override this.ToString() = 
                let (ShopFloor (String5 sf)) = this
                sf

    type ShopFloorInfo = 
        {
            Site : Site
            ShopFloor : ShopFloor
        }

    (* DEFINE ONE WORKCENTER *)
    


    type WorkCenter  = WorkCenter of String5
        with
            override this.ToString() = 
                let (WorkCenter (String5 wc)) = this
                wc 

    type WorkCenterInfo = 
        {
            WorkCenter      : WorkCenter
            ShopFloorInfo   : ShopFloorInfo
            StartHour       : Hour
            EndHour         : Hour
        }
    
    (* DEFINE ONE MACHINE *)
    type Machine = Machine of String10

    type MachineInfo =
        {
            ShopFloorInfo   : ShopFloorInfo
            Machine         : Machine
        }


    (* DEFINE ONE ACTIVITY *)
    [<RequireQualifiedAccess>]
    type ShopFloorAccess  = 
        | All  
        | List of ShopFloor list
    
    [<RequireQualifiedAccess>]
    type WorkCenterAccess = 
        | All 
        | List of WorkCenter list

    [<RequireQualifiedAccess>]
    type RecordLevel = 
        | ShopFloor  of ShopFloorAccess
        | WorkCenter of WorkCenterAccess
        with
            override this.ToString() =
                match this with
                    | ShopFloor  _   -> "shopfloor"
                    | WorkCenter _   -> "workcenter" 

    type TimeType = 
            | MachineTime
            | LabourTime
            with
                override x.ToString() =
                        match x with
                            | MachineTime -> "machine"
                            | LabourTime  -> "labour"  
  
    type ActivityCode = ActivityCode of String4
            with
                override this.ToString() = 
                        let (ActivityCode (String4 act)) = this
                        act

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
            Cause       :   String50
            Solution    :   String50 
            Comments    :   String200
        }

    type ActivityInfo = 
        | Normal         of ActivityCode
        | Detailed       of ActivityCode  * ActivityDetails

    type ActivityInfoId = uint32

    (* DEFINE ONE USER *)
    type SiteAccess = | AllSites | SiteList of Site list

    type AuthLevel = | Viewer | User | KeyUser | Admin

    type Login = Login of String8
        with
            override x.ToString() = 
                let (Login (String8 l)) = x
                l

    type UserName = UserName of String50
        with
            override x.ToString() = 
                let (UserName (String50 n)) = x
                n

    type Password = Password of String50


    type UserInfo =
        {
            Login       : Login
            Name        : UserName
            Password    : Password
            SiteAccess  : SiteAccess
            Level       : AuthLevel
        }

    (* DEFINE ONE WORKORER *)
    type WorkOrder = WorkOrder of String10
            with
            override this.ToString() = 
                let (WorkOrder (String10 wo)) = this
                wo

    type ItemCode  = ItemCode of String6

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
    
    type EntryMode = 
        | MachineOnly     
        | MachineAndLabour 
        | LabourOnly      

    [<RequireQualifiedAccess>]
    type Attribution = 
        | WorkOrder of WorkOrder
        | Activity  of ActivityCode

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
            NbPeople        : NbPeople
            Attribution     : Attribution
            Status          : RecordStatus
        }
    
    //Model id of record in Database
    type TimeRecordId = uint32

    type EntryMethod = 
        | ProductionLine
        | Individual

    [<RequireQualifiedAccess>]
    type EntryLevel = 
        | ShopFloor
        | WorkCenter
    
    [<RequireQualifiedAccess>]
    type AttributionType = 
        | WorkOrder
        | Activity

// --------------------------------------------------

(* CAPABILITIES *)
// https://gist.github.com/swlaschin/909c5b24bf921e5baa8c#file-capabilitybasedsecurity_consoleexample-fsx
    [<RequireQualifiedAccess>]
    type Cap = 
        | Login
        | UpdatePassword
        | CreateSite
        | SelectSite
        | UnselectSite
        | SelectEntryMethod
        | UnselectEntryMethod
        | SelectEntryLevel
        | UnselectEntryLevel
        | SelectShopFloor
        | UnselectShopFloor
        | SelectWorkCenter
        | UnselectWorkCenter
        | SelectEntryMode
        | UnselectEntryMode
        | SelectAttributionType
        | UnselectAttrbutionType
        | SelectActivity
        | SelectWorkOrder
        | UnselectAttribution
        | EnterDuration
        | CancelDuration
        | EnterNbPeople
        | CancelNbPeople
        | RecordTime
        | Logout
        | Exit

(* INPUT DATA *)

    type UserCredentialData     = { Login : Login ; Password : Password }
    type LoggedInData           = { UserInfo : UserInfo }
    type UpdatePasswordData     = { UserInfo : UserInfo }
    type SiteSelectedData       = { UserInfo : UserInfo ; Site : Site}

    type EntryMethodSelectedData = 
        { 
            UserInfo : UserInfo ; 
            Site : Site; 
            EntryMethod : EntryMethod 
        }
    
    type EntryLevelSelectedData = 
        { 
            UserInfo: UserInfo; 
            Site: Site; 
            EntryMethod : EntryMethod; 
            EntryLevel : EntryLevel
        }  
    
    type ShopFloorSelectedData  = 
        { 
            Site        : Site
            ShopFloor   : ShopFloor
            UserInfo    : UserInfo
            EntryMethod : EntryMethod
            EntryLevel  : EntryLevel 
        }
    type WorkCenterSelectedData = 
        {   
            Site        : Site 
            ShopFloor   : ShopFloor
            WorkCenter  : WorkCenter option
            UserInfo    : UserInfo 
            EntryMethod : EntryMethod 
            EntryLevel  : EntryLevel
        }

    type EntryModeSelectedData = 
        { 
            Site        : Site 
            ShopFloor   : ShopFloor
            WorkCenter  : WorkCenter option
            UserInfo    : UserInfo 
            EntryMethod : EntryMethod 
            EntryLevel  : EntryLevel
            EntryMode   : EntryMode
        }

    type AttributionTypeSelectedData = 
        { 
            Site            : Site 
            ShopFloor       : ShopFloor
            WorkCenter      : WorkCenter option
            UserInfo        : UserInfo 
            EntryMethod     : EntryMethod 
            EntryLevel      : EntryLevel
            EntryMode       : EntryMode
            AttributionType : AttributionType
        }
    
    type AttributionSelectedData = 
        { 
            Site            : Site 
            ShopFloor       : ShopFloor
            WorkCenter      : WorkCenter option
            UserInfo        : UserInfo 
            EntryMethod     : EntryMethod 
            EntryLevel      : EntryLevel
            EntryMode       : EntryMode
            AttributionType : AttributionType
            Attribution     : Attribution
        }

    type DurationEnteredData = 
        {
            Site            : Site 
            ShopFloor       : ShopFloor
            WorkCenter      : WorkCenter option
            UserInfo        : UserInfo 
            EntryMethod     : EntryMethod 
            EntryLevel      : EntryLevel
            EntryMode       : EntryMode
            AttributionType : AttributionType
            Attribution     : Attribution
            Duration        : Duration
        }    

    type NbPeopleEnteredData =
        {
            Site            : Site 
            ShopFloor       : ShopFloor
            WorkCenter      : WorkCenter option
            UserInfo        : UserInfo 
            EntryMethod     : EntryMethod 
            EntryLevel      : EntryLevel
            EntryMode       : EntryMode
            AttributionType : AttributionType
            Attribution     : Attribution
            Duration        : Duration
            NbPeople        : NbPeople
        }
(*

    type Attribution = 
        | Activity
        | WorkOrder 

    if activity     ==> Record Activity : Normal Activity Code 
    if workorder    ==> Record WorkOrder: WorkOrder Code 

    type TimeRecordData = 
        { 
            Context     : EntryModeSelectedData
            Attribution : Attribution
            Duration    : Duration
            NbPeople    : NbPeople
        }
*)
(*  SERVICES *)
    
    //Use case or services: 
    type UserLogin  = UserCredentialData  -> Result<LoggedInData>

    type UpdatePassword = Login -> Password -> LoggedInData -> Result<UpdatePasswordData> 
    
    type DisplaySites = LoggedInData -> Result<string list>
    
    type SelectSite = Site -> LoggedInData -> Result<SiteSelectedData>

    type UnSelectSite = SiteSelectedData -> LoggedInData

    type DisplayEntryMethod = SiteSelectedData -> Result<string list>

    type SelectEntryMethod = EntryMethod -> SiteSelectedData -> Result<EntryMethodSelectedData>

    type DisplayEntryLevel = EntryMethodSelectedData -> Result<string list>

    type SelectEntryLevel = EntryLevel -> EntryMethodSelectedData -> Result<EntryLevelSelectedData>

    type DisplayShopFloors = EntryLevelSelectedData -> Result<string list>

    type SelectShopFloor = ShopFloor -> EntryLevelSelectedData -> Result<ShopFloorSelectedData>

    type DisplayWorkCenters = ShopFloorSelectedData -> Result<string list>

    type SelectWorkCenter = WorkCenter -> ShopFloorSelectedData -> Result<WorkCenterSelectedData>

    type DisplayEntryModes = WorkCenterSelectedData -> Result<string list>

    type SelectEntryMode  = EntryMode -> WorkCenterSelectedData -> Result<EntryModeSelectedData>

    type DisplayAttributionTypes = EntryModeSelectedData -> Result<string list>

    type SelectAttributionType = AttributionType -> EntryModeSelectedData -> Result<AttributionTypeSelectedData>
    
    type DisplayActivityCodes = AttributionTypeSelectedData -> Result<string list>

    type DisplayWorkOrders    = AttributionTypeSelectedData -> Result<string list>
    
    type SelectAttribution = Attribution -> AttributionTypeSelectedData -> Result<AttributionSelectedData> 

    type EnterDuration = Duration -> AttributionSelectedData -> Result<DurationEnteredData>

    type EnterNbPeople = NbPeople -> DurationEnteredData -> Result<NbPeopleEnteredData>

    //User may not have the right to create one site or input is invalid
    type CreateSite = string -> LoggedInData -> Result<SiteSelectedData>

    //User may not have the right to update one site or input is invalid
    type DesactivateSite = SiteSelectedData -> Result<unit>

(* APPLICATION STATE *)

    type TimeEntryState = 
        | LoggedOut
        | Exit
        //Connect user and list possible actions he can do:
        | LoggedIn                  of LoggedInData
        | SiteCreated               of SiteSelectedData
        | PasswordUpdated           of UpdatePasswordData
        | SiteSelected              of SiteSelectedData
        | EntryMethodSelected       of EntryMethodSelectedData  
        | EntryLevelSelected        of EntryLevelSelectedData
        | ShopFloorSelected         of ShopFloorSelectedData   
        | WorkCenterSelected        of WorkCenterSelectedData
        | EntryModeSelected         of EntryModeSelectedData
        | AttributionTypeSelected   of AttributionTypeSelectedData
        | AttributionSelected       of AttributionSelectedData
        | DurationEntered           of DurationEnteredData
        | NbPeopleEntered           of NbPeopleEnteredData

(* 
    //Model the creation of one time Record
    //type RecordTime = UserInfo -> Site -> EntryMethod  -> ShopFloor -> WorkCenter option -> TimeEntryMode -> 
            //2 cases   Production Line - Multiple records:  -> TimeAttribution -> Duration * NbPerson -> TimeRecord list
            //          Individual - one record: -> TimeAttribution -> Duration * NbPerson -> TimeRecord list with only one item

    Attribution type -> Attribution display (cache) Selected -> Duration Entered -> NbPerson Entered 
        -> Time record validated
        -> Go back to Attribution selection  
*)