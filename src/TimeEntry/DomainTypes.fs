namespace TimeEntry

module DomainTypes =
    open System
    open TimeEntry.Result
    open TimeEntry.ConstrainedTypes
    
    //Domain Model:
    // http://fsharpforfunandprofit.com/ddd/

    (* DEFINE ONE SITE *)
    type Site  = Site of String3
    
     (* DEFINE ONE SHOPFLOOR *)
    type ShopFloor  = ShopFloor of String5

    type ShopFloorInfo = 
        {
            Site : Site
            ShopFloor : ShopFloor
        }

    (* DEFINE ONE WORKCENTER *)
    
    ///Type modeling hours of a day
    type Hour = Hour of uint32

    type WorkCenter  = WorkCenter of String5
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
  
    type ActivityCode = ActivityCode of String4
    

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

    type AuthLevel = | User | KeyUser | Admin

    type Login = Login of String8
    type UserName = UserName of String50
    type Password = Password of String50


    type UserInfo =
        {
            Login       : Login
            Name        : UserName
            Password    : Password
            SiteAccess  : SiteAccess
            Level       : AuthLevel
        }

    type User = 
        | LoggedUser of UserInfo
        | UnLoggedUser of Login * Password


    (* DEFINE ONE WORKORER *)
    type WorkOrder = WorkOrder of String10

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

// --------------------------------------------------

/// Capabitilities available in application
// https://gist.github.com/swlaschin/909c5b24bf921e5baa8c#file-capabilitybasedsecurity_consoleexample-fsx

    type SelectSiteCap      = (Site -> Result<Site>)
    type DesactivateSiteCap = (Site -> Result<unit>)
    type ActivateSiteCap    = (Site -> Result<unit>)
    
    type CreateSiteCap      = (string -> Result<Site>)


    type SiteMaintenanceCap = 
        { 
            Creation    : CreateSiteCap
            Activate    : ActivateSiteCap
            Desactivate : DesactivateSiteCap
        }


    type CreateShopFloorCap = ( unit -> Result<ShopFloorInfo> )
    type UpdateShopFloorCap = ( unit -> ShopFloorInfo -> Result<ShopFloorInfo> )
    type DesactivateShopFloorCap = ( unit -> Result<unit> )


    type ShopFloorMaintenanceCap = 
        { 
            Creation    : CreateShopFloorCap
            Update      : UpdateShopFloorCap
            Desactivate : DesactivateShopFloorCap
        }


    type UpdatePasswordCap =  ( Login -> Password -> Result<UserInfo> )
    type UpdateUserNameCap =  ( unit -> UserName -> Result<UserInfo> )
    type UpdateUserInfoCap = ( unit -> UserInfo -> Result<UserInfo> )

    type UserMaintenanceCap = 
        {
            UpdatePassword : UpdatePasswordCap option
            UpdateUserName : UpdateUserNameCap option 
            UpdateUserInfo : UpdateUserInfoCap option

        }

    type CapabilityProvider = 
        {
            //User may have the right to maintain site, shopfloor, workcenter
            SiteMaintenance      : SiteMaintenanceCap option
            ShopFloorMaintenance : ShopFloorMaintenanceCap option
            UserMaintenance      : UserMaintenanceCap option
        }


    //To be put in capabilityProvider
    type Capability = 
        | LoginCap
        | UpdatePasswordCap
        | CreateSiteCap
        | SelectSiteCap
        | UnselectSite
        | DesactivateSite
        | Logout

(* INPUT DATA *)

    type UserCredentialData     = { Login : Login ; Password : Password }
    type LoggedInData           = { UserInfo : UserInfo }
    type UpdatePasswordData     = { UserInfo : UserInfo }
    type SiteSelectedData       = { UserInfo : UserInfo ; Site : Site}
    type ShopFloorSelectedData  = { Site  : Site  ; ShopFloor : ShopFloor; UserInfo : UserInfo }

(*  SERVICES *)
    
    //Use case or services: 
    type UserLogin  = UserCredentialData  -> Result<LoggedInData>

    type UpdatePassword = Login -> Password -> LoggedInData -> Result<UpdatePasswordData> 
    
    type SelectSite = Site -> LoggedInData -> Result<SiteSelectedData>

    type UnSelectSite = SiteSelectedData -> LoggedInData

    //User may not have the right to create one site or input is invalid
    type CreateSite = string -> LoggedInData -> Result<SiteSelectedData>

    //User may not have the right to update one site or input is invalid
    type DesactivateSite = SiteSelectedData -> Result<unit>

(* APPLICATION STATE *)

    type TimeEntryState = 
        | LoggedOut
        //Connect user and list possible actions he can do:
        | LoggedIn              of LoggedInData
        | PasswordUpdated       of UpdatePasswordData
        | SiteSelected          of SiteSelectedData
        | SiteCreated           of SiteSelectedData
        | ShopFloorSelected     of ShopFloorSelectedData   



    //Model the creation of one time Record
    //type RecordTime = UserInfo -> Site -> TimeAttribution -> TimeEntryMode -> ShopFloor -> WorkCenter option -> Duration * NbPerson -> TimeRecord

    // Use Types
    //type EntryRequest = { User: UserInfo; Entry : TimeRecord }

    //type EntryResponse = { Id: int; Request: EntryRequest}

    //Use Case 1: Entry of time
    // Db failure or JSon failure
    //type AddEntryData = EntryRequest -> Result<EntryResponse>