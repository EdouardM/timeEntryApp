namespace TimeEntry

module Constructors =
    open System
    open TimeEntry.Result
    open TimeEntry.ConstrainedTypes
    open TimeEntry.DomainTypes

    ///Helper function to validate domain types values against a list of valid input
    let validate ctor name validList = 
        (fun id -> 
            match List.exists (fun s -> s = id ) validList with
                | true -> Success (id)
                | false -> Failure <| sprintf "Can't find %s: %s" name id)
        >=> ctor
    
    ///Helper function to create a new domain type value outside of the list of existing valid records
    let create ctor name validList = 
        (fun id -> 
            match List.exists (fun s -> s = id ) validList with
                | false -> Success (id)
                | true  -> Failure <| sprintf "The value exists. Can't create %s: %s" name id)
        >=> ctor

    (* SITE CONSTRUCTORS / VALIDATION *)
    module Site = 
        let create = create ( stringExact3 >>= DomainTypes.Site)  "site"
        let validate = validate ( stringExact3  >>= DomainTypes.Site)  "site"

    (* SHOPFLOOR CONSTRUCTORS *)
    module ShopFloor = 
        let create = create ( stringExact5 >>= ShopFloor)  "shopfloor"
        let validate = validate (stringExact5 >>= ShopFloor) "shopfloor"

    module ShopFloorInfo =
        let create site shopfloor = 
            { ShopFloorInfo.Site = site; ShopFloorInfo.ShopFloor = shopfloor}
    (* HOUR *)
    module Hour = 
        let validate = 
            function
                | h when h > 23u -> Failure "Hour can't be bigger than 23."
                | h -> Success (Hour h)
        
    (* WORKCENTER CONSTRUCTORS *)
    module WorkCenter =
        let create = create (failIfNullString >=> stringMax5 >>= WorkCenter) "workcetner"
        let validate = validate (stringMax5 >>= WorkCenter) "workcenter"
    module WorkCenterInfo = 
        let create shopFloor workCenter startTime endTime = 
            { WorkCenter = workCenter; ShopFloorInfo = shopFloor; StartHour = startTime; EndHour = endTime }
        
    (* MACHINE CONSTRUCTORS *)
    module Machine = 
        let create = create (failIfNullString >=> stringMax10 >>= Machine) "machine"
        let validate = validate (stringMax10 >>= Machine) "machine"

    module MachineInfo = 
        let create machine shopFloor =
            { Machine = machine; ShopFloorInfo = shopFloor}
        
    (* ACTIVITY CONSTRUCTORS *)
    module ShopFloorAccess = 
        let validate accessAll authorizedShopFloor =
            match accessAll, authorizedShopFloor with
                | true, []      -> Success (AllShopFloors)
                | false, []     -> Failure "Must have at least one authorized shopfloor when access is set to shopfloor list."
                | false, sf     -> Success (ShopFloorList sf)
                | true, sf      -> Failure "Should have empty list of shopfloors when access is set to all."
    module WorkCenterAccess = 
        let validate accessAll authorizedWorkCenter =
            match accessAll, authorizedWorkCenter with
                | true, []      -> Success (AllWorkCenters)
                | false, []     -> Failure "Must have at least one authorized workcenter when access is set to workcenter list."
                | false, wc     -> Success (WorkCenterList wc)
                | true, wc      -> Failure "Should have empty list of workcenter when access is set to all."
    module TimeType =     
        let validate = 
            function
                | "labour"      -> Success (LabourTime)
                | "machine"     -> Success (MachineTime)
                | ty            -> Failure <| sprintf "Invalid time type: %s" ty
    module ExtraInfo =
        let validate = 
            function 
                | "withinfo"    -> Success (ExtraInfo.WithInfo)
                | "withoutinfo" -> Success (ExtraInfo.WithoutInfo)
                | extra         -> Failure <| sprintf "Invalid extra info: %s" extra

    module ActivityCode = 
        let validate = validate (stringMax4 >>= ActivityCode) "activity"

    module Activity =
        let validate site code level timetype link extrainfo =
            { Site = site; RecordLevel = level; Code = code; TimeType = timetype; ActivityLink = link; ExtraInfo = extrainfo }
        
    module ActivityLink =
        let validate (validateActivityCode: string -> Result<ActivityCode>) islinked linkedActivity = 
            match islinked, linkedActivity with
                | true, Some act    -> (validateActivityCode act |> Result.map Linked )
                | false,    None    -> Success NotLinked
                | false, Some act   -> Failure "One activity marked as not linked cannot have a linked activity."
                | true,     None    -> Failure "One activity marked as linked must have a linked activity."
    module Cause =
        let validate   = failIfNullString >=> stringMax50
    module Solution = 
        let validate = stringMax50
    module Comments =
        let validate = stringMax200

    module ActivityDetails =
        let validate machine cause solution comments = 
            { ActivityDetails.Machine = machine; Cause = cause; Solution = solution; Comments = comments}
    
    module ActivityInfo = 
        let validate activity details =
            match details  with
                | Some d      -> Success (Detailed (activity,d))
                | None        -> Success (Normal activity)
                
    (* USER CREDENTIAL *)
    module UserCredentialData =
        let create login password = 
            { UserCredentialData.Login = login; Password = password }

    (* USER CONSTRUCTORS *)
    module UserName =     
        let create = failIfNullString >=> stringMax50 >>= UserName
    module Login =     
        let create   = create (failIfNullString >=> stringMax8 >>= Login) "user login"

        let validate = validate (stringMax8 >>= Login) "user login"
    module Password =     
        let create = (failIfNullString >=> stringMax50 >>= Password)

    module SiteAccess = 
        let validate accessAll authorizedSites = 
            match accessAll, authorizedSites with
                | true, []      -> Success (AllSites)
                | false, []     -> Failure "User must have at least one authorized site when access is set to Site List."
                | false, sites  -> Success (SiteList sites)
                | true, l       -> Failure "Should have empty list of sites when access is set to AllSites."
    module AuthLevel =     
        let validate = 
            function
                | "user"    -> Success User
                | "keyuser" -> Success KeyUser
                | "admin"   -> Success Admin
                | "viewer"  -> Success Viewer
                | level     -> Failure <| sprintf "Unexpected value for authorization level: %s" level

    module UserInfo = 
        let validate login name password access level = 
            { Login = login ; Name = name; Password = password; SiteAccess = access; Level = level}

    (* WORKORDER CONSTRUCTORS *)
    module WorkOrder = 
        let create = stringExactNum10 >>= WorkOrder
        let validate = validate (stringExact10 >>= WorkOrder) "work order number"
    module ItemCode = 
        let validate = validate (stringMax6 >>= ItemCode) "item code"
    module WorkOrderStatus = 
        let validate =
            function
            | "open"    ->  Success Open
            | "closed"  ->  Success Closed
            | _         ->  Failure "Invalid workorder status."

    module TimeHr =
        let validate = 
            function
                | h when h < 0.f        -> Failure "Total hour can't be negative."
                | h when h > 999.9999f  -> Failure "Total hour can't be bigger than 999.9999 hr."
                | h -> Success (TimeHr h)

    module WorkOrderInfo = 
        let validate workOrder workCenter itemCode totalMachine totalLabour status = 
            { 
                WorkOrder = workOrder; 
                WorkCenter = workCenter; 
                ItemCode = itemCode; 
                TotalMachineTimeHr = totalMachine; 
                TotalLabourTimeHr = totalLabour; 
                Status = status
            }    

    (* TIME RECORD CONSTRUCTOS*)
    module Duration = 
        let validate (startTime: DateTime) (endTime: DateTime) =
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


    ///Validate a number of people:
    ///Number of people can only be integer or half of integer and positive
    module NbPeople = 
        let validate nb = 
            if nb >= 0.f then
                let r = floor nb
                let d = nb - r
                if d >= 0.5f then Success (NbPeople (r + 0.5f))
                else Success(NbPeople r)
            else Failure <| sprintf "Number of people can't be negative.\nNb people: %.2f" nb

    module RecordStatus =
        let validate = 
            function
                | "entered"   -> Success Entered
                | "validated" -> Success Validated
                | status      -> Failure <| sprintf "Invalid Record Status: %s" status 

    module TimeRecord = 
        let validate site shopfloor workcenter attribution timetype duration status =
            { Site = site; ShopFloor = shopfloor; WorkCenter = workcenter; TimeType = timetype;Duration = duration; Attribution = attribution; Status = status}

    module EntryMethod = 
        let validate = 
            function
                | "P"   -> Success ProductionLine
                | "I"   -> Success Individual
                | mode  -> Failure <| sprintf "Invalid Record mode: %s. Valid choices are : 'P' or 'I'" mode 

    module EntryLevel =
        let validate = 
            function
                | "S"   -> Success EntryLevel.ShopFloor
                | "W"   -> Success EntryLevel.WorkCenter
                | input -> Failure <| sprintf "Invalid input: %s. Valid choices are : 'S' or 'W'" input