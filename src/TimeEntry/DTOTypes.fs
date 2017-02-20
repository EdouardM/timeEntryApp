namespace TimeEntry
    module DTO =
        open System
        open Result
        open ConstrainedTypes
        open DomainTypes
        open Constructors

        module UserCredential =
            type UserCredentialDTO = 
                {
                    Login : string
                    Password : string 
                }
            let createDTO login password = 
                { Login = login ; Password = password }

            let toDTO (usercredential : UserCredentialData) = 
                let (Login (String8 l)) = usercredential.Login
                let (Password (String50 p)) = usercredential.Password
                { Login = l ; Password = p }

            let fromDTO 
                logins
                (usercredDTO : UserCredentialDTO) = 
                let loginRes = Login.validate logins usercredDTO.Login
                let passwordRes = Password.create usercredDTO.Password
                UserCredentialData.create <!> loginRes <*> passwordRes

        module Duration = 

            type DurationDTO = 
                {
                    Date         : string
                    StartTime   : string  
                    EndTime     : string
                }
            let createDTO date starttime endtime = 
                { Date = date; StartTime = starttime; EndTime = endtime }

            let fromDTO (input: DurationDTO) =
                result { 
                    let! startDT = DateTime.validate input.Date input.StartTime 
                    let! endDT   = DateTime.validate input.Date input.EndTime 
                    return! Duration.validate startDT endDT
                }

        module NbPeople = 

            type NbPeopleDTO = 
                { NbPeople : string }
            
            let createDTO nb = {NbPeople = nb}
            let fromDTO (input: NbPeopleDTO) =
                result { 
                    let! nb = Float.validate input.NbPeople
                            
                    return! NbPeople.validate nb
                }



    (*      module WorkCenterInfo =
            ///Domain to store data deserialized from JSON format
            type WorkCenterInfoDTO =
                {
                    Site        : string
                    ShopFloor   : string
                    WorkCenter  : string
                    StartHour   : uint32
                    EndHour     : uint32

                }
            
            let workCenterInfoFromDTO 
                sites
                shopfloors
                workcenters
                (dto: WorkCenterInfoDTO) =
                    let siteRes          = Site.create sites dto.Site
                    let shopfloorRes     = ShopFloor.validate shopfloors dto.ShopFloor
                    let shopfloorInfoRes = ShopFloorInfo.create <!> siteRes <*> shopfloorRes

                    let workcenterResult = WorkCenter.validate workcenters dto.WorkCenter
                    let startHourResult  = Hour.validate dto.StartHour
                    let endHourResult    = Hour.validate dto.EndHour   
                    WorkCenterInfo.create 
                    <!> shopfloorInfoRes
                    <*> workcenterResult 
                    <*> startHourResult 
                    <*> endHourResult
            
        module WorkOrder = 
            type WorkOrderDTO =
                {
                    WorkOrder       : string
                    ItemCode        : string
                }
*)
(*            let workOrderFromDTO 
                validWorkOrders
                validItemCodes
                (dto: WorkOrderDTO) =
                    let workorderResult  = WorkOrder.validate validWorkOrders dto.WorkOrder
                    let itemCodeResult   = ItemCode.validate validItemCodes dto.ItemCode
                    WorkOrderInfo.validate
                    <!> workorderResult
                    <*> itemCodeResult 
*)
        module TimeRecord = 
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
            
(*            let timeRecordFromDTO 
                validSites
                validShopFloors
                validWorkCenters
                (dto: TimeRecordDTO) =
                    let siteResult = Site.validate validSites dto.Site
                    let shopfloorResult  = ShopFloor.validate validShopFloors dto.ShopFloor
                    let workcenterResult = WorkCenter.validate validWorkCenters dto.WorkCenter
                    let workcenterInfoResult = WorkCenterInfo.create <!> siteResult <*> shopfloorResult <*> workcenterResult
                    let timetypeResult = 
                        match dto.TimeType with
                            | "Machine" -> Success MachineTime
                            | "Labour"  -> Success LabourTime
                            | ty         -> Failure <| sprintf "Wrong time type: %s" ty

                    let durationResult = Duration.validate dto.StartTime dto.EndTime
                    let nbPeopleResult = NbPeople.validate dto.NbPeople

                    let timeRecordResult = TimeRecord.validate <!> siteResult <*> shopfloorResult <*> workcenterResult
                    timeRecordResult
    *)
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