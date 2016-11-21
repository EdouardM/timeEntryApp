namespace TimeEntry
    module DTO =
        open Result
        open DomainTypes
        open Constructors
        ///Domain to store data deserialized from JSON format
        type WorkCenterInfoDTO =
            {
                Site        : string
                ShopFloor   : string
                WorkCenter  : string
                StartHour   : int
                EndHour     : int

            }
        
        let workCenterInfoFromDTO 
            validSites
            validShopFloors
            validWorkCenters
            (dto: WorkCenterInfoDTO) =
                let siteResult = createSite validSites dto.Site
                let shopfloorResult  = createShopfloor validShopFloors dto.ShopFloor
                let workcenterResult = createWorkCenter validWorkCenters dto.WorkCenter
                let startHourResult  = createHour dto.StartHour
                let endHourResult    = createHour dto.EndHour
                createWorkCenterInfo 
                <!> siteResult 
                <*> shopfloorResult 
                <*> workcenterResult 
                <*> startHourResult 
                <*> endHourResult
        
       
        type WorkOrderDTO =
            {
                WorkOrder       : string
                ItemCode        : string
                Weight          : float
            }
        let workOrderFromDTO 
            validWorkOrders
            validItemCodes
            (dto: WorkOrderDTO) =
                let workorderResult  = createWorkOrder validWorkOrders dto.WorkOrder
                let itemCodeResult   = createItemCode validItemCodes dto.ItemCode
                let weightResult     = createWeight dto.Weight
                createWorkOrderEntry
                <!> workorderResult
                <*> itemCodeResult 
                <*> weightResult 

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
        
        let timeRecordFromDTO 
            validSites
            validShopFloors
            validWorkCenters
            (dto: TimeRecordDTO) =
                let siteResult = createSite validSites dto.Site
                let shopfloorResult  = createShopfloor validShopFloors dto.ShopFloor
                let workcenterResult = createWorkCenter validWorkCenters dto.WorkCenter
                let workcenterInfoResult = createWorkCenterInfo <!> siteResult <*> shopfloorResult <*> workcenterResult
                let timetypeResult = 
                    match dto.TimeType with
                        | "Machine" -> Success MachineTime
                        | "Labour"  -> Success LabourTime
                        | ty         -> Failure <| sprintf "Wrong time type: %s" ty

                let durationResult = createDuration dto.StartTime dto.EndTime
                let nbPeopleResult = createNbPeople dto.NbPeople

                let timeEntryResult = createTimeEntry <!> timetypeResult <*> nbPeopleResult <*> durationResult
                let timeRecordResult = createTimeRecord <!> siteResult <*> shopfloorResult <*> workcenterResult <*> timeEntryResult
                timeRecordResult