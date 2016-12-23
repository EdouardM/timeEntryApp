namespace TimeEntry
    open System
    open TimeEntry.Result
    open TimeEntry.ConstrainedTypes
    open TimeEntry.Option
    open TimeEntry.DomainTypes
    open TimeEntry.Constructors

    module DataBase =

            type DBShopFloorInfo = 
                {
                    Site         : string
                    ShopFloor    : string
                }
            
            let toDBShopfloorInfo (sfinfo: ShopFloorInfo) =
                let (Site site)     = sfinfo.Site
                let (ShopFloor sf)  = sfinfo.ShopFloor
                { Site = value site; ShopFloor = value sf }
         
            let fromDBShopfloorInfo 
                sites
                shopfloors
                (sfinfo: DBShopFloorInfo) =
                    let siteRes = createSite sites sfinfo.Site
                    let shopfloorRes = createShopfloor shopfloors sfinfo.ShopFloor
                    createShopfloorInfo 
                    <!> siteRes
                    <*> shopfloorRes

            type DBWorkCenterInfo =
                {
                    WorkCenter      : string
                    ShopFloorInfo   : DBShopFloorInfo
                    StartHour       : uint32
                    EndHour         : uint32
                }
            
            let toDBWorkCenterInfo (wcInfo: WorkCenterInfo) =
                let (WorkCenter wc) = wcInfo.WorkCenter
                let sf              = toDBShopfloorInfo wcInfo.ShopFloorInfo
                let (Hour startH)   = wcInfo.StartHour
                let (Hour endH)     = wcInfo.EndHour
                { WorkCenter = value wc; ShopFloorInfo = sf; StartHour = startH; EndHour = endH}
         
            let fromDBWorkCenterInfo 
                sites
                shopfloors
                workcenters
                (wcInfo: DBWorkCenterInfo) =
                    let shopfloorInfoRes = fromDBShopfloorInfo sites shopfloors wcInfo.ShopFloorInfo 
                    let workcenterRes = createWorkCenter workcenters wcInfo.WorkCenter
                    let starthourRes = createHour wcInfo.StartHour
                    let endhourRes   = createHour wcInfo.EndHour
                    createWorkCenterInfo
                    <!> shopfloorInfoRes
                    <*> workcenterRes
                    <*> starthourRes
                    <*> endhourRes 
                    
            let fromDBWorkCenterInfo2 
                fromDBShopfloorInfo
                workcenters
                (wcInfo: DBWorkCenterInfo) =
                    let shopfloorInfoRes = fromDBShopfloorInfo wcInfo.ShopFloorInfo 
                    let workcenterRes = createWorkCenter workcenters wcInfo.WorkCenter
                    let starthourRes = createHour wcInfo.StartHour
                    let endhourRes   = createHour wcInfo.EndHour
                    createWorkCenterInfo
                    <!> shopfloorInfoRes
                    <*> workcenterRes
                    <*> starthourRes
                    <*> endhourRes 
            
            type DBWorkOrderEntry =
                {
                    WorkOrder : string
                    WorkCenter : string
                    ItemCode  : string
                    TotalMachineTimeHr : float32
                    TotalLabourTimeHr : float32
                    WorkOrderStatus    : string
                }

            let toDBWorkOrderEntry (wo: WorkOrderEntry) =
                let (WorkOrder strWo) = wo.WorkOrder
                let (WorkCenter strWc) = wo.WorkCenter
                let (ItemCode strItem) = wo.ItemCode
                let status =
                    match wo.Status with
                        | Open   -> "open"
                        | Closed -> "closed"

                let (TimeHr totalMachine) = wo.TotalMachineTimeHr
                let (TimeHr totalLabour)  = wo.TotalLabourTimeHr

                { 
                    WorkOrder           = value strWo; 
                    WorkCenter          = value strWc;
                    ItemCode            = value strItem; 
                    TotalMachineTimeHr  = totalMachine;
                    TotalLabourTimeHr   = totalLabour;
                    WorkOrderStatus     = status
                }

            let fromDBWorkOrderEntry
                workOrders
                workCenters
                itemCodes
                (wo: DBWorkOrderEntry) =
                let workOrderRes = createWorkOrder workOrders wo.WorkOrder
                let workCenterRes = createWorkCenter workCenters wo.WorkCenter
                let itemCodeRes  = createItemCode itemCodes wo.ItemCode
                let totalMachineTimeHrRes = createTimeHr wo.TotalMachineTimeHr
                let totalLabourTimeHrRes = createTimeHr wo.TotalLabourTimeHr
                let statusRes = createWorkOrderStatus wo.WorkOrderStatus
                createWorkOrderEntry 
                <!> workOrderRes
                <*> workCenterRes
                <*> itemCodeRes
                <*> totalMachineTimeHrRes
                <*> totalLabourTimeHrRes
                <*> statusRes  


            type DBEvent =
                {
                    Event : string
                    HasInfo : bool
                    AllowZeroPerson : bool
                }

            let toDBEvent =
                function
                    | WithInfo event    -> { Event = event; HasInfo = true; AllowZeroPerson = false }
                    | WithoutInfo event -> { Event = event; HasInfo = false; AllowZeroPerson = false }
                    | ZeroPerson event  ->  { Event = event; HasInfo = false; AllowZeroPerson = true }
            
            let fromDBEvent (event: DBEvent) =
                match event.HasInfo, event.AllowZeroPerson with
                    | true, false   -> Success (WithInfo event.Event)
                    | false, true   -> Success (ZeroPerson event.Event)
                    | false, false  -> Success (WithoutInfo event.Event)
                    | true, true    -> Failure "Event cannot require information and allow zero person."

            type DBEventEntry =
                {
                    Event       : DBEvent 
                    Machine     : string option
                    Cause       : string option
                    Solution    : string option
                    Comments    : string option
                }
            let toDBEventEntry = 
                function
                    | EventWithInfo (ev, info) -> 
                        let  (Machine machine) = info.Machine
                        let cause   = info.Cause
                        let solution = info.Solution
                        let comments = info.Comments
                        {   Event = toDBEvent ev; 
                            Machine = Some <| value machine; 
                            Cause = Some cause; 
                            Solution = Some solution; 
                            Comments = Some comments
                        }
                    | EventWithoutInfo (ev) -> 
                        {   Event = toDBEvent ev; 
                            Machine = None; 
                            Cause = None; 
                            Solution = None; 
                            Comments = None
                        }
                    | EventZeroPerson (ev) ->
                        {   Event = toDBEvent ev; 
                            Machine = None; 
                            Cause = None; 
                            Solution = None; 
                            Comments = None
                        }
                     
            let fromDBEventEntry  
                machines
                (eventEntry: DBEventEntry) =
                let eventRes = fromDBEvent eventEntry.Event
   
                let machineRes   = 
                        eventEntry.Machine
                        |> fromOption "Machine missing"
                        |> bind (createMachine machines)

                let causeRes     = fromOption "Cause missing" eventEntry.Cause
                let solutionRes  = fromOption "Cause missing" eventEntry.Solution
                let commentsRes  = fromOption "Comments missing" eventEntry.Comments
                
                let eventInfoResOpt = 
                            createEventInfo <!> machineRes <*> causeRes <*> solutionRes <*> commentsRes
                            |> Result.map Some
                                         
                createEventEntry <!> eventRes <*> eventInfoResOpt
                |> Result.flatten

            //DataBase model (pure)
            type DBTimeRecord =
                {
                    Site            : string
                    ShopFloor       : string
                    WorkCenter      : string
                    TimeType        : string
                    StartTime       : DateTime
                    EndTime         : DateTime
                    DurationHr      : float32
                    NbPeople        : float32
                    Allocation      : string
                    WorkOrderEntry  : DBWorkOrderEntry option
                    EventEntry      : DBEventEntry option
                    Status          : string
                }
            
            let allocationToString =
                function
                    | WorkOrderEntry wo -> "workorder"
                    | EventEntry ev     -> "event"

            let updateAllocation allocation timeRecord = 
                match allocation with
                    | WorkOrderEntry wo ->
                        { timeRecord with WorkOrderEntry = toDBWorkOrderEntry wo |> Some}
                    | EventEntry ev     ->
                        { timeRecord with EventEntry = toDBEventEntry ev |> Some}

            let recordStatusToString = 
                function
                    | Entered   -> "entered"
                    | Validated -> "validated"

            ///Function to convert one Domain Record into records to insert into Database
            let toDBTimeRecord  (time : TimeRecord) = 
                let (Site site) = time.Site
                let (ShopFloor shopfloor) = time.ShopFloor
                let (WorkCenter workcenter) = time.WorkCenter
                let status = recordStatusToString time.Status

                match time.TimeEntry with
                    //List of one record: Machine time
                    | MachineOnly duration ->
                        let machineRecord = {   
                            Site            = value site
                            ShopFloor       = value shopfloor 
                            WorkCenter      = value workcenter 
                            TimeType        = "machine"
                            StartTime       = duration.StartTime
                            EndTime         = duration.EndTime
                            DurationHr      = float32 duration.ToHr
                            NbPeople        = 0.f
                            Allocation      = allocationToString time.Allocation
                            WorkOrderEntry  = None
                            EventEntry      = None
                            Status          = status
                             }
                        let machineRecord' = updateAllocation time.Allocation machineRecord
                        [machineRecord']

                    //List of two records: Machine & Labour time
                    | MachineAndLabour (duration, nbPeople) ->
                        let (NbPeople nb) = nbPeople
                        let machineRecord = 
                            { 
                            Site        = value site
                            ShopFloor   = value shopfloor 
                            WorkCenter  = value workcenter
                            TimeType    = "machine" 
                            StartTime   = duration.StartTime
                            EndTime     = duration.EndTime
                            DurationHr  = float32 duration.ToHr
                            NbPeople    = 0.f
                            Allocation  = allocationToString time.Allocation
                            WorkOrderEntry = None
                            EventEntry  = None
                            Status      =  status }
                        let labourRecord = {machineRecord with TimeType = "labour"; NbPeople = nb}

                        [machineRecord; labourRecord] 
                        |> List.map (updateAllocation time.Allocation)

                    //List of one record: Labour time
                    | LabourOnly (duration, nbPeople) -> 
                        let (NbPeople nb) = nbPeople
                        let labourRecord = 
                            { 
                            Site        = value site
                            ShopFloor   = value shopfloor 
                            WorkCenter  = value workcenter
                            TimeType    = "labour" 
                            StartTime   = duration.StartTime
                            EndTime     = duration.EndTime
                            DurationHr  = float32 duration.ToHr
                            NbPeople    = nb
                            Allocation  = allocationToString time.Allocation
                            WorkOrderEntry = None
                            EventEntry  = None
                            Status      =  status }
                        let labourRecord' = updateAllocation time.Allocation labourRecord
                        [labourRecord']    
            
            let fromTimeRecordDB 
                sites
                shopfloors
                workcenters
                workorders
                machines
                itemcodes
                (time: DBTimeRecord) =
                    let siteRes = createSite sites time.Site
                    let shopFloorRes = createShopfloor shopfloors time.ShopFloor
                    let workCenterRes = createWorkCenter workcenters time.WorkCenter
                    
                    let timeTypeRes = createTimeType time.TimeType
                    let durationRes = createDuration time.StartTime time.EndTime
                    let nbPeopleRes = createNbPeople time.NbPeople

                    let timeEntryRes = createTimeEntry <!> timeTypeRes <*> nbPeopleRes <*> durationRes

                    match time.WorkOrderEntry, time.EventEntry with
                        | Some wo, None -> 
                            let workOrderEntryRes = (fromDBWorkOrderEntry workorders workcenters itemcodes) wo
                            let allocationRes = Result.map WorkOrderEntry workOrderEntryRes
                            createTimeRecord
                            <!> siteRes
                            <*> shopFloorRes
                            <*> workCenterRes
                            <*> allocationRes
                            <*> timeEntryRes
                        | None, Some ev -> 
                            let eventEntryRes = (fromDBEventEntry machines) ev
                            let allocationRes = Result.map EventEntry eventEntryRes
                            createTimeRecord
                            <!> siteRes
                            <*> shopFloorRes
                            <*> workCenterRes
                            <*> allocationRes
                            <*> timeEntryRes
                        | Some wo, Some ev -> Failure "Both Workorder and Event entry are set."
                        | None, None       -> Failure "Both Workorder and Event entry are missing."


 