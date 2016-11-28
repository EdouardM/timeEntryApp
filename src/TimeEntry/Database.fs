namespace TimeEntry
    open TimeEntry.Result
    open TimeEntry.DomainTypes
    open TimeEntry.Constructors

    module DataBase =

            type DBWorkCenterInfo =
                {
                    //WorkCenterId : int
                    Site         : string
                    WorkCenter   : string
                    ShopFloor    : string
                    StartHour    : uint32
                    EndHour      : uint32
                }
            
            type GetAllWorkCenters = unit -> DBWorkCenterInfo list

            let toDBWorkCenterInfo (wcInfo: WorkCenterInfo) =
                let (Site site)     = wcInfo.Site
                let (WorkCenter wc) = wcInfo.WorkCenter
                let (ShopFloor sf)  = wcInfo.ShopFloor
                let (Hour startH)    = wcInfo.StartHour
                let (Hour endH)      = wcInfo.EndHour
                { Site = site; WorkCenter = wc; ShopFloor = sf; StartHour = startH; EndHour = endH}
         
            let fromDBWorkCenterInfo 
                sites
                shopfloors
                workcenters
                (wcInfo: DBWorkCenterInfo) =
                    let siteRes = createSite sites wcInfo.Site
                    let shopfloorRes = createShopfloor shopfloors wcInfo.Site
                    let workcenterRes = createWorkCenter workcenters wcInfo.WorkCenter
                    let starthourRes = createHour wcInfo.StartHour
                    let endhourRes   = createHour wcInfo.EndHour
                    createWorkCenterInfo <!> siteRes
                    <*> shopfloorRes
                    <*> workcenterRes
                    <*> starthourRes
                    <*> endhourRes 
            
            type DBWorkOrderEntry =
                {
                    WorkOrder : string
                    WorkCenter : string
                    ItemCode  : string
                    TotalMachineTimeHr : float
                    TotalLabourTimeHr : float
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
                    WorkOrder = strWo; 
                    WorkCenter = strWc;
                    ItemCode = strItem; 
                    TotalMachineTimeHr = totalMachine;
                    TotalLabourTimeHr = totalLabour;
                    WorkOrderStatus = status
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
                            Machine = Some machine; 
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

            //DataBase model (pure)
            type DBTimeRecord =
                {
                    Site            : string
                    ShopFloor       : string
                    WorkCenter      : string
                    TimeType        : string
                    DurationHr      : float
                    NbPeople        : float
                    Allocation      : string
                    WorkOrderEntry   : DBWorkOrderEntry option
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
            let toTimeRecordDB  (time : TimeRecord) = 
                let (Site site) = time.Site
                let (ShopFloor shopfloor) = time.ShopFloor
                let (WorkCenter workcenter) = time.WorkCenter
                let status = recordStatusToString time.Status

                match time.TimeEntry with
                    //List of one record: Machine time
                    | MachineOnly duration ->
                        let machineRecord = {   
                            Site            = site
                            ShopFloor       = shopfloor 
                            WorkCenter      = workcenter 
                            TimeType        = "machine"
                            DurationHr      = duration.ToHr
                            NbPeople        = 0.
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
                            Site        = site
                            ShopFloor   = shopfloor 
                            WorkCenter  = workcenter
                            TimeType    = "machine" 
                            DurationHr  = duration.ToHr
                            NbPeople    = 0.
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
                            Site        = site
                            ShopFloor   = shopfloor 
                            WorkCenter  = workcenter
                            TimeType    = "labour" 
                            DurationHr  = duration.ToHr
                            NbPeople    = nb
                            Allocation  = allocationToString time.Allocation
                            WorkOrderEntry = None
                            EventEntry  = None
                            Status      =  status }
                        let labourRecord' = updateAllocation time.Allocation labourRecord
                        [labourRecord']
        
            let fromTimeRecordDB (time: DBTimeRecord) = ()