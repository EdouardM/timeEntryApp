namespace TimeEntry
    open TimeEntry.Result
    open DomainTypes
    open Constructors

    module DataBase =

            type DBWorkCenterInfo =
                {
                    //WorkCenterId : int
                    Site         : string
                    WorkCenter   : string
                    ShopFloor    : string
                    StartHour    : int
                    EndHour      : int
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
                    ItemCode  : string
                    Weight    : float
                    Unit      : string
                    WorkOrderStatus    : string
                }

            let toWorkOrderDB (wo: WorkOrderEntry) =
                let (WorkOrder strWo) = wo.WorkOrder
                let (ItemCode strItem) = wo.ItemCode
                let unit, weight = 
                    match wo.Weight with
                        | Kg (Weight w) -> "KG", w
                        | Gr (Weight w) -> "GR", w
                let status =
                    match wo.Status with
                        | Open   -> "open"
                        | Closed -> "closed"

                { WorkOrder = strWo; 
                ItemCode = strItem; 
                Weight = weight; 
                Unit = unit; 
                WorkOrderStatus = status}

            let fromWorkOrderDB 
                workOrders
                itemCodes
                (wo: DBWorkOrderEntry) =
                let workOrderRes = createWorkOrder workOrders wo.WorkOrder
                let itemCodeRes  = createItemCode itemCodes wo.ItemCode
                let weightWithUnitRes = createWeightWithUnit wo.Weight wo.Unit
                let statusRes = createWorkOrderStatus wo.WorkOrderStatus
                createWorkOrderEntry 
                <!> workOrderRes 
                <*> itemCodeRes 
                <*> weightWithUnitRes
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
                    Site        : Site
                    ShopFloor   : ShopFloor
                    WorkCenter  : WorkCenter
                    TimeType    : TimeType
                    DurationHr  : float
                    NbPeople    : NbPeople
                    Allocation  : string // String workOrder or Event
                    WorOrder    : DBWorkOrderEntry option
                    Event       : DBEventEntry option
                    Status      : string
                }
            
            let allocationToString =
                function
                    | WorkOrderEntry wo -> "workorder"
                    | EventEntry ev     -> "event"


            ///Function to convert one Domain Record into records to insert into Database
            let toTimeRecordDB  (time : TimeRecord) = 
                let allocation = allocationToString time.Allocation

                match time.TimeEntry with
                    //List of one record: Machine time
                    | MachineOnly duration -> 
                        [ {   
                            Site        = time.Site 
                            ShopFloor   = time.ShopFloor 
                            WorkCenter  = time.WorkCenter 
                            TimeType    = MachineTime
                            DurationHr    = int duration.Duration.TotalMinutes
                            NbPeople    = NbPeople 0.
                            Allocation  = allocation
                             } ]
                    //List of two records: Machine & Labour time
                    | MachineAndLabour (duration, nb) -> 
                        [ { 
                            Site        = time.Site
                            ShopFloor   = time.ShopFloor 
                            WorkCenter  = time.WorkCenter
                            TimeType    = MachineTime 
                            DurationHr    = int duration.Duration.TotalMinutes
                            NbPeople    = NbPeople 0. } ;

                            { 
                            Site        = time.Site
                            ShopFloor   = time.ShopFloor 
                            WorkCenter  = time.WorkCenter
                            TimeType    = LabourTime 
                            DurationMn    = int duration.Duration.TotalMinutes
                            NbPeople    = nb }]
                    //List of one record: Labour time
                    | LabourOnly (duration, nb) -> 
                        [ {   
                            Site        = time.Site 
                            ShopFloor   = time.ShopFloor
                            WorkCenter  = time.WorkCenter
                            TimeType    = LabourTime
                            Duration    = duration 
                            NbPeople    = nb } ]
            

            let fromTimeRecordDB (time: DBTimeRecord) = ()