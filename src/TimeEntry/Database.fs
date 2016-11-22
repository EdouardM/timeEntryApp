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

            
            type DBWorkOrder =
                {
                    WorkOrder : string
                    ItemCode  : string
                    Weight    : float
                    Unit      : string
                    Status    : string
                }

            let toWorkOrderDB (wo: WorkOrderEntry) =
                let (WorkOrder strWo) = wo.WorkOrder
                let (ItemCode strItem) = wo.ItemCode
                let unit, weight = 
                    match wo.Weight with
                        | Kg (Weight w) -> "KG", w
                        | Gr (Weight w) -> "GR", w
                let closed =
                    match wo.Status with
                        | Open   -> 0
                        | Closed -> 1

                { WorkOrder = strWo; ItemCode = strItem; Weight = weight; Unit = unit}

            let fromWorkOrderDB 
                workOrders
                itemCodes
                (wo: DBWorkOrder) =
                let workOrderRes = createWorkOrder workOrders wo.WorkOrder
                let itemCodeRes  = createItemCode itemCodes wo.ItemCode
                let weightWithUnitRes = createWeightWithUnit wo.Weight wo.Unit
                createWorkOrderEntry <!> workOrderRes <*> itemCodeRes <*> weightWithUnitRes 

            type DBEventInfo =
                {
                    Event       : string 
                    Machine     : string option
                    Cause       : string option
                    Solution    : string option
                    Comments    : string option
                }
            let toEventInfoDB (ev: EventEntry) = ()


            let fromEventInfoDB (wo: DBEventInfo) =
                //Return WorkOrder Entry
                ()

            //DataBase model (pure)
            type DBTimeRecord =
                {
                    Site        : Site
                    ShopFloor   : ShopFloor
                    WorkCenter  : WorkCenter
                    TimeType    : TimeType
                    DurationMn  : int
                    NbPeople    : NbPeople
                    Allocation  : string // String workOrder or Event
                    WorOrder    : DBWorkOrder option
                    Event       : DBEventInfo option
                }

            let allocationToString =
                function
                    | WorkOrderEntry wo -> "WorkOrder"
                    | EventEntry ev     -> "Event"


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
                            DurationMn    = int duration.Duration.TotalMinutes
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
                            DurationMn    = int duration.Duration.TotalMinutes
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