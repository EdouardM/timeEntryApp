namespace TimeEntry
    open DomainTypes
    module DataBase =

            type DBWorkOrder =
                {
                    WorkOrder : string
                    ItemCode  : string
                    Weight    : float
                }

            let toWorkOrderDB (wo: WorkOrderEntry) =
                let (WorkOrder strWo) = wo.WorkOrder
                let (ItemCode strItem) = wo.ItemCode
                let (Weight w) = wo.Weight
                { WorkOrder = strWo; ItemCode = strItem; Weight = w}

            let fromWorkOrderDB (wo: DBWorkOrder) = ()
                //Return WorkOrder Entry Result

            type DBEventInfo =
                {
                    Event       : string
                    Machine     : string
                    Cause       : string
                    Solution    : string
                    Comments    : string
                }

            let toEventInfoDB (ev: EventEntry) =
                ()


            let fromEventInfoDB (wo: DBEventInfo) =
                //Return WorkOrder Entry

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