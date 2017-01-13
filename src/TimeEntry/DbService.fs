namespace TimeEntry

module DBService = 
    open Result
    open DomainTypes
    open DBCommands  

    let removeExistingData = 
        deleteTimeRecords 
        >=> deleteActivityInfo
        >=> deleteUserAuth
        >=> deleteUser
        >=> deleteActivities
        >=> deleteWorkOrders
        >=> deleteMachines
        >=> deleteWorkCenters
        >=> deleteShopfloors
        >=> deleteSites

        
    let insertReferenceData () = 
        let s1 = Site "F21"
        let s2 = Site "F22"

        let sf1 = {ShopFloorInfo.ShopFloor = ShopFloor "F211A"; Site = Site "F21"}
        let sf2 = {ShopFloorInfo.ShopFloor = ShopFloor "F221A"; Site = Site "F22"}

        let wc1 = {WorkCenterInfo.WorkCenter = WorkCenter "F1"; ShopFloorInfo = sf1; StartHour = Hour 4u; EndHour = Hour 4u}
        let wc2 = {WorkCenterInfo.WorkCenter = WorkCenter "F2"; ShopFloorInfo = sf1; StartHour = Hour 4u; EndHour = Hour 4u}

        let m1: MachineInfo = {Machine = Machine "Rooslvo"; ShopFloorInfo = sf1}
        let m2: MachineInfo = {Machine = Machine "Scoel12"; ShopFloorInfo = sf2}
        
        (*
            INSERT Activity VALUES('FOR', 'F21', 'workcenter', 1, 'withoutinfo', 'machine', 1, 'MFOR', 1)
            INSERT Activity VALUES('MFOR','F21', 'workcenter', 1, 'withinfo', 'labour', 1, 'FOR', 1)
        *)

        let formatF21 = { 
                Site            = s1; 
                Code            = ActivityCode "FOR"; 
                RecordLevel     = WorkCenterLevel AllWorkCenters; 
                TimeType        = MachineTime; 
                ActivityLink    = Linked <| ActivityCode "MFOR"; 
                ExtraInfo       = ExtraInfo.WithoutInfo
                }
        let mformatF21 = {formatF21 with Code = ActivityCode "MFOR"; ActivityLink = Linked <| ActivityCode "FOR"}

        let divF21 = { 
                Site            = s1; 
                Code            = ActivityCode "DIV"; 
                RecordLevel     = WorkCenterLevel AllWorkCenters; 
                TimeType        = MachineTime; 
                ActivityLink    = Linked <| ActivityCode "MDIV"; 
                ExtraInfo       = ExtraInfo.WithoutInfo
                }

        let mdivF21 = {formatF21 with Code = ActivityCode "MDIV"; ActivityLink = Linked <| ActivityCode "DIV"}

        let arrF21 = { 
                Site            = s1; 
                Code            = ActivityCode "ARR"; 
                RecordLevel     = WorkCenterLevel AllWorkCenters; 
                TimeType        = MachineTime; 
                ActivityLink    = Linked <| ActivityCode "MARR"; 
                ExtraInfo       = ExtraInfo.WithInfo
                }

        let marrF21 = {arrF21 with Code = ActivityCode "MARR"; ActivityLink = Linked <| ActivityCode "ARR"}


        let actInfo2 = Detailed (ActivityCode "ARR", {Machine =Machine "ZX"; Cause="Arrêt imprévu";Solution="Brancher la prise";Comments="A retenir" })

        let wo1 = { WorkOrder = WorkOrder "12243"; ItemCode = ItemCode "099148"; WorkCenter = WorkCenter "F1"; TotalMachineTimeHr = TimeHr 0.f; TotalLabourTimeHr = TimeHr 0.f; Status =  Open }
        
        let actInfo1 = Normal (ActivityCode "FOR")
        
        insertSite(s1) |> ignore
        insertSite(s2) |> ignore

        insertShopfloor(sf1) |> ignore        
        insertShopfloor(sf2) |> ignore

        insertWorkCenter(wc1) |> ignore
        insertWorkCenter(wc2) |> ignore

        insertMachine(m1) |> ignore

        insertMachine(m2) |> ignore

        [formatF21; mformatF21; divF21; mdivF21; arrF21; marrF21]
        |> List.map insertActivity
        |> ignore
        insertActivityInfo actInfo1 |> ignore
        insertActivityInfo actInfo2 |> ignore



    