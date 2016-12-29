namespace TimeEntry

module DBService = 
    open Result
    open DomainTypes
    open DBCommands  

    let removeExistingData = 
        deleteTimeRecords 
        >=> deleteEventEntries
        >=> deleteUserAuth
        >=> deleteUser
        >=> deleteEvents
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
        
        let format = WithoutInfo "FOR"
        let div = ZeroPerson "DIV"
        let pan = WithInfo "PAN"
        let arr = WithInfo "ARR"
        
        insertSite(s1) |> ignore
        insertSite(s2) |> ignore

        insertShopfloor(sf1) |> ignore        
        insertShopfloor(sf2) |> ignore

        insertWorkCenter(wc1) |> ignore
        insertWorkCenter(wc2) |> ignore

        insertMachine(m1) |> ignore

        insertMachine(m2) |> ignore

        [format; div; pan; arr]
        |> List.map insertEvent
        |> ignore

        let wo1 = { WorkOrder = WorkOrder "12243"; ItemCode = ItemCode "099148"; WorkCenter = WorkCenter "F1"; TotalMachineTimeHr = TimeHr 0.f; TotalLabourTimeHr = TimeHr 0.f; Status =  Open }
        
        let ev1 = EventWithoutInfo (WithoutInfo "FOR")
        insertEventEntry ev1 |> ignore

        let ev2 = EventWithInfo (WithInfo "ARR", {Machine =Machine "ZX"; Cause="Arrêt imprévu";Solution="Brancher la prise";Comments="A retenir" })
        insertEventEntry ev2 |> ignore



    