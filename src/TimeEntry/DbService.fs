namespace TimeEntry

module DBService = 
    open Result
    open ConstrainedTypes
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
        let s1 = Site (String3 "F21")
        let s2 = Site (String3 "F22")

        let sf1 = {ShopFloorInfo.ShopFloor = ShopFloor (String5 "F211A"); Site = s1}
        let sf2 = {ShopFloorInfo.ShopFloor = ShopFloor (String5 "F221A"); Site = s2}

        let wc1 = {WorkCenterInfo.WorkCenter = WorkCenter (String5 "F1"); ShopFloorInfo = sf1; StartHour = Hour 4u; EndHour = Hour 4u}
        let wc2 = {WorkCenterInfo.WorkCenter = WorkCenter (String5 "F2"); ShopFloorInfo = sf1; StartHour = Hour 4u; EndHour = Hour 4u}

        let m1: MachineInfo = {Machine = Machine (String10 "Rooslvo"); ShopFloorInfo = sf1}
        let m2: MachineInfo = {Machine = Machine (String10 "Scoel12"); ShopFloorInfo = sf2}
        
        let formatF21 = { 
                Site            = s1; 
                Code            = ActivityCode (String4 "FOR"); 
                RecordLevel     = WorkCenterLevel AllWorkCenters; 
                TimeType        = MachineTime; 
                ActivityLink    = Linked <| ActivityCode (String4 "MFOR"); 
                ExtraInfo       = ExtraInfo.WithoutInfo
                }
        let mformatF21 = {formatF21 with Code = ActivityCode (String4 "MFOR"); ActivityLink = Linked <| ActivityCode (String4 "FOR")}

        let divF21 = { 
                Site            = s1; 
                Code            = ActivityCode (String4 "DIV"); 
                RecordLevel     = WorkCenterLevel AllWorkCenters; 
                TimeType        = MachineTime; 
                ActivityLink    = Linked <| ActivityCode (String4 "MDIV"); 
                ExtraInfo       = ExtraInfo.WithoutInfo
                }

        let mdivF21 = {formatF21 with Code = ActivityCode (String4 "MDIV"); ActivityLink = Linked <| ActivityCode (String4 "DIV")}

        let arrF21 = { 
                Site            = s1; 
                Code            = ActivityCode (String4 "ARR"); 
                RecordLevel     = WorkCenterLevel AllWorkCenters; 
                TimeType        = MachineTime; 
                ActivityLink    = Linked <| ActivityCode (String4 "MARR"); 
                ExtraInfo       = ExtraInfo.WithInfo
                }

        let marrF21 = {arrF21 with Code = ActivityCode (String4 "MARR"); ActivityLink = Linked <| ActivityCode (String4 "ARR")}

        let extrainfo = {
                                Machine  = Machine (String10 "ZX"); 
                                Cause    = String50 "Arrêt imprévu";
                                Solution = String50 "Brancher la prise";
                                Comments = String200 "A retenir" 
                        }
        let actInfo2 = Detailed (ActivityCode (String4 "ARR"), extrainfo )

        let wo1 = { 
                WorkOrder = WorkOrder (String10 "12243");
                ItemCode = ItemCode (String6 "099148") ; 
                WorkCenter = WorkCenter (String5 "F1");
                TotalMachineTimeHr = TimeHr 0.f;
                TotalLabourTimeHr = TimeHr 0.f;
                Status =  Open }
        
        let actInfo1 = Normal (ActivityCode (String4 "FOR"))
        
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



    