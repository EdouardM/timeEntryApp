namespace TimeEntry

module DBService = 
    open Result
    open ConstrainedTypes
    open Constructors
    open DomainTypes
    open DBCommands  

    let removeExistingData = 
        TimeRecordAPI.deleteAll 
        >=> ActivityInfoAPI.deleteAll
        >=> UserAuthAPI.deleteAll
        >=> UserInfoAPI.deleteAll
        >=> ActivityAPI.deleteAll
        >=> WorkOrderInfoAPI.deleteAll
        >=> MachineAPI.deleteAll
        >=> WorkCenterAPI.deleteAll
        >=> ShopFloorAPI.deleteAll
        >=> SiteAPI.deleteAll
        
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
        
        SiteAPI.insert(s1) |> ignore
        SiteAPI.insert(s2) |> ignore

        ShopFloorAPI.insert(sf1) |> ignore        
        ShopFloorAPI.insert(sf2) |> ignore

        WorkCenterAPI.insert(wc1) |> ignore
        WorkCenterAPI.insert(wc2) |> ignore

        MachineAPI.insert(m1) |> ignore

        MachineAPI.insert(m2) |> ignore

        [formatF21; mformatF21; divF21; mdivF21; arrF21; marrF21]
        |> List.map ActivityAPI.insert
        |> ignore
        ActivityInfoAPI.insert actInfo1 |> ignore
        ActivityInfoAPI.insert actInfo2 |> ignore


    let getUserInfo login = 
        let sites = SiteAPI.getSiteCodes()
        let logins = UserInfoAPI.getUserLogins()

        UserInfoAPI.getUser login
        |> Result.bind (DBConversions.UserInfo.fromDB sites logins)

    let getSite site = 
        let sites = SiteAPI.getSiteCodes()
        validateSite sites site

    //let insertSite sites = (createSite sites) >=> SiteAPI.insertSite

    //let desactivateSite sites = (createSite sites) >=> SiteAPI.desactivateSite

    //let activateSite sites = (createSite sites) >=> SiteAPI.activateSite

    (* let updatePassword 
        logins 
        login 
        password = 
                let logRes = createLogin logins login
                let passwordRes = createPassword password
                UserInfoAPI.updatePassword <!> logRes <*> passwordRes
                |> flatten
    *)