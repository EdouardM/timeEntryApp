namespace TimeEntry

module Application =
    open System
    open TimeEntry.Result
    open TimeEntry.Option
    open TimeEntry.DomainTypes
    open TimeEntry.Constructors

    (* DEFINITION OF CAPABILITIES *)
    module Capabilities = 
        open TimeEntry.DBService
        open TimeEntry.Authorization
        let updatePasswordForSameLogin =
            fun login userinfo ->
                onlyForSameLogin login userinfo DBService.updatePassword

        let displayAuthSite userinfo = hasAuthSites userinfo DBService.getAuthSiteCodes 
        let selectAuthSite site userinfo = onlyForAuthSites site userinfo succeed
        let desactivateForAdmin (userinfo:UserInfo) = 
            onlyForAdmin userinfo DBService.desactivateSite

        let createNewSiteForAdmin userinfo = onlyForAdmin userinfo DBService.newSite
        let displayEntryMethodNotForViewer userinfo = notForViewer userinfo (fun () -> ["(P)roduction"; "(I)ndividual"])
        let selectEntryModeNotForViewer userinfo = notForViewer userinfo succeed
        let displayEntryLevelNotForViewer userinfo = notForViewer userinfo (fun () -> ["(S)hopfloor"; "(W)orkcenter"])
        let selectEntryLevelNotForViewer userinfo = notForViewer userinfo succeed
        let displayWorkCenterNotForViewer userinfo = 
            DBService.getActiveWorkCentersByShopfloor
            |> notForViewer userinfo
        let displayEntryModeNotForViewer userinfo = notForViewer userinfo (fun () -> ["(M)achineOnly"; "(L)abourOnly"])
        let displayAttributionNotForViewer userinfo = notForViewer userinfo (fun () -> ["(W)orkorder"; "(A)ctivity"])
        let displayActivityCodesNotForViewer userinfo = 
            (fun entryLevel entrymode shopfloor workcenter -> 
                let timetype = EntryMode.toTimeType entrymode
                match entryLevel, shopfloor, workcenter with
                    | EntryLevel.ShopFloor, shopfloor, _ -> 
                        DBService.getActivityCodeByTimeTypeAndShopFloor timetype shopfloor
                        |> Some
                    | EntryLevel.WorkCenter,_   , Some wc -> 
                        DBService.getActivityCodeByTimeTypeAndWorkCenter timetype wc
                        |> Some
                    | EntryLevel.WorkCenter, _  , None -> None
            )
            |> notForViewer userinfo

        let displayWorkOrdersNotForViewer userinfo = 
            (DBService.getWorkOrderByWorkCenter )
            |> notForViewer userinfo


    (* SERVICES IMPLEMENTATION *)
    module Services =
        open TimeEntry.DBService
        open Capabilities
        let userLogin: UserLogin =
            fun usercredential ->  
                usercredential.Login
                |> DBService.getUserInfo 
                |> Result.bind
                    (fun userinfo -> 
                        if userinfo.Password = usercredential.Password then
                            Success { LoggedInData.UserInfo = userinfo }
                        else
                            Failure "Wrong Password")
        
        let updatePassword: UpdatePassword = 
            fun login password loggedInData -> 
                let updateCap = updatePasswordForSameLogin login loggedInData.UserInfo
                match updateCap with
                    | Some cap ->
                        cap login password
                        |> Result.map(fun userinfo -> { UserInfo = userinfo } )
                    | None -> 
                        Failure "You can only update the password of the account you are logged with."
                
        let displaySite: DisplaySites = 
            fun loggedInData -> 
                let displaySiteCap = displayAuthSite loggedInData.UserInfo
                match displaySiteCap with
                    | Some cap -> 
                        cap loggedInData.UserInfo.SiteAccess
                        |> Success
                    | None -> Failure "You can not access to any site."

        let selectSite: SelectSite =
            fun site loggedInData -> 
                let selectSiteCap = selectAuthSite site loggedInData.UserInfo
                match selectSiteCap with
                    | Some cap ->
                        cap site
                        |> Result.map(fun site -> { UserInfo = loggedInData.UserInfo; Site = site})
                    | None -> 
                        Failure "You can only select one site you are authorized to access."
        
        let unselectSite: UnSelectSite = 
            fun siteSelectedData ->
                let userinfo = siteSelectedData.UserInfo
                { UserInfo = userinfo}


        let desactivateSite: DesactivateSite =
            fun siteSelectedData ->
                let site =  siteSelectedData.Site
                let desactivateCap = desactivateForAdmin siteSelectedData.UserInfo
                match desactivateCap with
                    | Some cap -> 
                        cap site
                    | None -> Failure "You can only desactivate one site you are authorized to access."

        
        let createSite: CreateSite =
            fun input loggedInData ->
                let createSiteCap = createNewSiteForAdmin loggedInData.UserInfo
                match createSiteCap with
                    | Some cap -> 
                        cap input
                        |> Result.map(fun site -> {UserInfo = loggedInData.UserInfo; Site = site})
                    | None -> Failure "Only the Administrator can create a new site."

        let displayEntryMethod: DisplayEntryMethod = 
            fun siteSelectedData -> 
                let displayEntryModecap = displayEntryMethodNotForViewer siteSelectedData.UserInfo
                match displayEntryModecap with
                    | Some cap -> 
                        cap() |> Success
                    | None -> 
                        Failure "You are not authorized to record time."

        let selectEntryMethod: SelectEntryMethod = 
            fun method siteSelectedData -> 
                let selectEntryModecap = selectEntryModeNotForViewer siteSelectedData.UserInfo
                match selectEntryModecap with
                    | Some cap -> 
                        cap method
                        |> Result.map(fun mode -> 
                            {   
                                UserInfo = siteSelectedData.UserInfo; 
                                Site = siteSelectedData.Site; 
                                EntryMethod = method 
                            })
                    | None -> 
                        Failure "You are not authorized to record time."
        let displayEntryLevel: DisplayEntryLevel = 
            fun entryMethodData ->
                let displayEntryLevelCap = displayEntryLevelNotForViewer entryMethodData.UserInfo
                match displayEntryLevelCap with
                    | Some cap -> 
                        cap ()
                        |> Success
                    | None      -> Failure "You are not authorized to record time."

        let selectEntryLevel: SelectEntryLevel = 
            fun level entryMethodData -> 
                let selectEntryLevelCap = selectEntryLevelNotForViewer entryMethodData.UserInfo
                match selectEntryLevelCap with
                    | Some cap -> 
                        cap level
                        |> Result.map(fun level -> 
                            {   
                                UserInfo    = entryMethodData.UserInfo 
                                Site        = entryMethodData.Site;
                                EntryMethod = entryMethodData.EntryMethod
                                EntryLevel  = level
                            })
                    | None -> 
                        Failure "You are not authorized to record time."


        let displayShopFloors: DisplayShopFloors = 
            fun entryMethodData -> 
                let msg = sprintf "No active Shopfloor defined for the site: %s" <| entryMethodData.Site.ToString()
                getActiveShopFloorCodesBySite entryMethodData.Site
                |> failIfEmpty msg
        let selectShopFloor: SelectShopFloor = 
            fun shopfloor entryLevelData -> 
                { 
                    ShopFloorSelectedData.Site = entryLevelData.Site 
                    UserInfo = entryLevelData.UserInfo 
                    ShopFloor = shopfloor 
                    EntryMethod = entryLevelData.EntryMethod
                    EntryLevel = entryLevelData.EntryLevel
                }
                |> Success
                
        let displayWorkCenters: DisplayWorkCenters = 
            fun shopFloorData -> 
                let displayWorkCenterCap = displayWorkCenterNotForViewer shopFloorData.UserInfo
                match displayWorkCenterCap with
                    | Some cap -> 
                        let msg = 
                            sprintf "No active WorkCenter defined for the shopfloor: %s" 
                            <| shopFloorData.ShopFloor.ToString()
                        
                        cap shopFloorData.ShopFloor
                        |> Result.failIfEmpty msg 

                    | None -> Failure "You are not authorized to record time."
        let selectWorkCenter: SelectWorkCenter = 
            fun workcenter shopFloorData -> 
                { 
                    WorkCenterSelectedData.Site = shopFloorData.Site 
                    UserInfo = shopFloorData.UserInfo 
                    ShopFloor = shopFloorData.ShopFloor 
                    EntryMethod = shopFloorData.EntryMethod
                    EntryLevel = shopFloorData.EntryLevel
                    WorkCenter = Some workcenter
                }
                |> Success
                
        let displayEntryModes: DisplayEntryModes = 
            fun workcenterData -> 
                let displayEntryModeCap = displayEntryModeNotForViewer workcenterData.UserInfo
                match displayEntryModeCap with
                    | Some cap -> cap () |> Success
                    | None -> Failure "You are not authorized to record time."

        let selectEntryMode: SelectEntryMode = 
            fun entrymode workcenterData -> 
                { 
                    EntryModeSelectedData.Site = workcenterData.Site 
                    UserInfo            = workcenterData.UserInfo 
                    ShopFloor           = workcenterData.ShopFloor 
                    EntryMethod         = workcenterData.EntryMethod
                    EntryLevel          = workcenterData.EntryLevel
                    WorkCenter          = workcenterData.WorkCenter
                    EntryMode           = entrymode
                }
                |> Success   
        let displayAttributionTypes : DisplayAttributionTypes = 
            fun entrymodedata -> 
                let displayAttributionCap = displayAttributionNotForViewer entrymodedata.UserInfo
                match displayAttributionCap with
                    | Some cap -> cap () |> Success
                    | None -> Failure "You are not authorized to record time."

        let selectAttributionType: SelectAttributionType = 
            fun attr entrymodedata records  -> 
                { 
                    AttributionTypeSelectedData.Context = entrymodedata
                    TimeRecords         = records 
                    AttributionType     = attr
                }
                |> Success   

        let displayActivityCodes : DisplayActivityCodes = 
            fun data -> 
                let displayActivityCodeCap = displayActivityCodesNotForViewer data.Context.UserInfo
                match displayActivityCodeCap with
                    | Some cap ->
                        let ctxData = data.Context 
                        cap ctxData.EntryLevel ctxData.EntryMode ctxData.ShopFloor ctxData.WorkCenter
                        |> Result.failIfMissing "No activity code available." 

                    | None -> Failure "You are not authorized to record time."
        let displayWorkOrders : DisplayWorkOrders = 
            fun data -> 
                let ctxData = data.Context
                let displayWorkOrderCap = displayWorkOrdersNotForViewer ctxData.UserInfo
                match displayWorkOrderCap with
                    | Some cap -> 
                        match ctxData.WorkCenter with
                            | Some wc -> 
                                let msg = sprintf "No workorder available for workcenter: %s." <| wc.ToString()
                                cap wc
                                |> Result.failIfEmpty msg
                            | None    -> Failure "WorkCenter is not selected."

                    | None -> Failure "You are not authorized to record time."
        let selectAttribution: SelectAttribution = 
            fun attr data -> 
                { 
                    AttributionSelectedData.Context = data.Context
                    TimeRecords         = data.TimeRecords 
                    AttributionType     = data.AttributionType
                    Attribution         = attr
                }
                |> Success   

        let enterDuration: EnterDuration = 
            fun duration data -> 
                { 
                    DurationEnteredData.Context = data.Context 
                    TimeRecords         = data.TimeRecords
                    AttributionType     = data.AttributionType
                    Attribution         = data.Attribution
                    Duration            = duration
                }
                |> Success   
                
        let enterNbPeople: EnterNbPeople =
            fun nb data -> 
                { 
                    NbPeopleEnteredData.Context = data.Context 
                    TimeRecords         = data.TimeRecords
                    AttributionType     = data.AttributionType
                    Attribution         = data.Attribution
                    Duration            = data.Duration
                    NbPeople            = nb
                }
                |> Success   
            

    (* IMPLEMENT PROGRAM *)
    module Program = 
        open Services
        open Capabilities
        open DTO

        //Application returns here one key and the capability to communicate to the client
        //URL in case of web app
        let getCapabilities state =
            match state with
                | LoggedOut                 -> Map.ofList ["L", Cap.Login; "E", Cap.Exit]
                | LoggedIn  _               -> Map.ofList ["U", Cap.UpdatePassword; "S", Cap.SelectSite; "C", Cap.CreateSite; "L", Cap.Logout]
                | PasswordUpdated _         -> Map.ofList ["U", Cap.UpdatePassword; "S", Cap.SelectSite; "C", Cap.CreateSite; "L", Cap.Logout] 
                | SiteSelected  _           -> Map.ofList ["U", Cap.UnselectSite; "R", Cap.SelectEntryMethod; "L", Cap.Logout] 
                | EntryMethodSelected data  -> 
                        match data.EntryMethod with
                            | Individual        -> Map.ofList ["U", Cap.UnselectEntryMethod ; "S", Cap.SelectEntryLevel; "L", Cap.Logout] 
                            
                            //Skip EntryLevel selection, directly ShopFloor Selection
                            | ProductionLine    -> Map.ofList ["U", Cap.UnselectEntryMethod ; "S", Cap.SelectShopFloor; "L", Cap.Logout] 
                
                //Only in EntryMethod individual
                | EntryLevelSelected _      -> Map.ofList ["U", Cap.UnselectEntryLevel ; "S", Cap.SelectShopFloor; "L", Cap.Logout] 
                | ShopFloorSelected data    -> 
                        match data.EntryLevel with
                            | EntryLevel.WorkCenter -> 
                                    Map.ofList ["U", Cap.UnselectShopFloor; "S", Cap.SelectWorkCenter; "L", Cap.Logout] 

                            //Skip WorkCenter selection! 
                            | EntryLevel.ShopFloor  -> Map.ofList ["U", Cap.UnselectShopFloor; "S", Cap.SelectEntryMode ; "L", Cap.Logout] 

                | WorkCenterSelected data      -> 
                    match data.EntryMethod with
                        | Individual        -> Map.ofList ["U", Cap.UnselectWorkCenter; "S", Cap.SelectEntryMode ; "L", Cap.Logout]
                        //Skip EntryMode selection
                        | ProductionLine    -> Map.ofList ["U", Cap.UnselectWorkCenter; "S", Cap.SelectAttributionType ; "L", Cap.Logout]

                | EntryModeSelected _       -> Map.ofList ["U", Cap.UnselectEntryMode ; "S", Cap.SelectAttributionType ; "L", Cap.Logout]
                | AttributionTypeSelected data  -> 
                
                        match data.AttributionType with
                            | AttributionType.Activity  -> Map.ofList ["U", Cap.UnselectAttrbutionType; "S", Cap.SelectActivity; "L", Cap.Logout]
                            | AttributionType.WorkOrder -> Map.ofList ["U", Cap.UnselectAttrbutionType; "S", Cap.SelectWorkOrder; "L", Cap.Logout]
                
                | AttributionSelected _     ->  Map.ofList ["U", Cap.UnselectAttribution ; "E", Cap.EnterDuration  ; "L", Cap.Logout]
                
                | DurationEntered _         -> Map.ofList ["C", Cap.CancelDuration ; "E", Cap.EnterNbPeople  ; "L", Cap.Logout]

                | NbPeopleEntered data         -> 
                        match data.Context.EntryMethod with
                            //Stop recoring time after first recording
                            | Individual        -> Map.ofList [ "C", Cap.CancelNbPeople; "S", Cap.SaveRecord ; "L", Cap.Logout]

                            //Continue time recording with one record in memory
                            | ProductionLine    -> Map.ofList [ "C", Cap.CancelNbPeople; "A", Cap.AddRecord ; "L", Cap.Logout]
                

                | RecordAdded data -> Map.ofList [ "S", Cap.SelectAttributionType ; "L", Cap.Logout]
                
                | RecordPersisted  -> Map.ofList [ "S", Cap.SelectSite ; "L", Cap.Logout]

                | SiteCreated _             -> Map.ofList ["U", Cap.UnselectSite; "L", Cap.Logout] 
                | Exit                      -> Map.ofList []  
        

        let loginController (userLogin: UserLogin) state usercredential =
            match state with
                | LoggedOut ->
                    usercredential
                    |> DBService.validateUsercredential
                    |> Result.bind(userLogin)
                    |> Result.map(LoggedIn)
                | _ -> Failure "Already logged in."

        let updatePasswordController  (updatePassword: UpdatePassword) state login password = 
            result {
                let! login = DBService.validateLogin login
                let! password = Password.create password  
                let! loggedInData       = LoggedIn.getData state
                let! updatePasswordData = updatePassword login password loggedInData
                return (PasswordUpdated updatePasswordData)
            }

        let logoutController () = Success LoggedOut
        let exitController () = Success Exit
        let displaySitesController (displaySite: DisplaySites) state =
            result {
                let! loggedInData       = LoggedIn.getData state
                return! displaySite loggedInData
            }

        let selectSiteController (selectSite: SelectSite) state input =
            result {
                let! site           = DBService.validateSite input
                let! loggedInData   = LoggedIn.getData state
                let! selectSiteData = selectSite site loggedInData
                
                return (SiteSelected selectSiteData )
            }
        
        let unselectSiteController state =
            SiteSelected.getData state
            |> Result.map(fun data -> {LoggedInData.UserInfo = data.UserInfo})
            |> Result.map(LoggedIn)
        
        let displayEntryMethodController (displayEntryMethod: DisplayEntryMethod) state = 
            result {
                let! siteselectedData = SiteSelected.getData state
                return! displayEntryMethod siteselectedData
            }

        let selectEntryMethodController (selectEntryMethod: SelectEntryMethod) state input = 
            result {
                let! method             = EntryMethod.validate input
                let! siteselectedData   = SiteSelected.getData state
                let! timemethodData     = selectEntryMethod  method siteselectedData 
                return (EntryMethodSelected timemethodData)
            }
        let unselectEntryMethodController state = 
            EntryMethodSelected.getData state
            |> Result.map(fun data -> {SiteSelectedData.UserInfo = data.UserInfo; Site = data.Site })
            |> Result.map(SiteSelected)
        let displayEntryLevelController (displayEntryLevel: DisplayEntryLevel) state = 
            result {
                let! entryMethodData = EntryMethodSelected.getData state
                return! displayEntryLevel entryMethodData
            }
        let selectEntryLevelController (selectEntryLevel: SelectEntryLevel) state input = 
            result {
                let! level              = EntryLevel.validate input
                let! entryMethodData    = EntryMethodSelected.getData state
                let! entryLevelData     = selectEntryLevel  level entryMethodData 
                return (EntryLevelSelected entryLevelData)
            }
        
        let unselectEntryLevelController state = 
            EntryLevelSelected.getData state
            |> Result.map(fun data -> 
                {
                    EntryMethodSelectedData.UserInfo = data.UserInfo
                    Site = data.Site
                    EntryMethod = data.EntryMethod
                })
            |> Result.map(EntryMethodSelected)
        let displayShopfloorsController (displayShopfloors: DisplayShopFloors) state = 
            result {
                let! entryLevelData = EntryLevelSelected.getOrCreateData state
                return! displayShopfloors entryLevelData
            }

        let selectShopfloorController (selectShopFloor: SelectShopFloor) state input =  
                result {
                        let! entryleveldata   = EntryLevelSelected.getOrCreateData state
                        let! shopfloor       = DBService.validateShopFloor entryleveldata.Site input
                        let! shopfloordata = selectShopFloor shopfloor entryleveldata
                        return (ShopFloorSelected shopfloordata ) 
                }
        let unselectShopFloorController state =
            ShopFloorSelected.getData state
            |> Result.map(fun data -> 
                {
                    EntryLevelSelectedData.UserInfo = data.UserInfo
                    Site = data.Site
                    EntryMethod = data.EntryMethod
                    EntryLevel = data.EntryLevel
                })
            |> Result.map(EntryLevelSelected)

        let displayWorkCentersController (displayWorkcenters: DisplayWorkCenters) state = 
            result {
                let! shopfloorData = ShopFloorSelected.getData state
                return! displayWorkcenters shopfloorData
            }
        let selectWorkCenterController (selectWorkCenter: SelectWorkCenter) state input = 
            result {
                        let! shopfloordata  = ShopFloorSelected.getData state
                        let! workcenter     = DBService.validateWorkCenter shopfloordata.ShopFloor input
                        let! workcenterdata = selectWorkCenter workcenter shopfloordata
                        return (WorkCenterSelected workcenterdata)
            }

        let unselectWorkCenterController state =
            WorkCenterSelected.getData state
            |> Result.map(fun data ->
                {
                    ShopFloorSelectedData.Site        = data.Site
                    UserInfo    = data.UserInfo 
                    EntryMethod = data.EntryMethod
                    EntryLevel  = data.EntryLevel
                    ShopFloor   = data.ShopFloor

                }) 
            |> Result.map(ShopFloorSelected)
        
        let displayEntryModesController (displayEntryModes: DisplayEntryModes) state =
            result {
                let! workcenterdata = WorkCenterSelected.getOrCreateData state
                return! displayEntryModes workcenterdata
            }
        let selectEntryModeController (selectEntryMode: SelectEntryMode) state input = 
            result {
                    let! workcenterdata = WorkCenterSelected.getOrCreateData state
                    let! entryMode      = EntryMode.validate input
                    let! entrymodedata  = selectEntryMode entryMode workcenterdata
                    return (EntryModeSelected entrymodedata)
            }
        let unselectEntryModeController state =
            EntryModeSelected.getData state
            |> Result.map(fun data ->
                {
                    WorkCenterSelectedData.Site        = data.Site
                    UserInfo    = data.UserInfo 
                    EntryMethod = data.EntryMethod
                    EntryLevel  = data.EntryLevel
                    ShopFloor   = data.ShopFloor
                    WorkCenter  = data.WorkCenter
                }) 
            |> Result.map(WorkCenterSelected)

        let displayAttributionTypesController (displayAttributionTypes : DisplayAttributionTypes) state =
            result {
                match state with
                    | RecordAdded data -> 
                        let! data  = RecordAdded.getData state
                        let entrymodedata = data.Context
                        return! displayAttributionTypes entrymodedata
                         
                    | state -> 
                        let! entrymodedata = EntryModeSelected.getOrCreateData state
                        return! displayAttributionTypes entrymodedata 
            }

        let selectAttributionTypeController (selectAttributionType: SelectAttributionType) state input = 
            result {
                    match state with
                        | RecordAdded data -> 
                            //extract context data = entrymode data and time records so far 
                            let! data           = RecordAdded.getData state
                            let entrymodedata, timerecords = data.Context, data.TimeRecords
                            let! attrType       = AttributionType.validate input
                            let! attrdata       = selectAttributionType attrType entrymodedata timerecords
                            return (AttributionTypeSelected attrdata)

                        | state -> 
                            let! entrymodedata  = EntryModeSelected.getOrCreateData state
                            let! attrType       = AttributionType.validate input
                            //No time record yet
                            let! attrdata       = selectAttributionType attrType entrymodedata []
                            return (AttributionTypeSelected attrdata)
            }

        let unselectAttributionTypeController state =
            AttributionTypeSelected.getData state
            |> Result.map(fun data -> data.Context) 
            |> Result.map(EntryModeSelected)

        let displayActivityCodesController (displayActivityCodes : DisplayActivityCodes) state =
            result {
                let! data = AttributionTypeSelected.getData state
                return! displayActivityCodes data 
            }

        let displayWorkOrdersController (displayWorkOrders : DisplayWorkOrders) state =
            result {
                let! data = AttributionTypeSelected.getData state
                return! displayWorkOrders data 
            }

        let selectActivityCodeController (selectAttribution: SelectAttribution) state input = 
            result {
                    let! data           = AttributionTypeSelected.getData state

                    let timetype        = EntryMode.toTimeType data.Context.EntryMode

                    let! attribution   = 
                        DBService.validateActivityCode data.Context.EntryLevel timetype data.Context.ShopFloor data.Context.WorkCenter input
                        |> Result.map(Attribution.Activity)

                    let! attrdata       = selectAttribution attribution data
                    return (AttributionSelected attrdata)
            }

        let selectWorkOrderController (selectAttribution: SelectAttribution) state input = 
            result {
                    let! data = AttributionTypeSelected.getData state
                                        
                    let! attribution      = 
                            data.Context.WorkCenter
                            |> Option.map( fun wc -> DBService.validateWorkOrder wc input)
                            |> Result.failIfMissing "Workcenter is not selected."
                            |> flatten
                            |> Result.map (Attribution.WorkOrder)

                    let! attrdata = selectAttribution attribution data
                    return (AttributionSelected attrdata)
            }

        let unselectAttributionController state = 
            AttributionSelected.getData state
            |> Result.map(fun data ->
                {
                    AttributionTypeSelectedData.Context = data.Context
                    TimeRecords     = data.TimeRecords
                    AttributionType = data.AttributionType

                }) 
            |> Result.map(AttributionTypeSelected)

        let enterDurationController (enterDuration: EnterDuration) state input =
            result {
                    let! data = AttributionSelected.getData state

                    let! duration = Duration.fromDTO input

                    let! durData = enterDuration duration data
                    return (DurationEntered durData)
            }

        let cancelDurationController state = 
            DurationEntered.getData state
            |> Result.map(fun data ->
                {
                    AttributionSelectedData.Context = data.Context
                    TimeRecords = data.TimeRecords
                    AttributionType = data.AttributionType
                    Attribution = data.Attribution
                }) 
            |> Result.map(AttributionSelected)

        let enterNbPeopleController (enterNbPeople: EnterNbPeople) state input =
            result {
                    let! data = DurationEntered.getData state

                    let! nbpeople = NbPeople.fromDTO input

                    let! nbpdata = enterNbPeople nbpeople data
                    return (NbPeopleEntered nbpdata)
            }

        let cancelNbPeopleController state = 
            NbPeopleEntered.getData state
            |> Result.map(fun data ->
                {
                    DurationEnteredData.Context = data.Context
                    TimeRecords = data.TimeRecords
                    AttributionType = data.AttributionType
                    Attribution = data.Attribution
                    Duration    = data.Duration
                }) 
            |> Result.map(DurationEntered)

        let addRecordController state = 
            result {
                let! data = NbPeopleEntered.getData state
                let ctx = data.Context
                let timetype = EntryMode.toTimeType ctx.EntryMode
                let timeRecord = TimeRecord.create ctx.Site ctx.ShopFloor ctx.WorkCenter data.Attribution timetype data.Duration data.NbPeople Entered
                let timeRecords = timeRecord::data.TimeRecords
                let recorddata = RecordAddedData.create data.Context timeRecords
                return (RecordAdded recorddata) 
            }
            


        let siteCreationController state input (createSite: CreateSite) =            
            result {
                let! loggedInData  = LoggedIn.getData state
                let! createSiteData = createSite input loggedInData
                
                return (SiteCreated createSiteData)
            }
