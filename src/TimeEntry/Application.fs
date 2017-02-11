namespace TimeEntry

module Application =
    open System
    open TimeEntry.Result
    open TimeEntry.Option
    open TimeEntry.DomainTypes

    (* DEFINITION OF CAPABILITIES *)
    module Capabilities = 
        open TimeEntry.DBService
        open TimeEntry.Authorization
        let updatePasswordForSameLogin =
            fun login userinfo ->
                onlyForSameLogin login userinfo DBService.updatePassword

        let displayAuthSite userinfo = hasAuthSites userinfo DBService.getAuthSiteCodes 
        let selectAuthSite site userinfo = onlyForAuthSites site userinfo retn
        let desactivateForAdmin (userinfo:UserInfo) = 
            onlyForAdmin userinfo DBService.desactivateSite

        let createNewSiteForAdmin userinfo = onlyForAdmin userinfo DBService.newSite
        let displayEntryModeNotForViewer userinfo = notForViewer userinfo (fun () -> ["(P)roduction"; "(I)ndividual"])
        let selectEntryModeNotForViewer userinfo = notForViewer userinfo retn
        let displayEntryLevelNotForViewer userinfo = 
            (fun entrymethod -> 
                if entrymethod = Individual then 
                    Some ["(S)hopfloor"; "(W)orkcenter"]
                else None )
            |> notForViewer userinfo
        let selectEntryLevelNotForViewer userinfo = notForViewer userinfo retn
        let displayWorkCenterNotForViewer userinfo = 
            (fun entryLevel shopfloor -> 
                if entryLevel = EntryLevel.WorkCenter then 
                    DBService.getActiveWorkCentersByShopfloor shopfloor
                    |> Some
                else None )
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
                let displayEntryModecap = displayEntryModeNotForViewer siteSelectedData.UserInfo
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
                        cap entryMethodData.EntryMethod
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
                        maybe {
                            let! wcList = cap shopFloorData.EntryLevel shopFloorData.ShopFloor
                            let msg = sprintf "No active WorkCenter defined for the shopfloor: %s" <| shopFloorData.ShopFloor.ToString()
                            return failIfEmpty msg wcList
                        }
                        |> Result.switchResOpt

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
                


    (* IMPLEMENT PROGRAM *)
    module Program = 
        open Services
        open Capabilities
        open Constructors

        //Application returns here one key and the capability to communicate to the client
        //URL in case of web app
        let getCapabilities state =
            match state with
                | LoggedOut                 -> Map.ofList ["L", Cap.Login; "E", Cap.Exit]
                | LoggedIn  _               -> Map.ofList ["U", Cap.UpdatePassword; "S", Cap.SelectSite; "C", Cap.CreateSite; "L", Cap.Logout]
                | PasswordUpdated _         -> Map.ofList ["U", Cap.UpdatePassword; "S", Cap.SelectSite; "C", Cap.CreateSite; "L", Cap.Logout] 
                | SiteSelected  _           -> Map.ofList ["U", Cap.UnselectSite; "R", Cap.SelectEntryMethod; "L", Cap.Logout] 
                | EntryMethodSelected _     -> Map.ofList ["U", Cap.UnselectEntryMethod ; "S", Cap.SelectEntryLevel; "L", Cap.Logout] 
                | EntryLevelSelected _      -> Map.ofList ["U", Cap.UnselectEntryLevel ; "S", Cap.SelectShopFloor; "L", Cap.Logout] 
                | ShopFloorSelected _       -> Map.ofList ["U", Cap.UnselectShopFloor; "S", Cap.SelectWorkCenter; "L", Cap.Logout] 
                | WorkCenterSelected _      -> Map.ofList ["U", Cap.UnselectWorkCenter; "S", Cap.SelectAttribution; "L", Cap.Logout]
                | SiteCreated _             -> Map.ofList ["U", Cap.UnselectSite; "L", Cap.Logout] 
                | Exit                      -> Map.ofList []  
                
        let getLoggedInData =
            function
                | LoggedIn data -> Success data
                | _             -> Failure "Not in LoggedIn state, cannot access to inner data."
        let getSelecteSiteData = 
            function
                | SiteSelected data -> Success data
                | _                 -> Failure "Not in Selected Site state, cannot access to inner data."
        
        let getEntryMethodSelectedData = 
            function
                | EntryMethodSelected data  -> Success data
                | _                         -> Failure "Not in Entry Method selected state, cannot access to inner data."

        let getEntryLevelSelectedData = 
            function 
                | EntryLevelSelected data   -> Success data
                | _                         -> Failure "Not in Entry Level selected state, cannot access to inner data."
        let getSelectShopFloorData = 
            function
                | ShopFloorSelected data -> Success data
                | _                      -> Failure "Not in Shopfloor Selected state, cannot access to inner data."

        let getWorkCenterSelectedData =
            function
                | WorkCenterSelected data -> Success data
                | _                       -> Failure "Not in Workcenter Selected state, cannot access to inner data."
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
                let! password = Constructors.Password.create password  
                let! loggedInData       = getLoggedInData state
                let! updatePasswordData = updatePassword login password loggedInData
                return (PasswordUpdated updatePasswordData)
            }

        let logoutController () = Success LoggedOut
        let exitController () = Success Exit

        let displaySitesController (displaySite: DisplaySites) state =
            result {
                let! loggedInData       = getLoggedInData state
                return! displaySite loggedInData
            }

        let selectSiteController (selectSite: SelectSite) state input =
            result {
                let! site           = DBService.validateSite input
                let! loggedInData   = getLoggedInData state
                let! selectSiteData = selectSite site loggedInData
                
                return (SiteSelected selectSiteData )
            }
        
        let unselectSiteController state =
            getSelecteSiteData state
            |> Result.map(fun data -> {LoggedInData.UserInfo = data.UserInfo})
            |> Result.map(LoggedIn)
        
        let displayEntryMethodController (displayEntryMethod: DisplayEntryMethod) state = 
            result {
                let! siteselectedData = getSelecteSiteData state
                return! displayEntryMethod siteselectedData
            }

        let selectEntryMethodController (selectEntryMethod: SelectEntryMethod) state input = 
            result {
                let! method             = EntryMethod.validate input
                let! siteselectedData   = getSelecteSiteData state
                let! timemethodData     = selectEntryMethod  method siteselectedData 
                return (EntryMethodSelected timemethodData)
            }
        let unselectEntryMethodController state = 
            getEntryMethodSelectedData state
            |> Result.map(fun data -> {SiteSelectedData.UserInfo = data.UserInfo; Site = data.Site })
            |> Result.map(SiteSelected)
        let displayEntryLevelController (displayEntryLevel: DisplayEntryLevel) state = 
            result {
                let! entryMethodData = getEntryMethodSelectedData state
                return! displayEntryLevel entryMethodData
            }
        let selectEntryLevelController (selectEntryLevel: SelectEntryLevel) state input = 
            result {
                match input with 
                    | Some str -> 
                        let! level              = EntryLevel.validate str
                        let! entryMethodData    = getEntryMethodSelectedData state
                        let! entryLevelData     = selectEntryLevel  level entryMethodData 
                        return (EntryLevelSelected entryLevelData)
                    | None -> 
                        ///TO DOs
                        let! entryMethodData    = getEntryMethodSelectedData state
                        let entrylevelData =
                            {   
                                EntryLevelSelectedData.UserInfo    = entryMethodData.UserInfo 
                                Site        = entryMethodData.Site
                                EntryMethod = entryMethodData.EntryMethod
                                EntryLevel  = EntryLevel.WorkCenter
                            }
                        return (EntryLevelSelected entrylevelData)
            }
        
        let unselectEntryLevelController state = 
            getEntryLevelSelectedData state
            |> Result.map(fun data -> 
                {
                    EntryMethodSelectedData.UserInfo = data.UserInfo
                    Site = data.Site
                    EntryMethod = data.EntryMethod
                })
            |> Result.map(EntryMethodSelected)
        let displayShopfloorsController (displayShopfloors: DisplayShopFloors) state = 
            result {
                let! entryLevelData = getEntryLevelSelectedData state
                return! displayShopfloors entryLevelData
            }

        let selectShopfloorController (selectShopFloor: SelectShopFloor) state input = 
            result {
                let! entryLevelData = getEntryLevelSelectedData state
                let! shopfloor       = DBService.validateShopFloor entryLevelData.Site input
                let! shopfloordata = selectShopFloor shopfloor entryLevelData
                return (ShopFloorSelected shopfloordata ) 
            } 
        let unselectShopFloorController state =
            getEntryLevelSelectedData state
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
                let! shopfloorData = getSelectShopFloorData state
                return! displayWorkcenters shopfloorData
            }
        let selectWorkCenterController (selectWorkCenter: SelectWorkCenter) state input = 
            result {
                match input with
                    | Some str -> 
                        let! shopfloordata  = getSelectShopFloorData state
                        let! workcenter     = DBService.validateWorkCenter shopfloordata.ShopFloor str
                        let! workcenterdata = selectWorkCenter workcenter shopfloordata
                        return (WorkCenterSelected workcenterdata)
                    | None -> 
                        let! shopfloordata  = getSelectShopFloorData state
                        let workcenterdata = 
                            {
                                Site        = shopfloordata.Site
                                UserInfo    = shopfloordata.UserInfo 
                                EntryMethod = shopfloordata.EntryMethod
                                EntryLevel  = shopfloordata.EntryLevel
                                ShopFloor   = shopfloordata.ShopFloor
                                WorkCenter  = None

                            }
                        return (WorkCenterSelected workcenterdata)
            }

        let unselectWorkCenterController state =
            getWorkCenterSelectedData state
            |> Result.map(fun data ->
                {
                    ShopFloorSelectedData.Site        = data.Site
                    UserInfo    = data.UserInfo 
                    EntryMethod = data.EntryMethod
                    EntryLevel  = data.EntryLevel
                    ShopFloor   = data.ShopFloor

                }) 
            |> Result.map(ShopFloorSelected)
        
        let siteCreationController state input (createSite: CreateSite) =            
            result {
                let! loggedInData  = getLoggedInData state
                let! createSiteData = createSite input loggedInData
                
                return (SiteCreated createSiteData)
            }
