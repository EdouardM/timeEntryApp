namespace TimeEntry

module Application =
    open System
    open TimeEntry.Result
    open TimeEntry.DomainTypes

    (* DEFINITION OF CAPABILITIES *)
    module Capabilities = 
        open TimeEntry.DBService
        open TimeEntry.Authorization
        let updatePasswordForSameLogin: Login -> UserInfo -> UpdatePasswordCap option =
            fun login userinfo ->
                onlyForSameLogin login userinfo DBService.updatePassword

        let displayAuthSite userinfo = hasAuthSites userinfo DBService.getAuthSiteCodes 
        let selectAuthSite site userinfo = onlyForAuthSites site userinfo retn

        let desactivateOnlyAuthSite site (userinfo:UserInfo) = 
            onlyForAuthSites site userinfo DBService.desactivateSite

        let createNewSiteForAdmin userinfo = onlyForAdmin userinfo DBService.newSite


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
                
        let displayShopFloors: DisplayShopFloors = 
            fun siteSelectedData -> 
                let msg = sprintf "No active Shopfloor defined for the site: %s" <| siteSelectedData.Site.ToString()
                getActiveShopFloorCodesBySite siteSelectedData.Site
                |> failIfEmpty msg
        let unselectSite: UnSelectSite = 
            fun siteSelectedData ->
                let userinfo = siteSelectedData.UserInfo
                { UserInfo = userinfo}


        let desactivateSite: DesactivateSite =
            fun siteSelectedData ->
                let site =  siteSelectedData.Site
                let desactivateCap = desactivateOnlyAuthSite site  siteSelectedData.UserInfo
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


    (* IMPLEMENT PROGRAM *)
    module Program = 
        open Services
        open Capabilities

        //Application returns here one key and the capability to communicate to the client
        //URL in case of web app
        let getCapabilities state =
            match state with
                | LoggedOut               -> Map.ofList ["L", LoginCap; "E", ExitCap]
                | LoggedIn  d             -> Map.ofList ["U", UpdatePasswordCap; "S", SelectSiteCap; "C", CreateSiteCap; "L", LogoutCap]
                | PasswordUpdated d       -> Map.ofList ["U", UpdatePasswordCap; "S", SelectSiteCap; "C", CreateSiteCap; "L", LogoutCap] 
                | SiteSelected  d         -> Map.ofList ["U", UnselectSiteCap; "S", SelectShopFloorCap; "L", LogoutCap] 
                | SiteCreated   d         -> Map.ofList ["U", UnselectSiteCap; "L", LogoutCap] 
                | ShopFloorSelected d     -> Map.ofList ["U", UnselectShopFloorCap; "L", LogoutCap] 
                | Exit                    -> Map.ofList []  
                
        let getLoggedInData =
            function
                | LoggedIn data -> Success data
                | _             -> Failure "Not in LoggeedIn state, cannot access to inner data."
        let getSelecteSiteData = 
            function
                | SiteSelected data -> Success data
                | _                 -> Failure "Not in Selected Site state, cannot access to inner data."

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

        let siteSelectController (selectSite: SelectSite) state input =
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
        
        let displayShopfloorsController (displayShopfloors: DisplayShopFloors) state = 
            result {
                let! siteselectedData = getSelecteSiteData state
                return! displayShopfloors siteselectedData
            }
        let siteCreationController state input (createSite: CreateSite) =            
            result {
                let! loggedInData  = getLoggedInData state
                let! createSiteData = createSite input loggedInData
                
                return (SiteCreated createSiteData)
            }
