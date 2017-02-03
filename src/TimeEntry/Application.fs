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
                
        let selectSite: SelectSite =
            fun site loggedInData -> 
                let selectSiteCap = selectAuthSite site loggedInData.UserInfo
                match selectSiteCap with
                    | Some cap ->
                        cap site
                        |>Result.map(fun site -> { UserInfo = loggedInData.UserInfo; Site = site})
                    | None -> 
                        Failure "You can only select one site you are authorized to access."
                
        
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

        //Application returns here an id of a capability to communicate to the client
        //URL in case of web app
        let getCapabilities state =
            match state with
                | LoggedOut               -> [LoginCap]
                | LoggedIn  d             -> [UpdatePasswordCap; SelectSiteCap; CreateSiteCap; Logout]
                | PasswordUpdated d       -> [LoginCap]
                | SiteSelected  d         -> [UnselectSite; DesactivateSite; Logout]
                | SiteCreated   d         -> [UnselectSite; DesactivateSite; Logout]
                | ShopFloorSelected d     -> [UnselectSite; Logout]
        
        
        //Global variable
        //In web app store it in session variable (.NET type HttpContext.SessionState) 
        //or store in a cookie
        let mutable sessionState = LoggedOut
        
        let getLoggedInData =
            function
                | LoggedIn data -> Success data
                | _             -> Failure "Not in LoggedIn status, cannot access LoggedIn data."

        let updateState logmsg = 
                function
                    | Success newState ->  
                                printfn "%s" logmsg
                                sessionState <- newState
                    | Failure msg -> printfn "%s" msg 
        let loginController usercredential (userLogin: UserLogin) =
            match sessionState with
                | LoggedOut -> 
                    userLogin usercredential
                    |> Result.map(LoggedIn)
                    |> updateState "Logged in."
                | _ -> printfn "Already logged in."

        let updatePasswordController login password (updatePassword: UpdatePassword) = 
            result {
                let! loggedInData       = getLoggedInData sessionState
                let! updatePasswordData = updatePassword login password loggedInData
                return (PasswordUpdated updatePasswordData)
            }
            |> updateState "Password updated. "

        let logoutController () =
            sessionState <- LoggedOut

        let siteSelectController input (selectSite: SelectSite) (getSite: string -> Result<Site>) =
            result {
                let! site           = getSite input
                let! loggedInData   = getLoggedInData sessionState
                let! selectSiteData = selectSite site loggedInData
                
                return (SiteSelected selectSiteData )
            }
            |> updateState "Site selected"
        

        let siteCreationController input (createSite: CreateSite) =            
            result {
                let! loggedInData  = getLoggedInData sessionState
                let! createSiteData = createSite input loggedInData
                
                return (SiteCreated createSiteData)
            }
            |> updateState "Site created"


        let renderView state msg =
            printfn "Result of previous operation was: %s" msg
            let capabilities = getCapabilities state
            if List.contains Logout capabilities then
                printfn "click here to logout"
            if List.contains LoginCap capabilities then
                printfn "click here to login"
