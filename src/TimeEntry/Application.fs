namespace TimeEntry

module Application =
    open System
    open TimeEntry.Result
    open TimeEntry.DomainTypes
    open TimeEntry.DBService

    (* SERVICES IMPLEMENTATION *)
    let userLogin: UserLogin =
        fun usercredential ->  
            usercredential.Login
            |> getUserInfo 
            |> Result.bind
                (fun userinfo -> 
                    if userinfo.Password = usercredential.Password then
                        Success { LoggedInData.UserInfo = userinfo }
                    else
                        Failure "Wrong Password"
                )
        
    //Application returns here an id of a capability to communicate to the client
    //URL in case of web app
    let getCapabilities state =
        match state with
            | LoggedOut               -> [LoginCap]
            | LoggedIn  d             -> [SelectSite; Logout]
            | SiteSelected  d         -> [UnselectSite;Logout]
            | ShopFloorSelected d    -> [UnselectSite; Logout]
    
    
    //Global variable
    //In web app store it in session variable (.NET type HttpContext.SessionState) 
    //or store in a cookie
    let mutable sessionState = LoggedOut
    
    let loginController usercredential (userLogin: UserLogin) =
        match sessionState with
            | LoggedOut -> 
                let result = userLogin usercredential
                match result with
                    | Success data -> 
                        sessionState <- LoggedIn data
                    | Failure msg  ->
                        printfn "%s" msg
            | _ -> printfn "Already logged in."

    let logoutController () =
        sessionState <- LoggedOut


    let getLoggedInData sessionState =
        match sessionState with
            | LoggedIn data -> Success data
            | _             -> Failure "Can't select Site"

    let siteSelector state site (selectSite: SelectSite) (getSite: string -> Result<Site>) =
        let siteRes = getSite site
        let loggedInDataRes = getLoggedInData state
        selectSite
        <!> siteRes 
        <*> loggedInDataRes
        |> Result.map SiteSelected

    let siteSelectController site (selectSite: SelectSite) (createSite: string -> Result<Site>) =
        siteSelector sessionState site selectSite createSite
        |> function
                | Success newState ->  
                            printfn "Success"
                            sessionState <- newState
                | Failure msg -> printfn "%s" msg 
        
    let renderView state msg =
        printfn "Result of previous operation was: %s" msg
        let capabilities = getCapabilities state
        if List.contains Logout capabilities then
            printfn "click here to logout"
        if List.contains LoginCap capabilities then
            printfn "click here to login"
