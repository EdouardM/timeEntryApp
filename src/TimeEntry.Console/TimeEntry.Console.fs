module TimeEntry.Console
open System
open TimeEntry.DomainTypes
open TimeEntry.Application
//open TimeEntry.Constructors
open TimeEntry.DTO
open TimeEntry.Result


//Global variable
//In web app store it in session variable (.NET type HttpContext.SessionState) 
//or store in a cookie
//let initialState = LoggedOut

let useCapability state = 
    function
        | LoginCap -> 
            printfn "[Login] Enter your login:"
            let login = Console.ReadLine()
            printfn "[Login] Enter your password:"
            let password = Console.ReadLine()
            
            UserCredential.createDTO login password
            |> Program.loginController Services.userLogin state
        
        | UpdatePasswordCap -> 
            printfn "[Update Password] Enter your login:"
            let login = Console.ReadLine()
            printfn "[Update Password] Enter your new password: "
            let password = Console.ReadLine()
            
            Program.updatePasswordController Services.updatePassword state login password

        | SelectSiteCap -> 
            result {
                let! sites = 
                    Program.displaySitesController Services.displaySite state
                let choices = List.reduce(fun s s' -> s + " , " + s') sites
                printfn "[Select Site] Possible Choice: %s" choices
                printfn "[Select Site] Enter the site to select:"
                let site = Console.ReadLine()

                return! Program.siteSelectController Services.selectSite state site 
            }

        | SelectShopFloorCap -> 
            result {
                let! shopfloors = 
                    Program.displayShopfloorsController Services.displayShopFloors state
                let choices = List.reduce(fun s s' -> s + " , " + s') shopfloors
                printfn "[Select ShopFloor] Possible Choice: %s" choices

                printfn "[Select ShopFloor] Enter the shopfloor to select:"
                let site = Console.ReadLine()
            
                //Implement select shopfloor!
                return! Program.siteSelectController Services.selectSite state site 
            }
            
        | UnselectSiteCap -> Program.unselectSiteController state

        | ExitCap -> Program.exitController ()
        
        //Any other capability => logout
        | _  -> 
                printfn "Not implemented yet. Logging out..."
                Program.logoutController ()
let capabilityToMenu =
    function
        | LoginCap              -> "(L)ogin"
        | UpdatePasswordCap     -> "(U)pdate your password"
        | SelectSiteCap         -> "(S)elect one site"
        | CreateSiteCap         -> "(C)reate one site"
        | UnselectSiteCap       -> "(U)nselect site"
        | SelectShopFloorCap    -> "(S)elect one shopfloor"
        | UnselectShopFloorCap  -> "(U)nselect site"
        | LogoutCap             -> "(L)ogout"
        | ExitCap               -> "(E)xit"
let processAction state capabilities =
    let input = Console.ReadLine().ToUpper()
    let capability = failIfNotInMap capabilities input "Invalid choice"
    result {
        let! cap = capability
        return! useCapability state cap   
    }

let printStateMessage = 
    function
        | Exit                  -> 
            Some "Goodbye."
        | LoggedIn data         -> 
            sprintf "You are logged in as: %s" 
            <| data.UserInfo.Login.ToString()
            |> Some
        | PasswordUpdated data  -> 
            sprintf "Your password has been successfully updated. Logged in as: %s"
            <| data.UserInfo.Login.ToString()
            |> Some
        | LoggedOut             -> 
            Some "Your are logged out."
        | SiteSelected data     -> 
            let loggedinmsg = 
                sprintf "You are logged in as: %s" 
                <| data.UserInfo.Login.ToString()

            let siteselectmsg = 
                sprintf "You have selected the site: %s" 
                <| data.Site.ToString()
            
            //Compose msg:
            loggedinmsg + "\n" + siteselectmsg
            |> Some
        | _                     -> None
    >> Option.map (printfn "%s")
    >> ignore

let renderView state =
    let capabilities = Program.getCapabilities state
    
    //Print options available
    capabilities
    |> Map.toList
    |> List.map (snd >> capabilityToMenu)
    |> List.reduce (fun s t -> s + " , " + t)
    |> printfn "%s"

    //Process choice
    processAction state capabilities

let rec mainUILoop state =
    match state with
        | Exit -> 
                //Print message corresponding to current state:
                printStateMessage state
                ()
        | state ->
                //Print message corresponding to current state:
                printStateMessage state 

                let newStateRes = renderView state
                match newStateRes with
                    | Success newState ->  mainUILoop newState
                    | Failure msg      -> 
                                        printfn "%s" msg
                                        mainUILoop state


[<EntryPoint>]
let main argv =    
    DBService.removeExistingData()  |> ignore
    DBService.insertReferenceData() |> ignore

    printfn "Welcome to Time Entry App."
    mainUILoop LoggedOut
    0 