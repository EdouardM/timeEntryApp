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
        | Cap.Login -> 
            printfn "[Login] Enter your login:"
            let login = Console.ReadLine()
            printfn "[Login] Enter your password:"
            let password = Console.ReadLine()
            
            UserCredential.createDTO login password
            |> Program.loginController Services.userLogin state
        
        | Cap.UpdatePassword -> 
            printfn "[Update Password] Enter your login:"
            let login = Console.ReadLine()
            printfn "[Update Password] Enter your new password: "
            let password = Console.ReadLine()
            
            Program.updatePasswordController Services.updatePassword state login password

        | Cap.SelectSite -> 
            result {
                let! sites = 
                    Program.displaySitesController Services.displaySite state
                let choices = List.reduce(fun s s' -> s + " , " + s') sites
                printfn "[Select Site] Possible Choice: %s" choices
                printfn "[Select Site] Enter the site to select:"
                let site = Console.ReadLine()

                return! Program.selectSiteController Services.selectSite state site 
            }

        | Cap.UnselectSite -> Program.unselectSiteController state       
        | Cap.SelectEntryMethod ->
            result {
                let! modes = Program.displayEntryMethodController Services.displayEntryMethod state
                let choices = List.reduce(fun s s' -> s + " , " + s') modes
                printfn "[Select Time Entry Mode] Possible Choice: %s" choices
                let mode = Console.ReadLine()

                return! Program.selectEntryMethodController Services.selectEntryMethod state mode
            }
        | Cap.UnselectEntryMethod -> Program.unselectEntryMethodController state
        | Cap.SelectEntryLevel -> 
            result {
                let! levels = Program.displayEntryLevelController Services.displayEntryLevel state
                let input = Option.map(
                                    fun levels -> 
                                        let choices = List.reduce(fun s s' -> s + " , " + s') levels
                                        printfn "[Select Entry Level] Possible Choice: %s" choices

                                        printfn "[Select Entry Level] Enter the Entry Level to select:"
                                        Console.ReadLine()) levels

                return! Program.selectEntryLevelController Services.selectEntryLevel state input 
            }

        |Cap.UnselectEntryLevel -> Program.unselectEntryLevelController state

        | Cap.SelectShopFloor -> 
            result {
                let! shopfloors = Program.displayShopfloorsController Services.displayShopFloors state
                let choices = List.reduce(fun s s' -> s + " , " + s') shopfloors
                printfn "[Select ShopFloor] Possible Choice: %s" choices

                printfn "[Select ShopFloor] Enter the shopfloor to select:"
                let site = Console.ReadLine()
            
                return! Program.selectShopfloorController Services.selectShopFloor state site 
            }

        | Cap.UnselectShopFloor -> Program.unselectShopFloorController state

        | Cap.SelectWorkCenter -> 
            result {
                let! workcenters = Program.displayWorkCentersController Services.displayWorkCenters state
                let input = Option.map(
                                    fun wcs -> 
                                        let choices = List.reduce(fun s s' -> s + " , " + s') wcs
                                        printfn "[Select WorkCenter] Possible Choice: %s" choices

                                        printfn "[Select WorkCenter] Enter the workcenter to selesct:"
                                        Console.ReadLine()) workcenters

                return! Program.selectWorkCenterController Services.selectWorkCenter state input 
            }

            // If EntryLevel = Shop Floor => None 
            // Else          = display + selectWorkCenter => Some wc
            // Return Workcenterselected data
            //Success state
        | Cap.UnselectWorkCenter -> Program.unselectWorkCenterController state

        | Cap.Exit -> Program.exitController ()
        
        //Any other capability => logout
        | _  -> 
                printfn "Not implemented yet. Logging out..."
                Program.logoutController ()
let capabilityToMenu =
    function
        | Cap.Login              -> "(L)ogin"
        | Cap.UpdatePassword     -> "(U)pdate your password"
        | Cap.SelectSite         -> "(S)elect one site"
        | Cap.CreateSite         -> "(C)reate one site"
        | Cap.SelectEntryMethod    -> "(R)ecord time"
        | Cap.UnselectEntryMethod  -> "(U)nselect entry mode"
        | Cap.SelectEntryLevel   -> "(S)elect entry level"
        | Cap.UnselectEntryLevel -> "(U)nselect entry level"
        | Cap.UnselectSite       -> "(U)nselect site"
        | Cap.SelectShopFloor    -> "(S)elect one shopfloor"
        | Cap.UnselectShopFloor  -> "(U)nselect shopfloor"
        | Cap.SelectWorkCenter   -> "(S)elect one workcenter"
        | Cap.UnselectWorkCenter -> "(U)nselect workcenter"
        | Cap.SelectAttribution  -> "(S)elect time attribution"
        | Cap.Logout             -> "(L)ogout"
        | Cap.Exit               -> "(E)xit"

let processAction state capabilities =
    let input = Console.ReadLine().ToUpper()
    result {
        let! capabilitiesM = failIfNotInMap capabilities input "Invalid choice"
        let cap = capabilitiesM.Item(input)
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
        | ShopFloorSelected data -> 
            let loggedinmsg = 
                sprintf "You are logged in as: %s" 
                <| data.UserInfo.Login.ToString()

            let siteselectmsg = 
                sprintf "You have selected the site: %s" 
                <| data.Site.ToString()
            
            let shopfloormsg = 
                sprintf "You have selected the shopfloor: %s" 
                <| data.ShopFloor.ToString()
            
            //Compose msg:
            loggedinmsg + "\n" + siteselectmsg + "\n" + shopfloormsg
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