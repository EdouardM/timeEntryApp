module TimeEntry.Console
open System
open TimeEntry.Option
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
                let input = 
                    let choices = List.reduce(fun s s' -> s + " , " + s') levels
                    printfn "[Select Entry Level] Possible Choice: %s" choices

                    printfn "[Select Entry Level] Enter the Entry Level to select:"
                    Console.ReadLine()

                return! Program.selectEntryLevelController Services.selectEntryLevel state input 
            }

        | Cap.UnselectEntryLevel -> Program.unselectEntryLevelController state
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
                let input =
                    let choices = List.reduce(fun s s' -> s + " , " + s') workcenters
                    printfn "[Select WorkCenter] Possible Choice: %s" choices

                    printfn "[Select WorkCenter] Enter the workcenter to selesct:"
                    Console.ReadLine()

                return! Program.selectWorkCenterController Services.selectWorkCenter state input 
            }
        | Cap.UnselectWorkCenter -> Program.unselectWorkCenterController state       

         | Cap.SelectEntryMode -> 
            result {
                let! entrymodes = Program.displayEntryModesController Services.displayEntryModes state
                let choices = List.reduce(fun s s' -> s + " , " + s') entrymodes
                printfn "[Select Entry Mode] Possible Choice: %s" choices

                printfn "[Select Entry Mode] Enter the entry mode to select:"
                let mode = Console.ReadLine()
            
                return! Program.selectEntryModeController Services.selectEntryMode state mode
            }

        | Cap.UnselectEntryMode -> Program.unselectEntryModeController state

        | Cap.SelectAttributionType  -> 
            result {
                let! attributions = Program.displayAttributionTypesController Services.displayAttributionTypes state
                let choices = List.reduce(fun s s' -> s + " , " + s') attributions
                printfn "[Select Attribution Type] Possible Choice: %s" choices

                printfn "[Select Attribution Type] Enter the attribution type:"
                let attribution = Console.ReadLine()
            
                return! Program.selectAttributionTypeController Services.selectAttributionType state attribution
            }

        | Cap.UnselectAttrbutionType -> Program.unselectAttributionTypeController state

        | Cap.SelectActivity -> 
            result {
                let! activityCodes = Program.displayActivityCodesController Services.displayActivityCodes state
                let choices = List.reduce(fun s s' -> s + " , " + s') activityCodes
                printfn "[Select Activity] Possible Choice: %s" choices

                printfn "[Select Activity] Enter the activity code:"
                let activityCode = Console.ReadLine()
            
                return! Program.selectActivityCodeController Services.selectAttribution state activityCode
            }

        | Cap.SelectWorkOrder -> 
            result {
                let! workOrders = Program.displayWorkOrdersController Services.displayWorkOrders state
                let choices = List.reduce(fun s s' -> s + " , " + s') workOrders
                printfn "[Select WorkOrder] Possible Choice: %s" choices

                printfn "[Select WorkOrder] Enter the workorder:"
                let workorder = Console.ReadLine()
            
                //To be changed
                return! Program.selectWorkOrderController Services.selectAttribution state workorder
            }
        | Cap.UnselectAttribution -> Program.unselectAttributionController state

        | Cap.EnterDuration -> 
            printfn "[Enter Duration] Enter date (format: dd/mm/yyyy):"
            let date = Console.ReadLine()
            printfn "[Enter Duration] Enter start time (format: hh:mm:ss):"
            let starttime = Console.ReadLine()
            printfn "[Enter Duration] Enter end time (format: hh:mm:ss):"
            let endtime = Console.ReadLine()
            
            Duration.createDTO date starttime endtime
            |> Program.enterDurationController Services.enterDuration state
        
        | Cap.CancelDuration -> Program.cancelDurationController state

        | Cap.EnterNbPeople  -> 
            printfn "[Enter Nb of People] Enter Nb of People:"
            let nb = Console.ReadLine()

            NbPeople.createDTO nb 
            |> Program.enterNbPeopleController Services.enterNbPeople state

        | Cap.CancelNbPeople -> Program.cancelNbPeopleController state

        | Cap.AddRecord      -> Program.addRecordController Services.addRecord state

        | Cap.SaveRecord     -> Program.saveRecordController Services.addRecord Services.saveRecord state

        | Cap.CreateSite ->
                printfn "Not implemented yet. Logging out..."
                Program.logoutController ()

        | Cap.Logout -> Program.logoutController()
        | Cap.Exit -> Program.exitController ()        
let capabilityToMenu =
    function
        | Cap.Login                     -> "(L)ogin"
        | Cap.UpdatePassword            -> "(U)pdate your password"
        | Cap.SelectSite                -> "(S)elect one site"
        | Cap.CreateSite                -> "(C)reate one site"
        | Cap.SelectEntryMethod         -> "(R)ecord time"
        | Cap.UnselectEntryMethod       -> "(U)nselect entry mode"
        | Cap.SelectEntryLevel          -> "(S)elect entry level"
        | Cap.UnselectEntryLevel        -> "(U)nselect entry level"
        | Cap.UnselectSite              -> "(U)nselect site"
        | Cap.SelectShopFloor           -> "(S)elect one shopfloor"
        | Cap.UnselectShopFloor         -> "(U)nselect shopfloor"
        | Cap.SelectWorkCenter          -> "(S)elect one workcenter"
        | Cap.UnselectWorkCenter        -> "(U)nselect workcenter"
        | Cap.SelectEntryMode           -> "(S)elect entry mode"
        | Cap.UnselectEntryMode         -> "(U)nselect entry mode"
        | Cap.SelectAttributionType     -> "(S)elect attribution type"
        | Cap.UnselectAttrbutionType    -> "(U)nselect attribution type"
        | Cap.SelectActivity            -> "(S)elect one activity code"
        | Cap.SelectWorkOrder           -> "(S)elect one work order" 
        | Cap.UnselectAttribution       -> "(U)nselect activity or work oder"
        | Cap.EnterDuration             -> "(E)nter duration"
        | Cap.CancelDuration            -> "(C)ancel duration"
        | Cap.EnterNbPeople             -> "(E)nter Nb of people"
        | Cap.CancelNbPeople            -> "(C)ancel Nb of people"
        | Cap.AddRecord                 -> "(A)dd time record to list"
        | Cap.SaveRecord                -> "(S)ave your entry"
        | Cap.Logout                    -> "(L)ogout"
        | Cap.Exit                      -> "(E)xit"

let processAction state capabilities =
    let input = Console.ReadLine().ToUpper()
    result {
        let! capabilitiesM = failIfNotInMap capabilities input "[Wrong Input] Invalid choice. Try again."
        let cap = capabilitiesM.Item(input)
        return! useCapability state cap   
    }

let printStateMessage = 
    function
        | Exit                  -> 
            Some "Goodbye."
        | LoggedIn data         -> 
            printfn " 
                ***************************
                          LOGGED IN 
                ***************************
            \n"
            sprintf "You are logged in as: %s" 
            <| data.UserInfo.Login.ToString()
            |> Some
        | PasswordUpdated data  -> 
            printfn " 
                ***************************
                      PASSWORD UPDATED 
                ***************************
            \n"
            sprintf "Your password has been successfully updated.\nLogged in as: %s"
            <| data.UserInfo.Login.ToString()
            |> Some
        | LoggedOut             -> 
            printfn " 
                ***************************
                      LOGGED OUT 
                ***************************
            \n"
            Some "Your are logged out."
        | SiteSelected data     -> 
            printfn " 
                ***************************
                      SITE SELECTED 
                ***************************
            \n"
            let loggedinmsg = data.UserInfo.Login.LoginMsg()
            let siteselectmsg = data.Site.SelectMsg()
            
            //Compose msg:
            loggedinmsg 
            + "\n" + "Entry Context: "
            + "\n" + siteselectmsg
            |> Some
        | EntryMethodSelected data -> 
            printfn " 
                ***************************
                   ENTRY METHOD SELECTED 
                ***************************
            \n"
            let loggedinmsg = data.UserInfo.Login.LoginMsg()
            let siteselectmsg = data.Site.SelectMsg()
            let entrymethodmsg = data.EntryMethod.SelectMsg()
            //Compose msg:
            loggedinmsg 
            + "\n" + "Entry Context: "
            + "\n" + siteselectmsg 
            + "\n" + entrymethodmsg
            |> Some
        | EntryLevelSelected data -> 
            printfn " 
                ***************************
                   ENTRY LEVEL SELECTED 
                ***************************
            \n"
            let loggedinmsg = data.UserInfo.Login.LoginMsg()
            let siteselectmsg = data.Site.SelectMsg()
            let entrymethodmsg = data.EntryMethod.SelectMsg()
            let entrylevelmsg  = data.EntryLevel.SelectMsg()
            //Compose msg:
            loggedinmsg 
            + "\n" + "Entry Context: "
            + "\n" + siteselectmsg 
            + "\n" + entrymethodmsg 
            + "\n" + entrylevelmsg
            |> Some

        | ShopFloorSelected data -> 
            printfn " 
                ***************************
                   SHOPFLOOR SELECTED 
                ***************************
            \n"
            let loggedinmsg = data.UserInfo.Login.LoginMsg()
            let siteselectmsg = data.Site.SelectMsg()
            let entrymethodmsg = data.EntryMethod.SelectMsg()
            let entrylevelmsg  = data.EntryLevel.SelectMsg()

            let shopfloormsg = data.ShopFloor.SelectMsg()

            //Compose msg:
            loggedinmsg 
            + "\n" + "Entry Context: "
            + "\n" + siteselectmsg 
            + "\n" + entrymethodmsg 
            + "\n" + entrylevelmsg
            + "\n" + shopfloormsg
            |> Some

        | WorkCenterSelected data ->
            printfn " 
                ***************************
                   WORKCENTER SELECTED 
                ***************************
            \n"
            maybe { 
                let loggedinmsg = data.UserInfo.Login.LoginMsg()
                let siteselectmsg = data.Site.SelectMsg()
                let entrymethodmsg = data.EntryMethod.SelectMsg()
                let entrylevelmsg  = data.EntryLevel.SelectMsg()

                let shopfloormsg = data.ShopFloor.SelectMsg()
            
                let! workcentermsg = 
                    data.WorkCenter
                    |> Option.map(fun wc -> wc.SelectMsg()) 
                
                //Compose msg:
                return loggedinmsg 
                        + "\n" + "Entry Context: "
                        + "\n" + siteselectmsg 
                        + "\n" + entrymethodmsg 
                        + "\n" + entrylevelmsg
                        + "\n" + shopfloormsg
                        + "\n" + workcentermsg }
        | EntryModeSelected data ->
            printfn " 
                ***************************
                   ENTRY MODE SELECTED 
                ***************************
            \n" 
            let loggedinmsg = data.UserInfo.Login.LoginMsg()
            let siteselectmsg = data.Site.SelectMsg()
            let entrymethodmsg = data.EntryMethod.SelectMsg()
            let entrylevelmsg  = data.EntryLevel.SelectMsg()

            let shopfloormsg = data.ShopFloor.SelectMsg()
        
            let workcentermsg = 
                data.WorkCenter
                |> Option.map(fun wc -> "\n" + wc.SelectMsg()) 
                |> optionToString
            let entrymodemsg = data.EntryMode.SelectMsg()

            //Compose msg:
            loggedinmsg 
            + "\n" + "Entry Context: "
            + "\n" + siteselectmsg 
            + "\n" + entrymethodmsg 
            + "\n" + entrylevelmsg
            + "\n" + shopfloormsg
            + workcentermsg
            + "\n" + entrymodemsg
            |> Some

        | AttributionTypeSelected data ->
            printfn " 
                ***************************
                   ATTR. TYPE SELECTED 
                ***************************
            \n" 
            let loggedinmsg = data.Context.UserInfo.Login.LoginMsg()
            let siteselectmsg = data.Context.Site.SelectMsg()
            let entrymethodmsg = data.Context.EntryMethod.SelectMsg()
            let entrylevelmsg  = data.Context.EntryLevel.SelectMsg()

            let shopfloormsg = data.Context.ShopFloor.SelectMsg()
        
            let workcentermsg = 
                data.Context.WorkCenter
                |> Option.map(fun wc -> "\n" + wc.SelectMsg()) 
                |> optionToString
            let entrymodemsg = data.Context.EntryMode.SelectMsg()
            let attrTypemsg  = data.AttributionType.SelectMsg()

            printfn "Records in memory:"
            data.TimeRecords 
            |> List.iter (printfn "%A")
            
            printfn "\n"
            //Compose msg:
            loggedinmsg 
            + "\n" + "Entry Context: "
            + "\n" + siteselectmsg 
            + "\n" + entrymethodmsg 
            + "\n" + entrylevelmsg
            + "\n" + shopfloormsg
            + workcentermsg
            + "\n" + entrymodemsg
            + "\n" + attrTypemsg
            |> Some

        | AttributionSelected data ->
            printfn " 
                ***************************
                   ATTRIBUTION SELECTED 
                ***************************
            \n" 
            let loggedinmsg = data.Context.UserInfo.Login.LoginMsg()
            let siteselectmsg = data.Context.Site.SelectMsg()
            let entrymethodmsg = data.Context.EntryMethod.SelectMsg()
            let entrylevelmsg  = data.Context.EntryLevel.SelectMsg()

            let shopfloormsg = data.Context.ShopFloor.SelectMsg()
        
            let workcentermsg = 
                data.Context.WorkCenter
                |> Option.map(fun wc -> "\n" + wc.SelectMsg()) 
                |> optionToString
            let entrymodemsg = data.Context.EntryMode.SelectMsg()
            let attrTypemsg  = data.AttributionType.SelectMsg()
            let attrmsg = data.Attribution.SelectMsg() 
            
            printfn "Records in memory:"
            data.TimeRecords 
            |> List.iter (printfn "%A")
            
            printfn "\n"
            //Compose msg:
            loggedinmsg 
            + "\n" + "Entry Context: "
            + "\n" + siteselectmsg 
            + "\n" + entrymethodmsg 
            + "\n" + entrylevelmsg
            + "\n" + shopfloormsg
            + workcentermsg
            + "\n" + entrymodemsg
            + "\n" + attrTypemsg
            + "\n" + attrmsg
            |> Some

        | _ -> None
    >> Option.map (printfn "%s\n\n")
    >> ignore

let renderView state =
    let capabilities = Program.getCapabilities state
    
    printfn "Available Actions:"
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

    printfn "
    ----------------------------------
        Welcome to Time Entry App
    ----------------------------------
    \n"
    mainUILoop LoggedOut
    0 