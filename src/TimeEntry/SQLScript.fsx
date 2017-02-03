// reference the type provider dll
#r "../../packages/SQLProvider/lib/FSharp.Data.SqlProvider.dll"
#r "System.Configuration"
#r "../../packages/FSharp.Configuration/lib/net40/FSharp.Configuration.dll"

//#r "./bin/Debug/TimeEntry.exe"

#load "./Helpers.fs"
#load "./ConstrainedTypes.fs"
#load "./DomainTypes.fs"
#load "./Constructors.fs"
#load "./Database.fs"
#load "./DatabaseAPI.fs"
#load "./DbService.fs"
open FSharp.Data.Sql
open TimeEntry.Result
open TimeEntry.Conversions
open TimeEntry.ConstrainedTypes
open TimeEntry.DomainTypes
open TimeEntry.Constructors
open TimeEntry.DBConversions
open TimeEntry.DBCommands
open TimeEntry.DBService


open System.Configuration
open FSharp.Configuration
let exePath = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "./bin/Debug/TimeEntry.exe.config")
let config = ConfigurationManager.OpenExeConfiguration(exePath)

config.Sections

//Reads in machine config
let t = ConfigurationManager.ConnectionStrings.["Dev"]


//Connection string described here: https://www.connectionstrings.com/mysql/
let [<Literal>] ConnectionString  = "Server=localhost;Port=3306;Database=timeentryapp;User=root;Password="

//Path to mysql ODBC divers: http://fsprojects.github.io/SQLProvider/core/parameters.html
let [<Literal>] ResolutionPath = __SOURCE_DIRECTORY__ + @"/../../packages/MySql.Data/lib/net45"

type Sql = SqlDataProvider<
            ConnectionString = ConnectionString,
            DatabaseVendor = Common.DatabaseProviderTypes.MYSQL,
            ResolutionPath = ResolutionPath,
            IndividualsAmount = 1000,
            UseOptionTypes = true,
            Owner = "timeentryapp" >


type DBContext = Sql.dataContext

FSharp.Data.Sql.Common.QueryEvents.SqlQueryEvent |> Event.add (printfn "Executing SQL: %s")

insertNewSite [ "F21"; "F22"] ("F23")

insertSite(Site "F22")

getSiteCodes()

let sf1 = {ShopFloorInfo.ShopFloor = ShopFloor "F211A"; Site = Site "F21"}
let sf2 = {ShopFloorInfo.ShopFloor = ShopFloor "F221A"; Site = Site "F22"}

insertShopfloor(sf1)
insertShopfloor(sf2)

getShopFloorCodes()

getShopFloorInfo("F211A")

let sf3 = {ShopFloorInfo.Site = Site ("F23"); ShopFloor = ShopFloor ("F231A") }
insertShopfloor (sf3)

getShopFloorCodes()

desactivateShopfloor("F231A")
getShopFloorCodes()
activateShopfloor("F231A")

let wc1 = {WorkCenterInfo.WorkCenter = WorkCenter "F1"; ShopFloorInfo = sf1; StartHour = Hour 4u; EndHour = Hour 4u}
insertWorkCenter(wc1)

let wc1' = {wc1 with StartHour = Hour 5u }
updateWorkCenter wc1.WorkCenter wc1'

let wc2 = {WorkCenterInfo.WorkCenter = WorkCenter "F2"; ShopFloorInfo = sf1; StartHour = Hour 4u; EndHour = Hour 4u}
insertWorkCenter(wc2)


getWorkCenterCodes()
getWorkCenter ("F1")

//Test to write
getWorkCenterCodes()
desactivateWorkCenter ("F1")
activateWorkCenter ("F1")

let m1: MachineInfo = {Machine = Machine "Rooslvo"; ShopFloorInfo = sf1}
insertMachine(m1)

let m2: MachineInfo = {Machine = Machine "Scoel12"; ShopFloorInfo = sf2}
insertMachine(m2)

getMachineCodes()
desactivateMachine("Rooslvo")
activateMachine("Rooslvo")

let format = WithoutInfo "FOR"
let div = ZeroPerson "DIV"
let pan = WithInfo "PAN"
let arr = WithInfo "ARR"

[format; div; pan; arr]
|> List.map insertEvent


getEventCodes()

let ev = ZeroPerson "NET"
insertEvent(ev)
getEventCodes()

//TESTS
getEvent("NET")
getEvent("EOD")

updateEvent( WithoutInfo "NET")
getEvent("NET")

updateEvent(WithInfo "NET")
getEvent("NET")

desactivateEvent ("NET")
activateEvent    ("NET")

updateEvent (WithoutInfo "NET")
getEvent ("NET")

let wo1 = { WorkOrder = WorkOrder "12243"; ItemCode = ItemCode "099148"; WorkCenter = WorkCenter "F1"; TotalMachineTimeHr = TimeHr 0.f; TotalLabourTimeHr = TimeHr 0.f; Status =  Open }
getWorkOrderCodes()

insertWorkOrderEntry wo1
getWorkOrderCodes()

getWorkOrder ("12243")

getWorkOrder ("12242")
let wo1' = {wo1 with TotalMachineTimeHr = TimeHr 1000.f}

updateWorkOrderEntry wo1'

getWorkOrder ("12243")

let eventEntry = EventWithoutInfo (WithoutInfo "NET")
insertEventEntry eventEntry

let eventEntry2 = EventWithInfo (WithInfo "ARR", {Machine =Machine "ZX"; Cause="Arrêt imprévu";Solution="Brancher la prise";Comments="A retenir" })
insertEventEntry eventEntry2

getEventEntry (5u)

let timeEntry = MachineAndLabour ( { StartTime = System.DateTime(2016, 12, 15, 12, 01, 12); EndTime = System.DateTime(2016, 12, 15, 15, 01, 12) },  NbPeople 2.f)

let timeRecord = 
    {
        TimeRecord.Site = Site "F21"
        ShopFloor = ShopFloor "F211A"
        WorkCenter = WorkCenter "F1"
        TimeEntry =  timeEntry
        Allocation =  WorkOrderEntry wo1 
        Status = Entered
    }
 
getShopFloorCodes()
insertTimeRecord timeRecord

getTimeRecord 5u
let ctx = Sql.GetDataContext()
let login = "moureed1"
getUser(login)


// Add a logic to add total time on workorder
// => Only when time record status is validated.
// Add logic to prevent from entering multiple time record on work order for same day ?
// => For machine yes
// => For labour not mandatory