// reference the type provider dll
#r "../../packages/SQLProvider/lib/FSharp.Data.SqlProvider.dll"
#r "System.Configuration"
#r "../../packages/FSharp.Configuration/lib/net40/FSharp.Configuration.dll"

//#r "./bin/Debug/TimeEntry.exe"

#load "./Helpers.fs" 
#load "./DomainTypes.fs"
#load "./Constructors.fs"
#load "./Database.fs"
#load "./DatabaseAPI.fs"
open FSharp.Data.Sql
open TimeEntry.Result
open TimeEntry.Conversions
open TimeEntry.DomainTypes
open TimeEntry.Constructors
open TimeEntry.DataBase
open TimeEntry.DBCommands


open System.Configuration
open FSharp.Configuration


#load "./ConstrainedTypes.fs"
open TimeEntry.ConstrainedTypes

let t = String5 "abcd"
t.Value



let t = ConfigurationManager.ConnectionStrings.Item("Dev").ConnectionString

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

getShopFloorCodes()

getShopFloorInfo("F211A")

insertShopfloor ({Site = Site "F23"; ShopFloor = ShopFloor "F231A"})

getShopFloorCodes()

desactivateShopfloor("F231A")
getShopFloorCodes()
activateShopfloor("F231A")

getWorkCenterCodes()

getWorkCenter ("F1")

getWorkCenterCodes()

//Test to write
getWorkCenterCodes()
desactivateWorkCenter ("F1")
activateWorkCenter ("F1")

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

getWorkOrderCodes()

let wo1 = { WorkOrder = WorkOrder "12243"; ItemCode = ItemCode "099148"; WorkCenter = WorkCenter "F1"; TotalMachineTimeHr = TimeHr 0.f; TotalLabourTimeHr = TimeHr 0.f; Status =  Open }
getWorkCenterCodes()
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

getTimeRecord 4u

// Add a logic to add total time on workorder
// => Only when time record status is validated.
// Add logic to prevent from entering multiple time record on work order for same day ?
// => For machine yes
// => For labour not mandatory