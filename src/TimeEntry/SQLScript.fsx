// reference the type provider dll
#r "../../packages/SQLProvider/lib/FSharp.Data.SqlProvider.dll"
#r "System.Configuration"
#r "../../packages/FSharp.Configuration/lib/net40/FSharp.Configuration.dll"

//#r "./bin/Debug/TimeEntry.exe"

#load "./Helpers.fs"
#load "./ConstrainedTypes.fs"
#load "./DomainTypes.fs"
#load "./Constructors.fs"
#load "./DBConversions.fs"
#load "./DBCommands.fs"
#load "./DTOTypes.fs"
#load "./DbService.fs"
open FSharp.Data.Sql
open TimeEntry.Result
open TimeEntry.Conversions
open TimeEntry.ConstrainedTypes
open TimeEntry.DomainTypes
open TimeEntry.Constructors
open TimeEntry.DBConversions
open TimeEntry.DBCommands
open TimeEntry.DTO
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

removeExistingData()

let s1 = Site (String3 "F21")

SiteAPI.insert s1
SiteAPI.getSiteCodes Active

SiteAPI.insert (Site (String3 "F22"))

let sf1 = {ShopFloorInfo.ShopFloor = ShopFloor (String5 "F211A"); Site = Site (String3 "F21")}
let sf2 = {ShopFloorInfo.ShopFloor = ShopFloor (String5 "F221A") ; Site = Site (String3 "F22")}

ShopFloorAPI.insert(sf1)
ShopFloorAPI.insert(sf2)

ShopFloorAPI.getShopFloorCodes Active

ShopFloorAPI.getShopFloorInfo Active sf1.ShopFloor

let sf3 = {ShopFloorInfo.Site = Site (String3 "F23"); ShopFloor = ShopFloor (String5 "F231A") }
ShopFloorAPI.insert (sf3)

ShopFloorAPI.getShopFloorCodes Active

ShopFloorAPI.desactivate("F231A")
ShopFloorAPI.getShopFloorCodes Active
ShopFloorAPI.activate("F231A")

let wc1 = {WorkCenterInfo.WorkCenter = WorkCenter (String5 "F1"); ShopFloorInfo = sf1; StartHour = Hour 4u; EndHour = Hour 4u}
WorkCenterAPI.insert(wc1)

let wc1' = {wc1 with StartHour = Hour 5u }
WorkCenterAPI.update wc1'

let wc2 = {WorkCenterInfo.WorkCenter = WorkCenter (String5 "F2"); ShopFloorInfo = sf1; StartHour = Hour 4u; EndHour = Hour 4u}
WorkCenterAPI.insert(wc2)


WorkCenterAPI.getWorkCenterCodes Active
WorkCenterAPI.getWorkCenter ("F1")

//Test to write
WorkCenterAPI.getWorkCenterCodes Active
WorkCenterAPI.desactivate ("F1")
WorkCenterAPI.activate ("F1")

let m1: MachineInfo = {Machine = Machine (String10 "Rooslvo"); ShopFloorInfo = sf1}
MachineAPI.insert(m1)

let m2: MachineInfo = {Machine = Machine (String10 "Scoel12"); ShopFloorInfo = sf2}
MachineAPI.insert(m2)

MachineAPI.getMachineCodes()
MachineAPI.activate("Rooslvo")
MachineAPI.desactivate("Rooslvo")

let formatF21 = { 
            Site            = s1; 
            Code            = ActivityCode (String4 "FOR"); 
            RecordLevel     = WorkCenterLevel AllWorkCenters; 
            TimeType        = MachineTime; 
            ActivityLink    = Linked <| ActivityCode (String4 "MFOR"); 
            ExtraInfo       = ExtraInfo.WithoutInfo
            }
let mformatF21 = {formatF21 with Code = ActivityCode (String4 "MFOR"); ActivityLink = Linked <| ActivityCode (String4 "FOR")}

let divF21 = { 
            Site            = s1; 
            Code            = ActivityCode (String4 "DIV"); 
            RecordLevel     = WorkCenterLevel AllWorkCenters; 
            TimeType        = MachineTime; 
            ActivityLink    = Linked <| ActivityCode (String4 "MDIV"); 
            ExtraInfo       = ExtraInfo.WithoutInfo
            }

let mdivF21 = {formatF21 with Code = ActivityCode (String4 "MDIV"); ActivityLink = Linked <| ActivityCode (String4 "DIV")}

ActivityAPI.insert formatF21
ActivityAPI.insert mformatF21
ActivityAPI.insert divF21
ActivityAPI.insert mdivF21

ActivityAPI.getActivityCodes()

//TESTS
let formatF21'= {formatF21 with ExtraInfo = WithInfo}
ActivityAPI.update formatF21'
ActivityAPI.getActivity "FOR"

ActivityAPI.desactivate "FOR"
ActivityAPI.activate    "FOR"

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

let login = Login (String8 "moureed1")

let user = { 
                Login           = Login (String8 "moureed1"); 
                Password        = Password (String50 "indaclub"); 
                Name            = UserName (String50 "Edouard"); 
                SiteAccess      = AllSites
                Level           = Viewer
            }
UserInfoAPI.update user

UserInfoAPI.getUser (login)

getUserInfo login

UserInfoAPI.updatePassword login (Password (String50 "depzoiam12"))

let user2 = { user with Password = Password (String50 "hello3")}

UserInfoAPI.update user2


let d = "29/02/2017"
stringDate d
let t = "23:10"
stringTime t

open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let phone = "(555) 555-5555"
match phone with
| Regex @"\(([0-9]{3})\)[-. ]?([0-9]{3})[-. ]?([0-9]{4})"  [ area; prefix; suffix ] ->
    printfn "Area: %s, Prefix: %s, Suffix: %s" area prefix suffix
| _ -> printfn "Not a phone number"


let stringTime =
    function
    | Regex @"^(?:(?:(?<hh>[01]\d|2[0-3])))$" [ hour] ->
        printfn "hour %s" hour
    | Regex @"^(?:(?:(?<hh>[01]\d|2[0-3])[:.](?<mm>[0-5]\d)))$" [ hour; minutes  ] ->
        printfn "hour %s / minutes: %s " hour minutes
    | Regex @"^(?:(?:(?<hh>[01]\d|2[0-3])[:.](?<mm>[0-5]\d))[:.](?<ss>[0-5]\d))$" [ hour; minutes ;seconds  ] -> 
        printfn "hour %s / minutes: %s / seconds: %s" hour minutes seconds
    | input -> 
        printfn "Your input: '%s' is not recognized as a time. Expected formats are: 'hh', 'hh:mm', 'hh:mm:ss'." input
let t = "23:23"
stringTime t 


let stringDate = 
    function 
    | Regex @"^(0[1-9]|[12][0-9]|3[01])[/.](0[1-9]|1[012])[/.](19|20\d\d)$" 
        [day; month; year] -> printfn "year: %s; month: %s; day: %s" year month day
    | input -> 
        printfn "Your input: '%s' is not recognized as a date. Expected formats is 'dd/mm/yyyy'" input       

let d = " 19/02/2017"
let t = "23:23:2322"

TimeEntry.Constructors.Time.validate t

TimeEntry.Constructors.DateTime.validate d t 

TimeEntry.DBService.getActivityCodeByTimeTypeAndShopFloor MachineTime (ShopFloor (String5 "F211A"))

// Add a logic to add total time on workorder
// => Only when time record status is validated.
// Add logic to prevent from entering multiple time record on work order for same day ?
// => For machine yes
// => For labour not mandatory