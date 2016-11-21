// reference the type provider dll
#r "../../packages/SQLProvider/lib/FSharp.Data.SqlProvider.dll" 
open FSharp.Data.Sql
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

let ctx = Sql.GetDataContext()

//Create one record
let timerecord = ctx.Timeentryapp.Timerecord.Create()

timerecord.Site <- "F21"
timerecord.Shopfloor <- "F211"
timerecord.TimeType <- 1u
timerecord.NbPeople <- 0u
timerecord.DurationMn <- 120u 


try 
    ctx.SubmitUpdates()
with
    | ex -> printfn "%s" ex.Message