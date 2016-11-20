// reference the type provider dll
#r "../../packages/SQLProvider/lib/FSharp.Data.SqlProvider.dll" 
open FSharp.Data.Sql
//Connection string described here: https://www.connectionstrings.com/mysql/
let [<Literal>] ConnectionString  = "Server=localhost;Port=3306;Database=timeentryapp;User=root;Password="

//Path to mysql ODBC divers: http://fsprojects.github.io/SQLProvider/core/parameters.html

let [<Literal>] ResolutionPath = __SOURCE_DIRECTORY__ + @"/../../packages/MySql.Data/lib/net45"

type sql = SqlDataProvider< 
              ConnectionString = ConnectionString,
              DatabaseVendor = Common.DatabaseProviderTypes.MYSQL,
              ResolutionPath = ResolutionPath,
              IndividualsAmount = 1000,
              UseOptionTypes = true,
              Owner = "timeentryapp" >

let ctx = sql.GetDataContext()

//Create one record
let timerecord = ctx.Timeentryapp.Timerecord.Create()

timerecord.Site <- Some "F21"
timerecord.Shopfloor <- Some "F211"
timerecord.TimeType <- Some 1u
timerecord.NbPeople <- Some 1u
timerecord.DurationMn <- Some 120u 

ctx.SubmitUpdates()