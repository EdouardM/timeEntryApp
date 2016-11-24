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
let t = ctx.Timeentryapp.Workcenter.Create()

//Create one record
t.Site <- "F21"
t.Shopfloor <- "F211"
t.WorkCenter <- "BOX"
t.StartTime <- 5u
t.EndTime <- 5u
t.Active <- 1y

try 
    ctx.SubmitUpdates()
with
    | ex -> printfn "%s" ex.Message

(*
    query {
    for student in db.Student do
    join selection in db.CourseSelection
        on (student.StudentID = selection.StudentID)
    select (student, selection)
}
*)

let wo = ctx.Timeentryapp.Workorderentry.Create()

let q = query {
        from event in ctx.Event 
        join eventEntry in ctx.EventEntry
        on (event.Id = eventEntry.EventId)
        select event, eventEntry
}

