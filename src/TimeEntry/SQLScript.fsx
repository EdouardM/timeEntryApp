// reference the type provider dll
#r "../../packages/SQLProvider/lib/FSharp.Data.SqlProvider.dll"
#r "../../packages/MySql.Data/lib/net45/MySql.Data.dll" 
open FSharp.Data.Sql
open MySql.Data
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


