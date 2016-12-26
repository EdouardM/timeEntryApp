module TimeEntry.Tests

open Expecto

open TimeEntry.DomainTypes
open TimeEntry.DBCommands
open TimeEntry.DBLib

[<Tests>]
let testinsertSite = 
  testCase "Insert Site" <| fun () -> 
    DBLib.removeExistingData()
    //Insert Site
    insertSite(Site "F21") |> ignore
    
    //Count Site
    let cnt = getSiteCodes() |> List.length

    Expect.equal cnt 1 "We expect one site after insert: "

[<Tests>]
let tests =
  testCase "Hello World" <| fun () ->
    let subject = "Hello world"
    Expect.equal subject "Hello World"
                 "The strings should equal"

[<Tests>]
let simpleTest = 
  testCase "A simple test" <| fun () -> 
    let expected = 4
    Expect.equal (2 + 2) expected "2 + 2 = 4"


[<EntryPoint>]
let main args =
  runTestsInAssembly defaultConfig args