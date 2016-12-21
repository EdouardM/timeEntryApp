module TimeEntry.Tests

open Expecto

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