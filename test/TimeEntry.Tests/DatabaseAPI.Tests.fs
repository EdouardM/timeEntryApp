module TimEntry.Tests.DatabasAPI

open Expecto

open TimeEntry.DomainTypes
open TimeEntry.DataBase
open TimeEntry.DBCommands
open TimeEntry.DBLib
open TimeEntry.Result

let stopOnFailure = function
  | Success x   -> ()
  | Failure msg -> Expect.equal (Failure msg) (Success ()) msg

//Unit tests of Database API functions
//Inspired by : https://fsharpforfunandprofit.com/posts/low-risk-ways-to-use-fsharp-at-work-4/

[<Tests>]
let testSite = 
  let site = "F21"
  
  testList "Site Database API" [
    testCase "Insert & Get" <| fun _ -> 
      
      removeExistingData() 
      |> stopOnFailure
      
      let step1 = "We expect to get one site after insert."
      insertSite(Site "F21")
      |> stopOnFailure
      
      let cnt = getSiteCodes() |> List.length
      Expect.equal cnt 1 step1;

    testCase "Desactivate" <| fun _ ->

      let step2 = "We expect to get no site after desactivation."
      desactivateSite(site)
      |> stopOnFailure
      
      let cnt = getSiteCodes() |> List.length
      Expect.equal cnt 0 step2;

    testCase "Activate" <| fun _ ->
      let step3 = "We expect to get one site after reactivation."

      activateSite(site)
      |> stopOnFailure

      let cnt = getSiteCodes() |> List.length
      Expect.equal cnt 1 step3 ]

[<Tests>]
let testShopfloor = 
  let sf: ShopFloorInfo = {Site = Site "F21"; ShopFloor = ShopFloor "F211A"}
  let sfCode = "F211A"

  testList "ShopFloor Database API" [
    testCase "Insert & Get" <| fun _ -> 
      removeExistingData() 
      |> stopOnFailure
      
      let step1 = "We expect to get one shopfloor after insert."
      
      insertShopfloor(sf) 
      |> stopOnFailure

      let cnt = getShopFloorCodes() |> List.length
      Expect.equal cnt 1 step1;

    testCase "Desactivate" <| fun _ ->
      let step2 = "We expect to get no shopfloor after desactivation."
      desactivateShopfloor(sfCode)
      |> stopOnFailure

      let cnt = getShopFloorCodes() |> List.length
      Expect.equal cnt 0 step2;

    testCase "Activate" <| fun _ ->
      let step3 = "We expect to get one shopfloor after reactivation."
      
      activateShopfloor(sfCode)
      |> stopOnFailure

      let cnt = getShopFloorCodes() |> List.length
      Expect.equal cnt 1 step3 ]

[<Tests>]
let testWorkCenter = 
  let site = Site "F21"
  let sf: ShopFloorInfo =  {Site = site; ShopFloor = ShopFloor "F211A"}
  let wc: WorkCenterInfo = {WorkCenterInfo.WorkCenter = WorkCenter "F1"; ShopFloorInfo = sf; StartHour = Hour 4u; EndHour = Hour 4u}
  let wcCode = "F1"

  testList "WorkCenter Database API" [
    testCase "Insert & Get" <| fun _ -> 
      removeExistingData() 
      |> stopOnFailure

      //Insert necessary data 
      insertSite(site)    |> ignore
      insertShopfloor(sf) |> ignore

      let step1 = "We expect to get one workcenter after insert."
      
      insertWorkCenter(wc) 
      |> stopOnFailure

      let cnt = getWorkCenterCodes() |> List.length
      Expect.equal cnt 1 step1;

    testCase "Desactivate" <| fun _ ->
      let step2 = "We expect to get no workcenter after desactivation."
      desactivateWorkCenter(wcCode)
      |> stopOnFailure

      let cnt = getWorkCenterCodes() |> List.length
      Expect.equal cnt 0 step2;

    testCase "Activate" <| fun _ ->
      let step3 = "We expect to get one workcenter after reactivation."
      
      activateWorkCenter(wcCode)
      |> stopOnFailure

      let cnt = getWorkCenterCodes() |> List.length
      Expect.equal cnt 1 step3

    testCase "Update" <| fun _ -> 
      let step4 = "We get the updated workcenter after update."
      
      let wc2 = {wc with StartHour = Hour 5u; EndHour = Hour 5u}
      
      updateWorkCenter wc.WorkCenter wc2 
      |> stopOnFailure

      let expected = Success <| toDBWorkCenterInfo wc2
      let dbwc = getWorkCenter(wcCode)

      Expect.equal dbwc expected step4 ]

[<Tests>]
let testMachine = 
  let sf: ShopFloorInfo = {Site = Site "F21"; ShopFloor = ShopFloor "F211A"}
  let m1: MachineInfo = {Machine = Machine "Rooslvo"; ShopFloorInfo = sf}
  let (Machine machCode) = m1.Machine

  testList "Machine Database API" [
    testCase "Insert & Get" <| fun _ -> 
      removeExistingData() 
      |> stopOnFailure
      
      let step1 = "We expect to get one machine after insert."
      
      //Insert necessary data
      insertShopfloor(sf) |> ignore

      insertMachine m1
      |> stopOnFailure

      let cnt = getMachineCodes() |> List.length
      Expect.equal cnt 1 step1;

    testCase "Desactivate" <| fun _ ->
      let step2 = "We expect to get no machine after desactivation."
      
      desactivateMachine(machCode)
      |> stopOnFailure

      let cnt = getMachineCodes() |> List.length
      Expect.equal cnt 0 step2;

    testCase "Activate" <| fun _ ->
      let step3 = "We expect to get one machine after reactivation."
      
      activateMachine(machCode)
      |> stopOnFailure

      let cnt = getMachineCodes() |> List.length
      Expect.equal cnt 1 step3 ]

[<Tests>]
let testEvent = 
  let pan = WithInfo "PAN"

  testList "Event Database API" [
    testCase "Insert & Get" <| fun _ -> 
      removeExistingData() 
      |> stopOnFailure
      
      let step1 = "We expect to get one event after insert."
      
      insertEvent(pan)
      |> stopOnFailure

      let cnt = getEventCodes() |> List.length
      Expect.equal cnt 1 step1;

    testCase "Desactivate" <| fun _ ->
      let step2 = "We expect to get no event after desactivation."
      
      desactivateEvent("PAN")
      |> stopOnFailure

      let cnt = getEventCodes() |> List.length
      Expect.equal cnt 0 step2;

    testCase "Activate" <| fun _ ->
      let step3 = "We expect to get one event after reactivation."
      
      activateEvent("PAN")
      |> stopOnFailure

      let cnt = getEventCodes() |> List.length
      Expect.equal cnt 1 step3
      
    testCase "Update" <| fun _ -> 
      let step4 = "We get the updated event after update."
      
      let pan' = WithoutInfo "PAN"
      
      updateEvent pan'
      |> stopOnFailure

      let expected = Success <| toDBEvent pan'
      let dbwc = getEvent("PAN")

      Expect.equal dbwc expected step4]


[<Tests>]
let testWorkOrder =  
  let wo = { 
          WorkOrder   = WorkOrder "12243"; 
          ItemCode    = ItemCode "099148"; 
          WorkCenter  = WorkCenter "F1"; 
          TotalMachineTimeHr = TimeHr 0.f; 
          TotalLabourTimeHr = TimeHr 0.f; 
          Status      =  Open }

  let (WorkOrder woCode) = wo.WorkOrder
  testList "WorkOrder Database API" [    
    testCase "Insert & Get" <| fun () -> 
      removeExistingData()
      |> stopOnFailure
      
      //Insert Reference data
      insertReferenceData()
      
      let step1 = "We expect to get one work order after insert."
      
      insertWorkOrderEntry wo 
      |> stopOnFailure

      let cnt = getWorkOrderCodes() |> List.length
      Expect.equal cnt 1 step1;

    testCase "Update" <| fun _ -> 
      let step4 = "We get the updated work order after update."
      
      //change all fields except workorder code (Id)
      let wo' = { 
          WorkOrder   = wo.WorkOrder
          ItemCode    = ItemCode "099146"; 
          WorkCenter  = WorkCenter "F2"; 
          TotalMachineTimeHr = TimeHr 230.f; 
          TotalLabourTimeHr = TimeHr 120.f; 
          Status      =  Closed }
      
      updateWorkOrderEntry wo'
      |> stopOnFailure

      let expected = Success <| toDBWorkOrderEntry wo'
      let dbwo = getWorkOrder(woCode)

      Expect.equal dbwo expected step4]

[<Tests>]
let testEventEntries =
  let config = { FsCheck.Config.Default with MaxTest = 10000 }   
  
  testList "FsCheck" [

    testProperty "Generate Dummy Data" <| fun a b ->
    
    removeExistingData()
    |> stopOnFailure
      
    //Insert Reference data
    insertReferenceData()
  
    let sites = getSiteCodes()
    let generateSite() = FsCheck.Gen.elements sites 
    
    let shopfloor = getShopFloorCodes()
    let generateShopfloor() = FsCheck.Gen.elements sites 
    
      
  
    // you can also override the FsCheck config
    testPropertyWithConfig config "Product is distributive over addition" <|
      fun a b c ->
        a * (b + c) = a * b + a * c
  ]


[<EntryPoint>]
let main args =
  let config = { defaultConfig with parallel  = false }
  runTestsInAssembly config args