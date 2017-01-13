module TimEntry.Tests.DatabasAPI

open Expecto

open TimeEntry.DomainTypes
open TimeEntry.DataBase
open TimeEntry.DBCommands
open TimeEntry.DBService
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
      Expect.equal cnt 1 step1

      let step1' = "We get the same shopfloor."
      
      let dbsf = getShopFloorInfo(sfCode)
      let expected = toDBShopfloorInfo sf |> Success
      
      Expect.equal dbsf expected step1';

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
      Expect.equal cnt 1 step1
      
      let step1' = "We get the same workcenter after insert."
      
      let dbwc = getWorkCenter(wcCode)
      let expected = toDBWorkCenterInfo wc |> Success

      Expect.equal dbwc expected step1';

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
      
      updateWorkCenter(wc2)
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
let testActivity = 
  let pan = { 
                Site            = Site "F21"; 
                Code            = ActivityCode "PAN"; 
                RecordLevel     = WorkCenterLevel AllWorkCenters; 
                TimeType        = MachineTime; 
                ActivityLink    = Linked <| ActivityCode "MPAN"; 
                ExtraInfo       = ExtraInfo.WithoutInfo
                }

  testList "Event Database API" [
    testCase "Insert & Get" <| fun _ -> 
      removeExistingData() 
      |> stopOnFailure
      
      let step1 = "We expect to get one activity after insert."
      
      insertActivity(pan)
      |> stopOnFailure

      let cnt = getActivityCodes() |> List.length
      Expect.equal cnt 1 step1;

    testCase "Desactivate" <| fun _ ->
      let step2 = "We expect to get no activity after desactivation."
      
      desactivateActivity("PAN")
      |> stopOnFailure

      let cnt = getActivityCodes() |> List.length
      Expect.equal cnt 0 step2;

    testCase "Activate" <| fun _ ->
      let step3 = "We expect to get one activity after reactivation."
      
      activateActivity("PAN")
      |> stopOnFailure

      let cnt = getActivityCodes() |> List.length
      Expect.equal cnt 1 step3
      
    testCase "Update" <| fun _ -> 
      let step4 = "We get the updated activity after update."
      
      let pan' = {pan with ExtraInfo = WithInfo}
      
      updateActivity pan'
      |> stopOnFailure

      let expected = Success <| toDBActivity pan'
      let dbwc = getActivity("PAN")

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
      
      insertWorkOrderInfo wo 
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
      
      updateWorkOrderInfo wo'
      |> stopOnFailure

      let expected = Success <| toDBWorkOrderInfo wo'
      let dbwo = getWorkOrder(woCode)

      Expect.equal dbwo expected step4]

[<Tests>]
let testGenerateData =
  let config = { FsCheck.Config.Default with MaxTest = 10000 }   
  
  testList "FsCheck" [

    testProperty "Generate Dummy Data" <| fun a b ->
    
    removeExistingData()
    |> stopOnFailure
      
    //Insert Reference data
    insertReferenceData()
    |> Success
    |> stopOnFailure
  
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