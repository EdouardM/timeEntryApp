
//Dummy tests

type DBGetWorkOrder = string -> Result<DBWorkOrderEntry>

//DB test is slow => To be put in a separate test project
//Run in case of deployment
let testDBWorkOrder () =
    deleteAllRecords()
    insertWorkOrderEntry wo
    let res = DBGetWorkOrder l l l wo
    match res with
        | Success wi  -> Assert.equals(wi, wo)
        | Failure msg -> Assert.fail  

type StorageWorkOrder = DBGetWorkOrder -> string list -> string list -> string list -> WorkOrder -> Result<WorkOrderEntry>

let storageGetWorkOrder: StorageWorkOrder = 
    fun 
        dBGetWorkOrder
        workorders
        workcenters
        itemcodes
        workOrder -> 
        let (WorkOrder wo) = workOrder
        let res = dBGetWorkOrder wo
        res
        |> bind (fromDBWorkOrderEntry  workorders workcenters itemcodes) 

//Should Fail when Db function fails
let testStorage () = 
    let dbGetWorkOrder wo = Failure ""
    let workorders = []
    let workcenters = []
    let itemcodes = []
    let wo = { xxxx }
    let woRes = storageGetWorkOrder dbGetWorkOrder workorders workcenters itemcodes wo
    match woRes with
        | Success w -> Assert.fail
        | Failure msg -> Assert.equals(msg, "record not found")

////Should Fail when invalid item
let testStorage () = 
    let dbGetWorkOrder wo = Success wo
    let workorders = [wo]
    let workcenters = [wc]
    let itemcodes = [i1; i2; i3]
    //Wrong item 
    let wo = { wo; wc; i4 }
    let woRes = storageGetWorkOrder dbGetWorkOrder workorders workcenters itemcodes wo
    match woRes with
        | Success w -> Assert.fail
        | Failure msg -> Assert.equals(msg, "item not found")

