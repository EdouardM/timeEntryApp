(* FILE SYSTEM WATCHER PLAY AROUND *)


#r "../../packages/FSharp.Data/lib/net40/FSharp.Data.dll" 
open FSharp.Data

#load "./Helpers.fs"
#load "./ConstrainedTypes.fs"
#load "./DomainTypes.fs"
#load "./Constructors.fs"
open TimeEntry.Result
open TimeEntry.Conversions
open TimeEntry.ConstrainedTypes
open TimeEntry.DomainTypes
open TimeEntry.Constructors



#load "./Schedule.fs"
open FSharpDeepDives.Schedule
open System
open System.IO

let choiceToRes (input: Choice<'T,exn>) = 
    match input with
        | Choice1Of2 x -> Success x
        | Choice2Of2 ex -> Failure ex.Message

let path = @"C:\Users\Edouard\Documents\01.Dev\TimeEntryApp\data"
let ensureDirPath path = 
    let di = new DirectoryInfo(path)
    di.Create(); di.FullName

let ensureFilePath filepath = 
    if File.Exists(filepath) then 
        Success filepath 
    else 
        Failure <| sprintf "File not found at path: %s" filepath   

let readFileAsync filepath = 
    async {
        use fs = System.IO.File.OpenRead(path)
        use sr = new StreamReader(fs)
        return! sr.ReadToEndAsync() |> Async.AwaitTask
    }

type FileMessage =
    | FilePath of string
    | GetFailures of AsyncReplyChannel<string list> 


type FileReader () = 
    static let readSuccessEvent     = new Event<string>()
    static let updateState (failureFilePath : string list) path = 
        path::failureFilePath
    let reportFileRead = 
        function
        | Success input -> readSuccessEvent.Trigger(input)
        | Failure msg   -> printfn "Failure: %s" msg

    static let agent = MailboxProcessor.Start(fun inbox -> 

        // the message processing function
        let rec messageLoop state = async {

            // read a message
            let! msg = inbox.Receive()

            match msg with 
                | FilePath filepath -> 
                    // do the core logic
                    let! txt =  
                        readFileAsync path 
                        |> Async.Catch
                        
                    let newState  =
                        match txt with
                        | Choice1Of2 input ->  
                                            readSuccessEvent.Trigger(input)
                                            state
                        | Choice2Of2 ex -> 
                                            printfn "Failure: %s" ex.Message
                                            updateState state filepath

                    return! messageLoop newState 
                | GetFailures reply -> 
                    reply.Reply (state)
                    return! messageLoop []
            }

        // start the loop 
        messageLoop []
        )
    static member Read input = agent.Post input



module WorkOrderData = 
    type WorkOrderData = 
        {
            Site        : string
            ShopFloor   : string
            WorkCenter  : string
            WorkOrder   : string
            ItemCode    : string
            Status      : string
        }
    
    let create site shopfloor workcenter workorder itemcode status = 
        { 
            Site        = site
            ShopFloor   = shopfloor
            WorkCenter  = workcenter
            WorkOrder   = workorder
            ItemCode    = itemcode
            Status      = status
        }

    let print (wo: WorkOrderData) = 
        printfn "Site:  %s\tShopFloor: %s\tWorkCenter: %s\tWorkOrder: %s\tItemCode: %s\tStatus: %s"
            wo.Site wo.ShopFloor wo.WorkCenter wo.WorkOrder wo.ItemCode wo.Status
    let fromDB
        workOrders
        workCenters
        itemCodes
        (wo: WorkOrderData) =
            let workOrderRes            = WorkOrder.validate workOrders wo.WorkOrder
            let workCenterRes           = WorkCenter.validate workCenters wo.WorkCenter
            let itemCodeRes             = ItemCode.validate itemCodes wo.ItemCode
            let statusRes               = WorkOrderStatus.validate wo.Status
            let zeroHr                  = TimeHr 0.f |> Success
            WorkOrderInfo.validate 
            <!> workOrderRes
            <*> workCenterRes
            <*> itemCodeRes
            <*> zeroHr
            <*> zeroHr
            <*> statusRes  

type WorkOrderCSVFile = CsvProvider<"../../data/sample.csv", Schema="string,string,string,string,string,string">

type WorkOrderCSVRow = WorkOrderCSVFile.Row

let parseWorkOrderCSV (row: WorkOrderCSVRow) = 
    WorkOrderData.create row.Site row.ShopFloor row.WorkCenter 
                        row.WorkOrder row.ItemCode row.Status

let parseData (input: string) = 
    async {
        let rows = WorkOrderCSVFile.Parse(input).Rows
        return
            rows
            |> Seq.toList
            |> List.map(parseWorkOrderCSV) 
    }


type WorkOrderCSVParser () = 
    static let updateState (workorders: WorkOrderData.WorkOrderData list) msg = 

        // increment the counters and...
        let newwo = parseWorkOrderCSV msg
        WorkOrderData.print newwo
        // return the new state
        newwo::workorders

    static let agent = MailboxProcessor.Start(fun inbox -> 

        // the message processing function
        let rec messageLoop oldState = async{

            // read a message
            let! msg = inbox.Receive()

            // do the core logic
            let newState = updateState oldState msg

            // loop to top
            return! messageLoop newState 
            }

        // start the loop 
        messageLoop []
        )
    static member Parse input = agent.Post input

let row = 
    async {
        let! txt = readFileAsync (path + "\sample.csv") 
        let row =  WorkOrderCSVFile.Parse(txt).Rows |> Seq.head
        return row 
    }
    |> Async.RunSynchronously

WorkOrderCSVParser.Parse row


[<RequireQualifiedAccess>]
type Trigger = 
    | Cron
    | File of string 
    | None



let onCreation = (fun (e:FileSystemEventArgs) ->  
            printfn "File: %s %s" e.FullPath <| e.ChangeType.ToString()
            Trigger.File(e.FullPath))

let onDeletion = (fun (e:FileSystemEventArgs) ->  
            printfn "File: %s %s" e.FullPath <| e.ChangeType.ToString()
            Trigger.None)
let filewatcher path =
    let watcher = new FileSystemWatcher(ensurePath path)
    watcher.EnableRaisingEvents <- true

    let created = watcher.Created |> Observable.map(onCreation)
    let deleted = watcher.Deleted |> Observable.map(onDeletion)

    [created; deleted]
    |> List.reduce Observable.merge 

let cronExp = "0 0/1 * 1/1 * ? *"
let cron cronExp = 
    toObservable cronExp
    |> Observable.map(fun _ -> Trigger.Cron)

//Create a new file system watcher     
let tokenSource = new Threading.CancellationTokenSource()
let run comp = Async.StartImmediate(comp, tokenSource.Token)
let reactEvent = 
    let failedAttempts = ref []
    function
    | Trigger.Cron      -> 
        async { 
            do printfn "CRON generated" 
            match !failedAttempts with
                | [] -> return ()
                | p::ps -> 
                        let! txt = 
                            readFileAsync path |> Async.Catch
                        match txt with
                            | Choice1Of2 t ->  
                                                do printfn "%s" t
                                                failedAttempts := ps
                            | Choice2Of2 ex -> 
                                                do printfn "%s" ex.Message
                                                
        }
    | Trigger.File path -> 
        async {
            let! txt = 
                    readFileAsync path                                     
        }
    | Trigger.None      -> async { return () }

let triggers = [
    filewatcher path
    cron cronExp
]

let obs =  triggers |> List.reduce Observable.merge

let sub1 = obs |> Observable.subscribe (reactEvent >> run)

//Terminates Async process of event: React Event
tokenSource.Cancel()

// Have subscriber unsubscribe: no more listening to filewatcher
sub1.Dispose()
