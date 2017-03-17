(* FILE SYSTEM WATCHER PLAY AROUND *)
#r "../../packages/FSharp.Data/lib/net40/FSharp.Data.dll" 
open FSharp.Data

#r "../../packages/SQLProvider/lib/FSharp.Data.SqlProvider.dll"
open FSharp.Data.Sql


#load "./Helpers.fs"
#load "./ConstrainedTypes.fs"
#load "./DomainTypes.fs"
#load "./Constructors.fs"
#load "./DBConversions.fs"

#load "./DBCommands.fs"
#load "./DTOTypes.fs"
#load "./DbService.fs"

open TimeEntry.Result
open TimeEntry.Conversions
open TimeEntry.ConstrainedTypes
open TimeEntry.DomainTypes
open TimeEntry.Constructors
open TimeEntry.DBConversions

open TimeEntry.DBCommands
open TimeEntry.DTO
open TimeEntry.DBService

#load "./AgentTypes.fs"
open TimeEntry.AgentTypes

#load "./Agents.fs"
open TimeEntry.Agent

#load "./Etl.fs"
open TimeEntry.Etl

open TimeEntry

open System
open System.IO

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


type DBContext = Sql.dataContext

FSharp.Data.Sql.Common.QueryEvents.SqlQueryEvent |> Event.add (printfn "Executing SQL: %s")


(* CSV FILE TYPES *)
type WorkOrderData = 
    {
        WorkCenter  : string
        WorkOrder   : string
        ItemCode    : string
        Status      : string
    }

module WorkOrderData =  
    let create workcenter workorder itemcode status = 
        { 
            WorkCenter  = workcenter
            WorkOrder   = workorder
            ItemCode    = itemcode
            Status      = status
        }

    let print (wo: WorkOrderData) = 
        printfn "WorkCenter: %s\tWorkOrder: %s\tItemCode: %s\tStatus: %s" 
            wo.WorkCenter wo.WorkOrder wo.ItemCode wo.Status
    
    let fromCSV
        workCenters
        (wo: WorkOrderData) =
            let workOrderRes            = WorkOrder.create wo.WorkOrder
            let workCenterRes           = WorkCenter.validate workCenters wo.WorkCenter
            let itemCodeRes             = ItemCode.create wo.ItemCode
            let statusRes               = WorkOrderStatus.validate wo.Status
            let zeroHr                  = TimeHr 0.f |> Success
            WorkOrderInfo.create 
            <!> workOrderRes
            <*> workCenterRes
            <*> itemCodeRes
            <*> zeroHr
            <*> zeroHr
            <*> statusRes  
            
type WorkOrderCSVFile = CsvProvider<"../../data/sample.csv", Schema="string,string,string,string">
type WorkOrderCSVRow = WorkOrderCSVFile.Row

(* TYPE CONVERSION CSV TO DOMAIN TYPE *)
module WorkOrderCSV  =
    let toWorkOrderData (row: WorkOrderCSVRow) = 
        WorkOrderData.create row.WorkCenter row.WorkOrder row.ItemCode row.Status

    let parseFileContent (input: string) = 
        WorkOrderCSVFile.Parse(input).Rows
        |> Seq.toList
        |> List.map(toWorkOrderData) 


(* DB SERVICE *)
//should go to DBService
let validateWorkOrder (wos: WorkOrderData list)= 
    async { 
        let! workcenters = getActiveWorkCenters()        
        let res = 
                wos
                |> List.mapi(fun i x  -> (i, x) )
                |> List.map(fun (i, wo) -> 
                    WorkOrderData.fromCSV workcenters wo
                    //Add Line number to validation failure message: 
                    |> bimap id (fun msg -> sprintf "[ Line %d ] : " i + msg))
                |> Result.sequence
        return res
    }

let insertOrUpdateWorkOrderList (wos: WorkOrderInfo list) = 
    async {
        return (List.head <!> Result.traverse insertOrUpdateWorkOrder wos)
    }


//FSharp Deep Dive Chapter 8
(* ETL FUNCTION *)
let readFileAsync filepath = 
    async {
        use fs = System.IO.File.OpenRead(filepath)
        use sr = new StreamReader(fs)
        return! sr.ReadToEndAsync() |> Async.AwaitTask
    }

let writeFileAsync filepath (content: string) =
    async {
        use stream = new System.IO.FileStream(filepath,System.IO.FileMode.Create)
        let chars = content |> Seq.toArray 
        let bytes = System.Text.Encoding.UTF8.GetBytes chars
        return! stream.WriteAsync(bytes, 0, bytes.Length ) |> Async.AwaitTask
    }
let parseCsvAsync input = 
    async {
            return WorkOrderCSV.parseFileContent input
    }

let failAsync x = 
    async {
            return  Result.bimap (id) (failwith) x
    }

let work sourceFilePath destFilePath= 
    etl {
        let! extracted = "extractCsv", (readFileAsync sourceFilePath)
        let! parsedCsv = "parseCsv", (parseCsvAsync extracted)
        let! validated = "validate list", (validateWorkOrder parsedCsv)

        //Stops the workflow with Choice2Of2 in cas of validation failure
        let! res = "validation failure", failAsync validated
        let! copy = "write file", (writeFileAsync destFilePath <| "Success" )
        return copy
    }
    |> TimeEntry.Etl.toAsync id (fun name ex -> 
            printfn "Error: %s - %A" name ex.Message)

let path = @"C:\Users\Edouard\Documents\01.Dev\TimeEntryApp\data"

//Test
work  (path + @"\sample.csv") (path + @"\validated.csv") |> Async.RunSynchronously


(* DEFINE AGENTS *)
let fileExtractor = Agent.create "extractCsv" readFileAsync
let parser = Agent.create "parseCsv" parseCsvAsync

let validator = Agent.create "validate list" validateWorkOrder
let insertOrUpdater = Agent.create "insert update Workorder" insertOrUpdateWorkOrderList

let failer: AgentRef<Replyable<Result<_>, Choice<Result<unit>,exn>>> = Agent.create "validation failure" failAsync

let workOrderETL filepath = 
    etl {
        let! extracted = Agent.pipelined fileExtractor filepath
        let! parsedCsv = Agent.pipelined parser extracted
        let! validated = Agent.pipelined validator parsedCsv

        return validated
    }

let worker filepath =
    workOrderETL filepath
    |> TimeEntry.Etl.toAsync id (fun name ex -> 
            Failure <| sprintf "Error: %s - %A" name ex.Message)

//Test
worker (path + @"\sample.csv")   |> Async.RunSynchronously

let dbfailer: AgentRef<Replyable<Result<_>, Choice<Result<unit>,exn>>> = Agent.create "database failure" failAsync

let dBUpdaterETL wos = 
    etl {
        let! dbReply = Agent.pipelined insertOrUpdater wos
        let! res = Agent.pipelined failer dbReply
        return res
    }


type ReadFailure = 
    {
        FilePath   : string
        NbAttempts : int 
    }

type FileReadReply = 
    | FileContent of Result<WorkOrderInfo list>
    | Failures    of ReadFailure list

type FileMessage =
    | NewFile       of string       * AsyncReplyChannel<FileReadReply>
    | Retry         of ReadFailure  * AsyncReplyChannel<FileReadReply>
    | GetFailures   of                AsyncReplyChannel<FileReadReply>

type WorkOrderETL (workOrderETL) =   
    //let readSuccessEvent     = new Event<string>()
    let updateState (readFailures : ReadFailure list) (path, cnt) = 
        ({ FilePath = path; NbAttempts =  cnt})::readFailures
    let successFunc (replyChannel: AsyncReplyChannel<_>) (state: ReadFailure list) res = 
        replyChannel.Reply(FileContent res)
        state
    let failureFunc 
        ( replyChannel   : AsyncReplyChannel<_> ) 
        ( state          : ReadFailure list ) 
        ( filepath       : string )
        ( name           : string ) 
        ( ex             : exn )    = 
                let failure = Failure <| sprintf "Error: %s - %A" name ex.Message
                replyChannel.Reply(FileContent failure)
                updateState state (filepath, 1)
    let computeNewState 
        ( state         : ReadFailure list )
        ( replyChannel  : AsyncReplyChannel<_> )
        ( filepath      : string )  = 
        workOrderETL filepath  
        |> TimeEntry.Etl.toAsync 
            (successFunc replyChannel state)
            (failureFunc replyChannel state filepath)
    let agent = MailboxProcessor.Start(fun inbox -> 

        // the message processing function
        let rec messageLoop state = async {
            
            // read a message
            let! msg = inbox.Receive()

            match msg with 
                | NewFile ( filepath, replyChannel ) -> 
                
                    let! newState = computeNewState state replyChannel filepath
                    return! messageLoop newState
                
                | Retry ( readFailure , replyChannel) ->
                
                    let! newState =  computeNewState state replyChannel readFailure.FilePath
                    return! messageLoop newState

                | GetFailures (replyChannel) -> 
                    replyChannel.Reply (Failures state)
                    return! messageLoop []
            }

        messageLoop []
        )
    
    member x.Post = agent.Post
    member x.PostAndReply : ((AsyncReplyChannel<FileReadReply> -> FileMessage) -> FileReadReply) = agent.PostAndReply

type FailedFilesMessage = FailedFiles of ReadFailure list * AsyncReplyChannel<Result<WorkOrderInfo list>>

type FailedFileReader (fr : WorkOrderETL) = 
    let updateState (failureFilePath : ReadFailure list) (path, cnt) = 
        ({FilePath = path; NbAttempts = cnt} ) ::failureFilePath
    let agent = MailboxProcessor.Start(fun inbox -> 

        // the message processing function
        let rec messageLoop state = async {
            // read a message
            let! msg = inbox.Receive()

            let newState = 
                match msg with 
                    | FailedFiles ([], replyChannel) -> []
                    | FailedFiles (readFailures, replyChannel) -> 
                        readFailures
                        //Check if the file is still there
                        |> List.filter(fun readfailure ->  File.Exists readfailure.FilePath) 
                        |> List.map(fun readfailure -> 
                                //Stop after the third attempt
                                if readfailure.NbAttempts <= 3 then
                                    let reply = fr.PostAndReply(fun (rc:AsyncReplyChannel<FileReadReply>) -> Retry ( readfailure, rc) )
                                    match reply with 
                                        | FileContent res -> replyChannel.Reply(res)
                                        | _ -> ())
                        |> ignore
                        []
            return! messageLoop newState           
            }

        // start the loop 
        messageLoop []
        )
    member x.PostAndReply : ((AsyncReplyChannel<Result<WorkOrderInfo list>> -> FailedFilesMessage) -> Result<WorkOrderInfo list>) = agent.PostAndReply

type ValidatedWOMessage = ValidatedWorkOrders of (WorkOrderInfo list) * AsyncReplyChannel<Result<unit>>

type DBUpdaterETL (dBUpdaterETL) =   
    let successFunc (replyChannel: AsyncReplyChannel<Result<unit>>) res = 
        replyChannel.Reply(res)
        
    let failureFunc 
        ( replyChannel   : AsyncReplyChannel<Result<unit>> ) 
        ( wos            : WorkOrderInfo list )
        ( name           : string ) 
        ( ex             : exn )    = 
                let failure = Failure <| sprintf "Error: %s - %A" name ex.Message
                replyChannel.Reply(failure)
        
    let computeNewState 
        ( replyChannel  : AsyncReplyChannel<_> )
        ( wos           : WorkOrderInfo list )  = 
        dBUpdaterETL wos 
        |> TimeEntry.Etl.retry 3 1000
        |> TimeEntry.Etl.toAsync 
            (successFunc replyChannel)
            (failureFunc replyChannel wos)
    let agent = MailboxProcessor.Start(fun inbox -> 

        // the message processing function
        let rec loop() = async {
            
            // read a message
            let! (ValidatedWorkOrders (wos, replyChannel)) = inbox.Receive()
            
            let! newState = computeNewState replyChannel wos
            
            return! loop ()
            }
        loop ()
        )
    
    member x.Post = agent.Post
    member x.PostAndReply : ((AsyncReplyChannel<Result<unit>> -> ValidatedWOMessage) -> Result<unit>)   = agent.PostAndReply

//Test Agents:
let filepath = path + @"\sample.csv"

let agentWorkOrderETL = WorkOrderETL(workOrderETL)
let agentFailedFile = FailedFileReader(agentWorkOrderETL)
let agentDBUpdaterETL = DBUpdaterETL(dBUpdaterETL)

let reply = agentWorkOrderETL.PostAndReply (fun rc -> NewFile(filepath, rc))

//https://fsharpforfunandprofit.com/posts/concurrency-reactive/
let createTimerObservable timerInterval =
    let timer = new System.Timers.Timer(float timerInterval)

    timer.AutoReset <- true

    let observable = timer.Elapsed
    
    let inifiniteSeq = Seq.unfold(fun state -> Some (state, state + 1)) 0
    let cts = new System.Threading.CancellationTokenSource()

    //Start infinite loop - cancellable 
    Async.Start(async {
            timer.Start()
            let enum = inifiniteSeq.GetEnumerator()
            while enum.MoveNext() && not <| cts.IsCancellationRequested do 
                do! Async.Sleep ( 2 * timerInterval)
            timer.Stop()
        }, cts.Token)

    // return the observable
    observable

let ensureDirPath path = 
    let di = new DirectoryInfo(path)
    di.Create(); di.FullName


[<RequireQualifiedAccess>]
type Trigger = 
    | Timer
    | File of string 
let onCreation = (fun (e:FileSystemEventArgs) ->  
            printfn "File: %s %s" e.FullPath <| e.ChangeType.ToString()
            Trigger.File(e.FullPath))

let onModification = (fun (e:FileSystemEventArgs) ->  
            printfn "File: %s %s" e.FullPath <| e.ChangeType.ToString()
            Trigger.File(e.FullPath))

let onDeletion = (fun (e:FileSystemEventArgs) ->  
            printfn "File: %s %s" e.FullPath <| e.ChangeType.ToString() )
let filewatcher path =
    let watcher = new FileSystemWatcher(ensureDirPath path)
    watcher.EnableRaisingEvents <- true

    let created = watcher.Created |> Observable.map(onCreation)
    let modified = watcher.Changed |> Observable.map(onModification)
    let deleted = watcher.Deleted |> Observable.map(onDeletion)

    [created; modified]
    |> List.reduce Observable.merge 


let timerEventStream = createTimerObservable 60000
let printTick = 
    Observable.subscribe(fun _ -> printfn "tick %A" System.DateTime.Now) timerEventStream

let timer = 
    timerEventStream
    |> Observable.map(fun _ -> Trigger.Timer)

printTick.Dispose()
let tokenSource = new Threading.CancellationTokenSource()
let run comp = Async.StartImmediate(comp, tokenSource.Token)

let reactEvent = 
    function
    | Trigger.Timer      ->  
                            let reply = agentWorkOrderETL.PostAndReply(fun (rc:AsyncReplyChannel<FileReadReply>) -> GetFailures (rc) )
                            match reply with    
                                | Failures failures ->  
                                    result {
                                        let! res = agentFailedFile.PostAndReply(fun (rc:AsyncReplyChannel<Result<WorkOrderInfo list>>) -> FailedFiles (failures, rc))
                                        return! agentDBUpdaterETL.PostAndReply(fun rc-> ValidatedWorkOrders (res, rc))
                                    }
                                    
                                | FileContent res   ->  
                                    result {
                                        let! wos = res
                                        return! agentDBUpdaterETL.PostAndReply(fun rc-> ValidatedWorkOrders (wos, rc))
                                    }

    | Trigger.File filepath ->  
                            let reply = agentWorkOrderETL.PostAndReply (fun (rc:AsyncReplyChannel<FileReadReply>)  -> NewFile ( filepath, rc) )
                            match reply with    
                                | Failures failures ->  Failure "Wrong reply type Failures. FileContent was expected."
                                | FileContent res   ->  
                                    result {
                                        let! wos = res
                                        return! agentDBUpdaterETL.PostAndReply(fun rc-> ValidatedWorkOrders (wos, rc))
                                    }
let triggers = [
    filewatcher path
    timer
]
let obs =  triggers |> List.reduce Observable.merge
let sub1 = obs |> Observable.subscribe (reactEvent >> (printfn "%A") >> ignore)

//Terminates Async process of event: React Event
tokenSource.Cancel()

// Have subscriber unsubscribe: no more listening to filewatcher
sub1.Dispose()
