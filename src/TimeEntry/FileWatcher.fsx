(* FILE SYSTEM WATCHER PLAY AROUND *)

#load "./Schedule.fs"
open FSharpDeepDives.Schedule

open System
open System.IO

let path = @"C:\Users\Edouard\Documents\01.Dev\TimeEntryApp\data"

[<RequireQualifiedAccess>]
type Trigger = 
    | Cron
    | File of string 
    | None

let ensurePath path = 
    let di = new DirectoryInfo(path)
    di.Create(); di.FullName

let onCreation = (fun (e:FileSystemEventArgs) ->  
            printfn "File: %s %s" e.FullPath <| e.ChangeType.ToString()
            Trigger.File(e.FullPath))

let onDeletion = (fun (e:FileSystemEventArgs) ->  
            printfn "File: %s %s" e.FullPath <| e.ChangeType.ToString()
            Trigger.None)
let file path =
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
let readFileAsync path = 
    async {
        use fs = System.IO.File.OpenRead(path)
        use sr = new StreamReader(fs)
        return! sr.ReadToEndAsync() |> Async.AwaitTask
    }
let txt = readFileAsync <| path + "\ee.txt"
printfn "%s" <| (Async.RunSynchronously txt)


let tokenSource = new Threading.CancellationTokenSource()
let run comp = Async.StartImmediate(comp, tokenSource.Token)
let reactEvent = 
    function
    | Trigger.Cron      -> async { do printfn "CRON generated"}
    | Trigger.File path -> 
        async {
            let! txt = readFileAsync path |> Async.Catch
            match txt with
                | Choice1Of2 t ->  do printfn "%s" t
                | Choice2Of2 ex -> do printfn "%s" ex.Message
        }
    | Trigger.None      -> async { return () }

let triggers = [
    file path
    cron cronExp
]

triggers
|> List.reduce Observable.merge
|> Observable.add (reactEvent >> run)

//Terminates process
tokenSource.Cancel()
