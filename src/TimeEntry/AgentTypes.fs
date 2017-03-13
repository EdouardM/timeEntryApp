namespace TimeEntry

open System
open Microsoft.FSharp.Control
open System.Threading
open System.Threading.Tasks
open System.Collections.Concurrent

[<AutoOpen>]
module AgentTypes =
    
    type IAsyncReplyChannel<'a> =
        abstract Reply : 'a -> unit

    type MailboxReplyChannel<'a>(asyncReplyChannel:AsyncReplyChannel<'a>) =
        interface IAsyncReplyChannel<'a> with
            member x.Reply(msg) = asyncReplyChannel.Reply(msg)

    type ReplyChannel<'a>() = 
         let tcs = new TaskCompletionSource<'a>()
    
         member x.WaitResult =
            async {
                return! tcs.Task |> Async.AwaitTask
            }

         interface IAsyncReplyChannel<'a> with
             member x.Reply(msg) = 
                tcs.SetResult(msg)

    [<AbstractClass>]
    type AgentRef(id:string) =
        member val Id = id with get, set
        abstract Start : unit -> unit


    [<AbstractClass>]
    type AgentRef<'Msg>(id:string) =
        inherit AgentRef(id)
        abstract Receive : unit -> Async<'Msg>
        abstract Post : 'Msg -> unit
        abstract PostAndAsyncReply : (IAsyncReplyChannel<'Reply> -> 'Msg) -> Async<'Reply>
        abstract PostAndTryAsyncReply : (IAsyncReplyChannel<'Reply> -> 'Msg) -> Async<'Reply option>

    type Agent<'a>(id:string, comp, ?token) = 
        inherit AgentRef<'a>(id)
        let mutable agent = Unchecked.defaultof<MailboxProcessor<'a>>
        
        override x.Post(msg:'a) = agent.Post(msg)
        
        override x.PostAndAsyncReply(builder) = agent.PostAndAsyncReply(fun rc -> builder (new MailboxReplyChannel<_>(rc)))

        override x.PostAndTryAsyncReply(builder) = agent.PostAndTryAsyncReply(fun rc -> builder(new MailboxReplyChannel<_>(rc)))

        override x.Receive() = agent.Receive()
        override x.Start() = 
            agent <- MailboxProcessor.Start((fun inbox -> comp (x :> AgentRef<_>)), ?cancellationToken = token)