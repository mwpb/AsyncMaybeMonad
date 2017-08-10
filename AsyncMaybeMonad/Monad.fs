module Monad

type MaybeBuilder() =
    member t.Bind(a:'a option, f:'a->'b option) = a |> Option.bind(fun x -> f x)
    member t.Bind(a:'a option, f:'a->Async<'b>) =
        match a with
        | Some x ->
            async {
                let! b = f x
                return Some b}
        | None -> async {return None}
    member t.Bind(a:Async<'a>,f:'a->'b option) =
        async { let! b = a
                return f b}
    member t.Bind(a:Async<'a>,f:'a->Async<'b>) =
        async{  let! b = a
                let! c = f b
                return c}
    member t.Bind(a:System.Threading.Tasks.Task<'a>,f:'a->'b option) = 
        async { let! b = a |> Async.AwaitTask
                return f b}
    member t.Bind(a:System.Threading.Tasks.Task<'a>,f:'a->Async<'b>) = 
        async { let! b = a |> Async.AwaitTask
                let! c = f b
                return c}
    member t.Bind(a:System.Threading.Tasks.Task,f:unit->'b option) = 
        async { do! a |> Async.AwaitTask
                return f()}
    member t.Bind(a:System.Threading.Tasks.Task,f:unit->Async<'b>) = 
        async { do! a |> Async.AwaitTask
                return! f()}

    member t.Return(a:'a) = if obj.ReferenceEquals(a,null) then None else Some a
    member t.ReturnFrom(x:'a option) = x
    member t.ReturnFrom(x:Async<'a>) = x

let maybe = MaybeBuilder()

let defarg (defaultArgument:'a) (x:'a option) = defaultArg x defaultArgument

