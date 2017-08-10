module Monad

type MaybeBuilder() =
    // If we are binding an option.
    member t.Bind(a:'a option, f:'a->'b option) = a |> Option.bind(fun x -> f x)
    member t.Bind(a:'a option, f:'a->Async<'b>) =
        match a with
        | Some x ->
            async {
                let! b = f x
                return Some b}
        | None -> async {return None}
    // If we are binding an Async
    member t.Bind(a:Async<'a>,f:'a->'b option) =
        async { let! b = a
                return f b}
    member t.Bind(a:Async<'a>,f:'a->Async<'b>) =
        async{  let! b = a
                let! c = f b
                return c}
    // If we are binding a Task that returns a value
    member t.Bind(a:System.Threading.Tasks.Task<'a>,f:'a->'b option) = 
        async { let! b = a |> Async.AwaitTask
                return f b}
    member t.Bind(a:System.Threading.Tasks.Task<'a>,f:'a->Async<'b>) = 
        async { let! b = a |> Async.AwaitTask
                let! c = f b
                return c}
    // If we are binding a Task that doesn't return a value
    member t.Bind(a:System.Threading.Tasks.Task,f:unit->'b option) = 
        async { do! a |> Async.AwaitTask
                return f()}
    member t.Bind(a:System.Threading.Tasks.Task,f:unit->Async<'b>) = 
        async { do! a |> Async.AwaitTask
                return! f()}

    member t.Return(a:'a) = if obj.ReferenceEquals(a,null) then None else Some a
    member t.ReturnFrom(x:'a option) = x
    member t.ReturnFrom(x:Async<'a>) = x

/// Beefed up maybe monad with helper functions in ModelLibrary/Utils.fs.
/// It returns an option or Async<option> and it is intended that you 
/// pipe into defArg if required.
/// It is also intended that you use helper functions to catch exceptions
/// and convert them to an option type before putting them in the computation expression.
let maybe = MaybeBuilder()