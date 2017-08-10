module Monad

type MaybeBuilder() =
    member t.Bind(a:'a option,f:'a->('b option)) = a |> Option.bind(fun x -> f x)
    member t.Bind(a:System.Threading.Tasks.Task<'a>,f:'a->'b option) = 
        async {
                let! b = a |> Async.AwaitTask
                return f b
            } |> Async.RunSynchronously
    member t.Bind(a:System.Threading.Tasks.Task,f:unit->'b option) = 
        async {
                do! a |> Async.AwaitTask
                return f()
            } |> Async.RunSynchronously
    member t.Bind(a:Async<'a>,f:'a->'b option) =
            async {
                let! b = a
                return f b
            } |> Async.RunSynchronously
    member t.Return(a:'a) = if obj.ReferenceEquals(a,null) then None else Some a
    member t.ReturnFrom(x:'a option) = x

/// Beefed up maybe monad with helper functions in ModelLibrary/Utils.fs.
/// Returns an option: it is intended that you subsequently pipe to defArg if required.
/// It is also intended that you use helper functions to catch exceptions
/// and convert them to an option type before putting them in the computation expression.
let maybe = MaybeBuilder()