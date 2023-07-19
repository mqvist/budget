module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

module Storage =
    let accounts = ResizeArray()
    let transactions = ResizeArray()

    let addAccount (account: Account) = accounts.Add account

    let addTransaction transaction =
        if Transaction.isValid transaction then
            transactions.Add transaction
            Ok()
        else
            Error "Invalid transaction"

    do
        let account = Account.create "Käyttötili"
        addAccount (account)

        Transaction.createOutflow account.Id (Money 100) "test"
        |> addTransaction
        |> Result.mapError (fun error -> failwith error)
        |> ignore

        Transaction.createInflow account.Id (Money 100) "test"
        |> addTransaction
        |> Result.mapError (fun error -> failwith error)
        |> ignore

let budgetApi =
    { getTransactions = fun () -> async { return Storage.transactions |> List.ofSeq } }

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue budgetApi
    |> Remoting.buildHttpHandler

let app =
    application {
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

[<EntryPoint>]
let main _ =
    run app
    0