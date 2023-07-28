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

    let getTransactions accountId =
        transactions
        |> Seq.filter (fun transaction ->
            match transaction.Type with
            | Inflow (fromAccountId, _) -> fromAccountId = accountId
            | Outflow (toAccountId, _) -> toAccountId = accountId
            | Transfer (fromAccountId, toAccountId) ->
                toAccountId = accountId
                || fromAccountId = accountId)

    let updateTransaction transaction =
        if Transaction.isValid transaction then
            let index =
                transactions
                |> Seq.findIndex (fun t -> t.Id = transaction.Id)

            transactions[index] <- transaction
            Ok()
        else
            Error "Invalid transaction"

    do
        let account1 = Account.create "Käyttötili"
        addAccount (account1)
        let account2 = Account.create "Dummy"
        addAccount (account2)

        Transaction.createOutflow account1.Id "Mika" (Money 100m) "test"
        |> addTransaction
        |> Result.mapError (fun error -> failwith error)
        |> ignore

        Transaction.createInflow account1.Id "EB" (Money 100m) "test"
        |> addTransaction
        |> Result.mapError (fun error -> failwith error)
        |> ignore

        Transaction.createTransfer account1.Id account2.Id (Money 50m) "Transfer"
        |> addTransaction
        |> Result.mapError (fun error -> failwith error)
        |> ignore


let budgetApi =
    { getAccounts = fun () -> async { return Storage.accounts |> List.ofSeq }
      getTransactions =
        fun accountId ->
            async { return (accountId, Storage.getTransactions accountId |> List.ofSeq) }
      updateTransaction =
        fun (accountId, transaction) ->
            async {
                Storage.updateTransaction transaction |> ignore
                return (accountId, Storage.getTransactions accountId |> List.ofSeq)
            } }

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