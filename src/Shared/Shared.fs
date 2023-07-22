namespace Shared

open System

type Money = decimal
type AccountId = Guid
type Account = { Id: AccountId; Name: string }

type TransactionType =
    | Outflow of fromAccountId: Guid * payee: string
    | Inflow of toAccountId: Guid * payee: string
    | Transfer of fromAccountId: Guid * toAccountId: Guid

type Transaction =
    { Id: Guid
      Date: DateOnly
      Amount: Money
      Type: TransactionType
      Comment: string }

module Account =
    let create (name: string) = { Id = Guid.NewGuid(); Name = name }

module Transaction =
    let isValid t =
        (t.Amount = Money 0
         || String.IsNullOrWhiteSpace t.Comment)
        |> not

    let createOutflow fromAccountId payee amount comment =
        { Id = Guid.NewGuid()
          Date = DateOnly.FromDateTime(DateTime.Now)
          Amount = amount
          Type = Outflow(fromAccountId, payee)
          Comment = comment }

    let createInflow toAccountId payee amount comment =
        { Id = Guid.NewGuid()
          Date = DateOnly.FromDateTime(DateTime.Now)
          Amount = amount
          Type = Inflow(toAccountId, payee)
          Comment = comment }

    let createTransfer fromAccountId toAccountId amount comment =
        { Id = Guid.NewGuid()
          Date = DateOnly.FromDateTime(DateTime.Now)
          Amount = amount
          Type = Transfer(fromAccountId, toAccountId)
          Comment = comment }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IBudgetApi =
    { getAccounts: unit -> Async<Account list>
      getTransactions: AccountId -> Async<AccountId * Transaction list> }