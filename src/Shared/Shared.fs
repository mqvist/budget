namespace Shared

open System

type Money = decimal
type Account = { Id: Guid; Name: string }

type TransactionType =
    | Outflow of fromAccount: Guid
    | Inflow of toAccount: Guid
    | Transfer of fromAccount: Guid * toAccount: Guid

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

    let createOutflow fromAccount amount comment =
        { Id = Guid.NewGuid()
          Date = DateOnly.FromDateTime(DateTime.Now)
          Amount = amount
          Type = Outflow(fromAccount)
          Comment = comment }

    let createInflow toAccount amount comment =
        { Id = Guid.NewGuid()
          Date = DateOnly.FromDateTime(DateTime.Now)
          Amount = amount
          Type = Inflow(toAccount)
          Comment = comment }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IBudgetApi =
    { getTransactions: unit -> Async<Transaction list> }