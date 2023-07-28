namespace Shared

open System

type Money =
    | Money of decimal
    override this.ToString() = $"%.2f{this |> fun (Money m) -> m}€"

type AccountId = AccountId of Guid
type TransactionId = TransactionId of Guid

[<NoEquality; NoComparison>]
type Account = { Id: AccountId; Name: string }

type TransactionType =
    | Outflow of fromAccountId: AccountId * payee: string
    | Inflow of toAccountId: AccountId * payee: string
    | Transfer of fromAccountId: AccountId * toAccountId: AccountId

[<NoEquality; NoComparison>]
type Transaction =
    { Id: TransactionId
      Date: DateTime
      Amount: Money
      Type: TransactionType
      Comment: string }

module Account =
    let create (name: string) =
        { Id = AccountId(Guid.NewGuid())
          Name = name }

module Transaction =
    let isValid t = (t.Amount = Money 0m) |> not

    let createOutflow fromAccountId payee amount comment =
        { Id = TransactionId(Guid.NewGuid())
          Date = DateTime.Today
          Amount = amount
          Type = Outflow(fromAccountId, payee)
          Comment = comment }

    let createInflow toAccountId payee amount comment =
        { Id = TransactionId(Guid.NewGuid())
          Date = DateTime.Today
          Amount = amount
          Type = Inflow(toAccountId, payee)
          Comment = comment }

    let createTransfer fromAccountId toAccountId amount comment =
        { Id = TransactionId(Guid.NewGuid())
          Date = DateTime.Today
          Amount = amount
          Type = Transfer(fromAccountId, toAccountId)
          Comment = comment }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IBudgetApi =
    { getAccounts: unit -> Async<Account list>
      getTransactions: AccountId -> Async<AccountId * Transaction list>
      updateTransaction: AccountId * Transaction -> Async<AccountId * Transaction list> }