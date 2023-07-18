namespace Shared

open System

type Todo = { Id: Guid; Description: string }

type Money = decimal
type Account = { Id: Guid; Name: string }
type Outflow = { FromAccount: Guid }
type Inflow = { ToAccount: Guid }
type Transfer = { FromAccount: Guid; ToAccount: Guid }

type TransactionType =
    | Outflow
    | Inflow
    | Transfer

type Transaction =
    { Id: Guid
      Amount: Money
      Type: TransactionType
      Comment: string }

module Todo =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not

    let create (description: string) =
        { Id = Guid.NewGuid()
          Description = description }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type ITodosApi =
    { getTodos: unit -> Async<Todo list>
      addTodo: Todo -> Async<Todo> }