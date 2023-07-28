module Index

open Elmish
open Fable.Remoting.Client
open Fable.Core.JsInterop
open Shared

importAll "./css/style.css"

type ActiveTransactionState =
    | None
    | Selected of Transaction
    | Editing of Transaction

type ActiveAccountInfo =
    { Accounts: Account list
      ActiveAccount: Account
      Transactions: Transaction list
      ActiveTransaction: ActiveTransactionState }

    member this.getAccount accountId =
        this.Accounts
        |> List.find (fun acct -> acct.Id = accountId)

type Model =
    | NoAccounts
    | AccountsLoaded of Account list
    | ViewActiveAccount of ActiveAccountInfo

type Msg =
    | GotAccounts of Account list
    | SelectActiveAccount of AccountId
    | GotTransactions of AccountId * Transaction list
    | SelectActiveTransaction of Transaction
    | EditActiveTransaction of Transaction
    | FinishEditing
    | CancelEditing

let budgetApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IBudgetApi>

let init () : Model * Cmd<Msg> =
    let model = NoAccounts
    let cmd = Cmd.OfAsync.perform budgetApi.getAccounts () GotAccounts
    model, cmd

let update msg model : Model * Cmd<Msg> =
    match msg with
    | GotAccounts accounts when accounts.IsEmpty -> NoAccounts, Cmd.none
    | GotAccounts accounts ->
        let accountId = accounts.Head.Id
        let cmd = Cmd.OfAsync.perform budgetApi.getTransactions accountId GotTransactions

        AccountsLoaded accounts, cmd
    | SelectActiveAccount accountId ->
        let cmd = Cmd.OfAsync.perform budgetApi.getTransactions accountId GotTransactions
        model, cmd
    | GotTransactions (accountId, transactions) ->
        match model with
        | NoAccounts -> failwith "Got transactions but have no accounts"
        | AccountsLoaded accounts ->
            ViewActiveAccount
                { Accounts = accounts
                  ActiveAccount =
                    accounts
                    |> Seq.find (fun acct -> acct.Id = accountId)
                  Transactions = transactions
                  ActiveTransaction = None },
            Cmd.none
        | ViewActiveAccount info ->
            ViewActiveAccount
                { info with
                    ActiveAccount =
                        info.Accounts
                        |> Seq.find (fun acct -> acct.Id = accountId)
                    Transactions = transactions },
            Cmd.none

    | SelectActiveTransaction transaction ->
        match model with
        | ViewActiveAccount info ->
            ViewActiveAccount { info with ActiveTransaction = Selected transaction }, Cmd.none
        | _ -> failwith "Invalid state"

    | EditActiveTransaction transaction ->
        match model with
        | ViewActiveAccount info ->
            ViewActiveAccount { info with ActiveTransaction = Editing transaction }, Cmd.none
        | _ -> failwith "Invalid state"

    | FinishEditing _ ->
        match model with
        | ViewActiveAccount info ->
            match info.ActiveTransaction with
            | Editing transaction ->
                let cmd =
                    Cmd.OfAsync.perform
                        budgetApi.updateTransaction
                        (info.ActiveAccount.Id, transaction)
                        GotTransactions

                ViewActiveAccount { info with ActiveTransaction = Selected transaction }, cmd
            | _ -> failwith "Invalid state"
        | _ -> failwith "Invalid state"

    | CancelEditing ->
        match model with
        | ViewActiveAccount info ->
            match info.ActiveTransaction with
            | Editing transaction ->
                let origTransaction =
                    info.Transactions
                    |> Seq.find (fun t -> t.Id = transaction.Id)

                ViewActiveAccount { info with ActiveTransaction = Selected origTransaction },
                Cmd.none
            | _ -> failwith "Invalid state"
        | _ -> failwith "Invalid state"


open Feliz
open Feliz.Bulma
open System

let formatDate (date: DateOnly) =
    $"%02d{date.Day}.%02d{date.Month}.{date.Year}"

[<ReactComponent>]
let EditableTd (value: string) dispatch =
    let (editing, enableEditing) = React.useState (false)
    let (value, setValue) = React.useState (value)

    Html.td [
        if not editing then
            prop.onClick (fun _ -> enableEditing true)
            prop.text value
        else
            prop.children [
                Html.input [
                    // Bulma.input.isFocused
                    prop.autoFocus true
                    prop.onChange (fun v -> setValue v)
                    prop.onBlur (fun _ ->
                        // dispatch SetTransactionPayee value
                        enableEditing false)
                    prop.value value
                ]
            ]
    ]

let renderTransaction info transaction =
    [ Html.td (formatDate transaction.Date)
      match transaction.Type with
      | Inflow (_, payee)
      | Outflow (_, payee) -> Html.td payee
      | Transfer (fromAccountId, toAccountId) ->
          if info.ActiveAccount.Id = fromAccountId then
              let toAccount = info.getAccount toAccountId
              Html.td $"Transfer to {toAccount.Name}"
          else
              let fromAccount = info.getAccount fromAccountId
              Html.td $"Transfer from {fromAccount.Name}"

      Html.td "category"
      Html.td transaction.Comment

      match transaction.Type with
      | Inflow _ ->
          Html.td ""
          Html.td (transaction.Amount.ToString())
      | Outflow _ ->
          Html.td (transaction.Amount.ToString())
          Html.td ""
      | Transfer (fromAccountId, _) when fromAccountId = info.ActiveAccount.Id ->
          Html.td (transaction.Amount.ToString())
          Html.td ""
      | Transfer (_, toAccountId) when toAccountId = info.ActiveAccount.Id ->
          Html.td ""
          Html.td (transaction.Amount.ToString())
      | Transfer _ -> failwith "Not Implemented" ]

let renderTransactionEditor info transaction dispatch =
    let submitChange f =
        prop.onChange (fun (text: string) -> f text |> EditActiveTransaction |> dispatch)

    let finishWithEnter =
        prop.onKeyPress (fun event ->
            if event.charCode = 13.0 then
                dispatch FinishEditing)

    [ Html.td (formatDate transaction.Date)
      Html.td [
          prop.children [
              Html.input [
                  match transaction.Type with
                  | Inflow (accountId, payee) ->
                      prop.value payee
                      submitChange (fun text -> { transaction with Type = Inflow(accountId, text) })
                      finishWithEnter
                  | Outflow (accountId, payee) ->
                      prop.value payee
                      submitChange (fun text -> { transaction with Type = Outflow(accountId, text) })
                      finishWithEnter

                  | Transfer (fromAccountId, toAccountId) ->
                      if info.ActiveAccount.Id = fromAccountId then
                          let toAccount = info.getAccount toAccountId
                          prop.defaultValue $"Transfer to {toAccount.Name}"
                      else
                          let fromAccount = info.getAccount fromAccountId
                          prop.defaultValue $"Transfer from {fromAccount.Name}"
              ]
          ]
      ]

      Html.td "category"
      Html.td [
          prop.children [
              Html.input [
                  prop.defaultValue transaction.Comment
                  submitChange (fun text -> { transaction with Comment = text })
                  finishWithEnter
              ]
          ]
      ]

      match transaction.Type with
      | Inflow _ ->
          Html.td ""
          Html.td (transaction.Amount.ToString())
      | Outflow _ ->
          Html.td (transaction.Amount.ToString())
          Html.td ""
      | Transfer (fromAccountId, _) when fromAccountId = info.ActiveAccount.Id ->
          Html.td (transaction.Amount.ToString())
          Html.td ""
      | Transfer (_, toAccountId) when toAccountId = info.ActiveAccount.Id ->
          Html.td ""
          Html.td (transaction.Amount.ToString())
      | Transfer _ -> failwith "Not Implemented" ]

let renderTransactionEditButtons transaction dispatch =
    Html.td [
        // Use relative positioning to anchor the absolute buttons
        prop.classes [ "relative"; "h-8" ]
        prop.colSpan 100
        prop.children [
            Bulma.button.button [
                button.isSmall
                color.isPrimary
                prop.classes [
                    "absolute right-16 top-1"
                    "h-6 w-20"
                ]
                prop.text "Save"
                prop.onClick (fun ev ->
                    // ev.stopPropagation ()
                    dispatch FinishEditing)
            ]

            Bulma.button.button [
                button.isSmall
                button.isOutlined
                color.isInfo

                prop.classes [
                    "absolute right-40 top-1"
                    "h-6 w-20"
                ]
                prop.text "Cancel"
                prop.onClick (fun ev ->
                    // ev.stopPropagation ()
                    dispatch CancelEditing)
            ]
        ]
    ]

let renderTransactions info dispatch =
    Html.tbody [
        prop.children [
            for transaction in info.Transactions do
                match info.ActiveTransaction with
                | Selected t when transaction.Id = t.Id ->
                    Html.tr [
                        prop.classes [
                            "leading-none bg-blue-100"
                        ]

                        prop.onMouseUp (fun ev ->
                            ev.stopPropagation ()
                            dispatch (EditActiveTransaction t))

                        prop.children (renderTransaction info transaction)
                    ]

                | Editing t when transaction.Id = t.Id ->
                    Html.tr [
                        prop.classes [
                            "leading-none"
                            "bg-blue-100"
                        ]
                        prop.children (renderTransactionEditor info t dispatch)
                    ]

                    Html.tr [
                        prop.classes [
                            "leading-none"
                            "bg-blue-100"
                        ]
                        prop.children [
                            renderTransactionEditButtons t dispatch
                        ]
                    ]

                | _ ->
                    Html.tr [
                        prop.className "leading-none"
                        prop.onMouseUp (fun ev ->
                            // ev.stopPropagation ()
                            dispatch (SelectActiveTransaction transaction))
                        prop.children (renderTransaction info transaction)
                    ]
        ]
    ]

let renderMainView model dispatch =
    Bulma.column [
        match model with
        | NoAccounts -> Bulma.title "No accounts"
        | AccountsLoaded _ -> Html.none
        | ViewActiveAccount info ->
            Bulma.block [
                Bulma.title info.ActiveAccount.Name
                Bulma.tableContainer [
                    Bulma.table [
                        table.isFullWidth
                        prop.children [
                            Html.thead [
                                Html.tr [
                                    Html.th "Date"
                                    Html.th "Payee"
                                    Html.th "Category"
                                    Html.th "Comment"
                                    Html.th "Outflow"
                                    Html.th "Inflow"
                                ]
                            ]
                            renderTransactions info dispatch
                        ]
                    ]
                ]
            ]
    ]

let renderAccountList model dispatch =
    Bulma.column [
        column.isNarrow
        color.hasTextWhite
        prop.classes [ "w-52 m-2.5 bg-sky-800" ]
        prop.children [
            Html.strong [
                color.hasTextWhite
                prop.text "Accounts"
            ]

            match model with
            | NoAccounts
            | AccountsLoaded _ -> Html.none
            | ViewActiveAccount info ->
                Html.ul [
                    for account in info.Accounts do
                        Html.li [
                            prop.text account.Name
                            if account.Id = info.ActiveAccount.Id then
                                prop.classes [
                                    "rounded-md px-4 py-1 bg-sky-600"
                                ]

                            else
                                prop.classes [
                                    "rounded-md px-4 py-1"
                                    "hover:bg-sky-900 hover:cursor-pointer"
                                ]

                                prop.onClick (fun ev -> SelectActiveAccount account.Id |> dispatch)
                        ]

                    ]
        ]
    ]

let view model dispatch =
    Bulma.columns [
        prop.style [
            style.height (length.vh 100)
        ]
        prop.children [
            renderAccountList model dispatch
            renderMainView model dispatch
        ]
    ]