module Index

open Elmish
open Fable.Remoting.Client
open Fable.Core.JsInterop
open Shared

importAll "./css/tailwind.css"

type ActiveAccountInfo =
    { Accounts: Account list
      ActiveAccount: Account
      Transactions: Transaction list
      ActiveTransaction: Transaction option }

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

let budgetApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IBudgetApi>

let init () : Model * Cmd<Msg> =
    let model = NoAccounts

    let cmd = Cmd.OfAsync.perform budgetApi.getAccounts () GotAccounts
    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match model, msg with
    | _, GotAccounts accounts when accounts.IsEmpty -> NoAccounts, Cmd.none
    | _, GotAccounts accounts ->
        let accountId = accounts.Head.Id
        let cmd = Cmd.OfAsync.perform budgetApi.getTransactions accountId GotTransactions

        AccountsLoaded accounts, cmd
    | _, SelectActiveAccount accountId ->
        let cmd = Cmd.OfAsync.perform budgetApi.getTransactions accountId GotTransactions
        model, cmd
    | NoAccounts, GotTransactions _ -> failwith "Got transactions but there are no accounts"
    | AccountsLoaded accounts, GotTransactions (accountId, transactions)
    | ViewActiveAccount { Accounts = accounts }, GotTransactions (accountId, transactions) ->
        ViewActiveAccount
            { Accounts = accounts
              ActiveAccount =
                accounts
                |> Seq.find (fun acct -> acct.Id = accountId)
              Transactions = transactions
              ActiveTransaction = None },
        Cmd.none
    | ViewActiveAccount info, SelectActiveTransaction transaction ->
        ViewActiveAccount { info with ActiveTransaction = Some(transaction) }, Cmd.none
    | _, SelectActiveTransaction transaction -> failwith "Cannot select active transaction"


open Feliz
open Feliz.Bulma
open System

let backgroundColor = "#2C4461"
let darkBackgroundColor = "#29374D"
let lightBackgroundColor = "#4673A3"
let activeTransactionColor = "#DBE8F6"

let navBrand =
    Bulma.navbarBrand.div [
        Bulma.navbarItem.a [
            prop.href "https://safe-stack.github.io/"
            navbarItem.isActive
            prop.children [
                Html.img [
                    prop.src "/favicon.png"
                    prop.alt "Logo"
                ]
            ]
        ]
    ]

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

let viewActiveAccount model (dispatch: Msg -> unit) =
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
                        Html.tbody [
                            for transaction in info.Transactions do
                                Html.tr [
                                    prop.onClick (fun _ -> SelectActiveTransaction transaction |> dispatch)
                                    // Highlight active transaction
                                    if Some transaction = info.ActiveTransaction then
                                        prop.classes [
                                            $"bg-[{activeTransactionColor}]"
                                        ]
                                    prop.children [
                                        Html.td (formatDate transaction.Date)
                                        match transaction.Type with
                                        | Inflow (_, payee)
                                        | Outflow (_, payee) -> EditableTd payee dispatch
                                        | Transfer (fromAccountId, toAccountId) ->
                                            if info.ActiveAccount.Id = fromAccountId then
                                                let toAccount = info.getAccount toAccountId
                                                Html.td $"Transfer to {toAccount.Name}"
                                            else
                                                let fromAccount = info.getAccount fromAccountId
                                                Html.td $"Transfer from {fromAccount.Name}"

                                        Html.td "category"
                                        EditableTd transaction.Comment dispatch

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
                                        | Transfer _ -> failwith "Not Implemented"
                                    ]
                                ]
                        ]
                    ]
                ]
            ]
        ]

let renderAccounts info dispatch =
    Html.ul [
        for account in info.Accounts do
            Html.li [
                prop.text account.Name
                if account = info.ActiveAccount then
                    prop.classes [ "rounded-md p-1 pl-5" ]

                    prop.style [
                        style.backgroundColor lightBackgroundColor
                    ]
                else
                    prop.classes [
                        "rounded-md p-1 pl-5"
                        $"hover:bg-[{darkBackgroundColor}] hover:cursor-pointer"
                    ]

                    prop.onClick (fun ev -> SelectActiveAccount account.Id |> dispatch)
            ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.columns [
        prop.style [
            style.height (length.vh 100)
        ]
        prop.children [
            Bulma.column [
                column.isNarrow
                prop.style [
                    style.width (length.px 200)
                    style.backgroundColor backgroundColor
                ]
                prop.children [
                    Bulma.column [
                        color.hasTextWhite
                        prop.children [
                            Html.strong [
                                color.hasTextWhite
                                prop.text "Accounts"
                            ]
                            match model with
                            | NoAccounts -> Html.p "Create account"
                            | AccountsLoaded _ -> Html.none
                            | ViewActiveAccount info -> renderAccounts info dispatch
                        ]
                    ]
                ]
            ]
            Bulma.column [
                prop.children [
                    viewActiveAccount model dispatch
                ]
            ]
        ]
    ]