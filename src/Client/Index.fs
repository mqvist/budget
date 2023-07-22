module Index

open Elmish
open Fable.Remoting.Client
open Fable.Core.JsInterop
open Shared

importAll "./css/tailwind.css"

type Model =
    { Accounts: Account list
      ActiveAccount: Account option
      Transactions: Transaction list }

    member this.getAccount accountId =
        this.Accounts
        |> List.find (fun acct -> acct.Id = accountId)

type Msg =
    | GotAccounts of Account list
    | ChooseAccount of AccountId
    | GotTransactions of AccountId * Transaction list

let budgetApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IBudgetApi>

let init () : Model * Cmd<Msg> =
    let model =
        { Accounts = []
          ActiveAccount = None
          Transactions = [] }

    let cmd = Cmd.OfAsync.perform budgetApi.getAccounts () GotAccounts
    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotAccounts accounts when accounts.IsEmpty -> { model with Accounts = accounts }, Cmd.none
    | GotAccounts accounts ->
        let accountId = accounts.Head.Id
        let cmd = Cmd.OfAsync.perform budgetApi.getTransactions accountId GotTransactions

        { model with Accounts = accounts }, cmd
    | ChooseAccount accountId ->
        let cmd = Cmd.OfAsync.perform budgetApi.getTransactions accountId GotTransactions
        model, cmd
    | GotTransactions (accountId, transactions) ->
        { model with
            ActiveAccount =
                model.Accounts
                |> Seq.find (fun acct -> acct.Id = accountId)
                |> Some
            Transactions = transactions },
        Cmd.none

open Feliz
open Feliz.Bulma
open System

let backgroundColor = "#2C4461"
let darkBackgroundColor = "#29374D"
let lightBackgroundColor = "#4673A3"

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

let viewActiveAccount (model: Model) (dispatch: Msg -> unit) =
    match model.ActiveAccount with
    | None -> Bulma.title "No active account"
    | Some account ->
        Bulma.block [
            Bulma.title account.Name

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
                            for transaction in model.Transactions do
                                Html.tr [
                                    Html.td (formatDate transaction.Date)
                                    match transaction.Type with
                                    | Inflow (_, payee)
                                    | Outflow (_, payee) -> Html.td payee
                                    | Transfer (fromAccountId, toAccountId) ->
                                        if account.Id = fromAccountId then
                                            let toAccount = model.getAccount toAccountId
                                            Html.td $"Transfer to {toAccount.Name}"
                                        else
                                            let fromAccount = model.getAccount fromAccountId
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
                                    | Transfer (fromAccountId, toAccountId) when fromAccountId = account.Id ->
                                        Html.td (transaction.Amount.ToString())
                                        Html.td ""
                                    | Transfer (fromAccountId, toAccountId) when toAccountId = account.Id ->
                                        Html.td ""
                                        Html.td (transaction.Amount.ToString())
                                    | Transfer (fromAccountId, toAccountId) -> failwith "Not Implemented"
                                ]
                        ]
                    ]
                ]
            ]
        ]
// Bulma.box [
//     Bulma.content [
//         Html.ol [
//             for todo in model.Todos do
//                 Html.li [ prop.text todo.Description ]
//         ]
//     ]
//     Bulma.field.div [
//         field.isGrouped
//         prop.children [
//             Bulma.control.p [
//                 control.isExpanded
//                 prop.children [
//                     Bulma.input.text [
//                         prop.value model.Input
//                         prop.placeholder "What needs to be done?"
//                         prop.onChange (fun x -> SetInput x |> dispatch)
//                     ]
//                 ]
//             ]
//             Bulma.control.p [
//                 Bulma.button.a [
//                     color.isPrimary
//                     prop.disabled (Todo.isValid model.Input |> not)
//                     prop.onClick (fun _ -> dispatch AddTodo)
//                     prop.text "Add"
//                 ]
//             ]
//         ]
//     ]
// ]

let renderAccounts model dispatch =
    Html.ul [
        for account in model.Accounts do
            Html.li [
                prop.text account.Name
                if Some(account) = model.ActiveAccount then
                    prop.classes [ "rounded-md p-1 pl-5" ]

                    prop.style [
                        style.backgroundColor lightBackgroundColor
                    ]
                else
                    prop.classes [
                        "rounded-md p-1 pl-5"
                        $"hover:bg-[{darkBackgroundColor}] hover:cursor-pointer"
                    ]

                    prop.onClick (fun ev -> ChooseAccount account.Id |> dispatch)
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
                            renderAccounts model dispatch
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