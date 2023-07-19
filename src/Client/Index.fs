module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Model = { Transactions: Transaction list }

type Msg = GotTransactions of Transaction list

let budgetApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IBudgetApi>

let init () : Model * Cmd<Msg> =
    let model = { Transactions = [] }
    let cmd = Cmd.OfAsync.perform budgetApi.getTransactions () GotTransactions
    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotTransactions ts -> { model with Transactions = ts }, Cmd.none

open Feliz
open Feliz.Bulma
open System

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

let containerBox (model: Model) (dispatch: Msg -> unit) =
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
                            Html.td "payee"
                            Html.td "category"
                            Html.td transaction.Comment
                            match transaction.Type with
                            | Inflow _ ->
                                Html.td ""
                                Html.td (transaction.Amount.ToString())
                            | Outflow (fromAccount) ->
                                Html.td (transaction.Amount.ToString())
                                Html.td ""
                            | Transfer (fromAccount, toAccount) -> failwith "Not Implemented"
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
                    style.backgroundColor "#2C4461"
                ]
                prop.children [
                    Bulma.column [
                        color.hasTextWhite
                        prop.children [ Html.h1 "Accounts" ]
                    ]
                ]
            ]
            Bulma.column [
                prop.children [
                    Bulma.title [ prop.text "budget" ]
                    containerBox model dispatch
                ]
            ]
        ]
    ]