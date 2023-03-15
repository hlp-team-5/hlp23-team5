module NotificationHelper

open Fulma
open Fulma.Extensions.Wikiki
open Fable.React.Props
open Browser.Dom
open Fable.React

open ModelType
open Sheet.SheetInterface


let notificationStyle = Style [
    ZIndex 100 // In front of everything.
    Position PositionOptions.Absolute
    UserSelect UserSelectOptions.None
    Right "20px"
    Bottom "20px"
]

let errorNotification text closeMsg =
    fun dispatch ->
        let close = (fun _ -> dispatch closeMsg)
        Notification.notification [
            Notification.Color IsDanger
            Notification.Props [ notificationStyle ]
        ] [
            Delete.delete [ Delete.OnClick close ] []
            str text
        ]

let successNotification text closeMsg =
    fun dispatch ->
        let close = (fun _ -> dispatch closeMsg)
        Notification.notification [
            Notification.Color  Color.IsInfo
            Notification.Props [ notificationStyle ]
        ] [
            Delete.delete [ Delete.OnClick close ] []
            str text
        ]

let successPropertiesNotification text = successNotification text ClosePropertiesNotification


let viewNotifications model dispatch =
    let sheetNotifications =
        match model.Sheet.GetNotifications with
        | Some msg -> Some <| errorNotification msg CloseDiagramNotification
        | None -> None
            
    [ //model.Notifications.FromDiagram
      sheetNotifications
      model.Notifications.FromSimulation
    ]
    |> List.tryPick id
    |> function
    | Some notification -> notification dispatch
    | None -> div [] []
