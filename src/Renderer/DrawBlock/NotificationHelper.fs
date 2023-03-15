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