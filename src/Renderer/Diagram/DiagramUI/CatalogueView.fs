(*
    CatalogueView.fs

    View for catalogue in the right tab.
*)

module CatalogueView

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props

open DiagramStyle
open DiagramModelType
open DiagramMessageType
open DiagramTypes
open PopupView

let private menuItem label onClick =
    Menu.Item.li
        [ Menu.Item.IsActive false; Menu.Item.Props [ OnClick onClick ] ]
        [ str label ]

let private makeCustom model loadedComponent =
    menuItem loadedComponent.Name (fun _ ->
        let custom = Custom {
            Name = loadedComponent.Name
            InputLabels = loadedComponent.InputLabels
            OutputLabels = loadedComponent.OutputLabels
        }
        model.Diagram.CreateComponent custom loadedComponent.Name 100 100
        |> ignore
    )

let private makeCustomList model =
    match model.CurrProject with
    | None -> []
    | Some project ->
        // Do no show the open component in the catalogue.
        project.LoadedComponents
        |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        |> List.map (makeCustom model)

let private createIOPopup typeStr compType model dispatch =
    let title = sprintf "Add %s node" typeStr
    let beforeText =
        fun _ -> str <| sprintf "How do you want to name your %s?" typeStr
    let placeholder = "Component name"
    let beforeInt =
        fun _ -> str <| sprintf "How many bits should the %s node have?" typeStr
    let intDefault = 1
    let body = dialogPopupBodyTextAndInt beforeText placeholder beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let inputText = getText dialogData
            let inputInt = getInt dialogData
            model.Diagram.CreateComponent (compType inputInt) inputText 100 100 |> ignore
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) ->
            (getInt dialogData < 1) || (getText dialogData = "")
    dialogPopup title body buttonText buttonAction isDisabled dispatch

let private createSplitWirePopup model dispatch =
    let title = sprintf "Add SplitWire node" 
    let beforeInt =
        fun _ -> str "How many bits should the top wire have? The remaining bits will go in the bottom wire."
    let intDefault = 1
    let body = dialogPopupBodyOnlyInt beforeInt intDefault dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let inputInt = getInt dialogData
            JSHelpers.log inputInt
            model.Diagram.CreateComponent (SplitWire inputInt) "" 100 100 |> ignore
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) -> getInt dialogData < 1
    dialogPopup title body buttonText buttonAction isDisabled dispatch

let private makeMenuGroup title menuList =
    details [Open true] [
        summary [menuLabelStyle] [ str title ]
        Menu.list [] menuList
    ]

let viewCatalogue model dispatch =
    Menu.menu [] [
            makeMenuGroup
                "Input / Output"
                [ menuItem "Input"  (fun _ -> createIOPopup "input" Input model dispatch)
                  menuItem "Output" (fun _ -> createIOPopup "output" Output model dispatch) ]
            makeMenuGroup
                "Buses"
                [ menuItem "MergeWires"  (fun _ -> model.Diagram.CreateComponent MergeWires "" 100 100 |> ignore)
                  menuItem "SplitWire" (fun _ -> createSplitWirePopup model dispatch) ]
            makeMenuGroup
                "Gates"
                [ menuItem "Not"  (fun _ -> model.Diagram.CreateComponent Not "" 100 100 |> ignore)
                  menuItem "And"  (fun _ -> model.Diagram.CreateComponent And "" 100 100 |> ignore)
                  menuItem "Or"   (fun _ -> model.Diagram.CreateComponent Or "" 100 100 |> ignore)
                  menuItem "Xor"  (fun _ -> model.Diagram.CreateComponent Xor "" 100 100 |> ignore)
                  menuItem "Nand" (fun _ -> model.Diagram.CreateComponent Nand "" 100 100 |> ignore)
                  menuItem "Nor"  (fun _ -> model.Diagram.CreateComponent Nor "" 100 100 |> ignore)
                  menuItem "Xnor" (fun _ -> model.Diagram.CreateComponent Xnor "" 100 100 |> ignore) ]
            makeMenuGroup
                "Mux / Demux"
                [ menuItem "Mux2" (fun _ -> model.Diagram.CreateComponent Mux2 "" 100 100 |> ignore)
                  menuItem "Demux2" (fun _ -> model.Diagram.CreateComponent Demux2 "" 100 100 |> ignore) ]
            makeMenuGroup
                "This project"
                (makeCustomList model)
        ]
