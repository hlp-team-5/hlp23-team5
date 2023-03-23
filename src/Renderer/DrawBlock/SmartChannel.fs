module SmartChannel
open CommonTypes
open Elmish
open DrawHelpers
open DrawModelType.SymbolT
open DrawModelType.BusWireT
open BusWire
open BusWireUpdateHelpers

open Optics
open Operators


/// HLP23: suggested initial smartChannel top-level function
/// to be tested, it must be given a channel in through which to route wires nicely
/// Normally the channel will come from symbol edges.
/// The function must identify all wires with segments going through the channel and space them
/// This function routes the middle segment of all 7 segment wires by moving them perpendicular to its direction.
/// It is expected that all wires provided are 7 segment with middle segment in the same direction
/// wires not so will be ignored.
/// The messiness of when to call it, what to do with different types of wire, which wires to call it with
/// could be left till later.
/// For this simple routing only one dimension of the channel is needed (determined by orientation).
/// The Wires going through the channel must be returned as an updated Wires map in model.


/// Some helpers which are lower in compiler order
type BusUpdateHelpers = {
    wireIntersectsBoundingBox: Wire -> BoundingBox -> option<float>
}

/// Returns all wires present inside a specified channel
let findWiresInChannel channel wireList (busUpdateHelpers: BusUpdateHelpers) = 

    let isIntersecting (id,wire) = 
        match busUpdateHelpers.wireIntersectsBoundingBox wire channel with
            | None -> false
            | Some x -> true

    wireList |> List.filter isIntersecting
        

let calculateWireSpacing (channelDimension: float) (numWires: int) = 
    let wireSpacing = 0.7 * channelDimension / (float numWires)

    [1..numWires]
    |> List.map (fun i -> (float i) * wireSpacing)
    

let findWireSpacing (channelOrientation: Orientation) (channel: BoundingBox)
    (wireCount: int)= 

    let tl = channel.TopLeft

    match channelOrientation with 
    | Vertical -> 
        calculateWireSpacing channel.W wireCount
        |> List.map (fun pos -> tl.X + pos)

    | Horizontal -> 
        calculateWireSpacing channel.H wireCount
        |> List.map (fun pos -> tl.Y + pos) 

///Top level function for auto-spacing wires in a bounding box
let smartChannelRoute (channelOrientation: Orientation) 
    (channel: BoundingBox) 
    (model:Model) 
    (busUpdateHelpers: BusUpdateHelpers) :Model =

    let tl = channel.TopLeft
    printfn $"SmartChannel: channel {channelOrientation}:(%.1f{tl.X},%.1f{tl.Y}) W=%.1f{channel.W} H=%.1f{channel.H}"

    let oldWireList =
        model.Wires
        |> Map.toList
 
    let wiresInChannel = 
        findWiresInChannel channel oldWireList busUpdateHelpers
        |> List.sortBy (fun (id,wire) -> wire.StartPos.X)

    //
    let shiftedWiresList =

        let wireSpacing = findWireSpacing channelOrientation channel wiresInChannel.Length
        match channelOrientation with 
        | Vertical -> 
            true

        | Horizontal -> 
            let wireSpacing = findWireSpacing channelOrientation channel wiresInChannel.Length
            false
            
    model