#load @"..\.paket\load\netcoreapp3.0\main.group.fsx"

// Assuming you have :
// - python in your PATH
// - FAKE in your PATH
// You can do : "fake run" in the solution folder
// It will run the exercice and generate traces

type CargoId = int

type Warehouse =
| A
| B

type Cargo =
    {
        Id: CargoId
        Destination: Warehouse
    }

type LandPickupLocation =
| Factory

type LandDropOffLocation =
| PortTerminal
| WarehouseB

type LandLocation =
| LandPickupLocation of LandPickupLocation
| LandDropOffLocation of LandDropOffLocation

type SeaPickupLocation =
| PortQuay

type SeaDropOffLocation =
| WarehouseA

type SeaLocation = 
| SeaPickupLocation of SeaPickupLocation
| SeaDropOffLocation of SeaDropOffLocation

type VehicleId = int

module EventLogging =

    type EventType =
        | ARRIVE
        | DEPART

    type TransportKind =
        | TRUCK
        | SHIP

    type LocationLogInfo =
        | FACTORY
        | PORT
        | A
        | B

    type CargoLogInfo =
        {
            cargo_id: int
            destination: LocationLogInfo
            origin: LocationLogInfo
        }

    type Event =
        {
            event: EventType
            time: int
            transport_id: int
            kind: TransportKind
            location: LocationLogInfo
            destination: LocationLogInfo option
            cargo: CargoLogInfo list option
        }

    open FSharp.Json
    open System.IO

    let private config = JsonConfig.create (unformatted = true, serializeNone = SerializeNone.Omit)

    let serializeEvent evt = Json.serializeEx config evt

    let consoleWriteSink evt = serializeEvent evt |> System.Console.WriteLine
    
    let fileWriteSink fileName event =
        File.AppendAllText(fileName, (event |> serializeEvent) + System.Environment.NewLine)
    
    let consoleAndFileSink fileName event = 
        consoleWriteSink event
        fileWriteSink fileName event
    
    let inline private truckLocationToLocation (d: LandLocation) =
        match d with 
        | LandPickupLocation pl -> 
            match pl with
            | Factory -> FACTORY
        | LandDropOffLocation dl -> 
            match dl with
            | WarehouseB -> B
            | PortTerminal -> PORT

    let inline private shipLocationToLocation (d: SeaLocation) =
        match d with 
        | SeaPickupLocation p -> 
            match p with
            | PortQuay -> PORT
        | SeaDropOffLocation dl -> 
            match dl with
            | WarehouseA -> A

    let inline private cargoToCargoInfo (cargo: Cargo) =
        {
            cargo_id = cargo.Id
            destination = match cargo.Destination with | Warehouse.A -> A | Warehouse.B -> B
            origin = FACTORY
        }

    let private logEvent eventType kind (time: int) (vehicleId: VehicleId) (location: LocationLogInfo) (destination: LocationLogInfo option) (cargo: Cargo option) (sink: Event -> unit) =
        let event =
            {
                event = eventType
                time = time
                transport_id = vehicleId
                kind = kind
                location = location
                destination = destination
                cargo = match cargo with | None -> None | Some c -> Some [c |> cargoToCargoInfo]
            }
        event |> sink

    let private logDepartureEvent kind (time: int) (vehicleId: VehicleId) (location: LocationLogInfo) (destination: LocationLogInfo) (cargo: Cargo option) =
        logEvent DEPART kind time vehicleId location (Some destination) cargo

    let private logArrivalEvent kind (time: int) (vehicleId: VehicleId) (location: LocationLogInfo) (cargo: Cargo option) =
         logEvent ARRIVE kind time vehicleId location None cargo

    let logTruckDepartureEvent (time: int) (vehicleId: VehicleId) (location: LandLocation) (destination: LandLocation) (cargo: Cargo option) =
        logDepartureEvent TRUCK time vehicleId (location |> truckLocationToLocation) (destination |> truckLocationToLocation) cargo

    let logTruckArrivalEvent (time: int) (vehicleId: VehicleId) (location: LandLocation) (cargo: Cargo option) =
        logArrivalEvent TRUCK time vehicleId (location |> truckLocationToLocation) cargo

    let logShipDepartureEvent (time: int) (vehicleId: VehicleId) (location: SeaLocation) (destination: SeaLocation) (cargo: Cargo option) =
        logDepartureEvent SHIP time vehicleId (location |> shipLocationToLocation) (destination |> shipLocationToLocation) cargo

    let logShipArrivalEvent (time: int) (vehicleId: VehicleId) (location: SeaLocation) (cargo: Cargo option) =
        logArrivalEvent SHIP time vehicleId (location |> shipLocationToLocation) cargo

open System

type CargoTracker = {
    FactoryOutboundQueue: Cargo list
    PortOutboundQueue: Cargo list
    InTransit: (Cargo * VehicleId) list
    Delivered: (Cargo * VehicleId * int) list
}
with 
    static member Empty = { FactoryOutboundQueue = []; PortOutboundQueue = []; InTransit = []; Delivered = []}
    member this.AddToFactoryQueue ([<ParamArray>] cargo: Cargo[]) =
        { this with FactoryOutboundQueue = List.append this.FactoryOutboundQueue (cargo |> Array.toList) }
    member this.TryPickupAtFactory vehicleId  =
        match this.FactoryOutboundQueue with 
        | [] -> None, this
        | firstAvailableCargo::rest -> Some firstAvailableCargo, { this with FactoryOutboundQueue = rest; InTransit = (firstAvailableCargo, vehicleId)::this.InTransit }
    member this.TryPickupAtPort vehicleId  =
        match this.PortOutboundQueue with 
        | [] -> None, this
        | firstAvailableCargo::rest -> Some firstAvailableCargo, { this with PortOutboundQueue = rest; InTransit = (firstAvailableCargo, vehicleId)::this.InTransit }
    member this.DropOffAtPort cargo =
        { this with PortOutboundQueue = List.append this.PortOutboundQueue [cargo]; InTransit = this.InTransit |> List.filter (fun (c, _) -> c.Id <> cargo.Id) }
    member this.Deliver cargo vehicleId time =
        { this with Delivered = (cargo, vehicleId, time) :: this.Delivered; InTransit = this.InTransit |> List.filter (fun (c, _) -> c.Id <> cargo.Id) }
    member this.AllCargoDelivered () =
        match this.FactoryOutboundQueue, this.InTransit, this.PortOutboundQueue with
        | [], [], [] ->
            Some (this.Delivered |> List.map(fun (_,_,hourDelivered) -> hourDelivered) |> List.max)
        | _ -> None

open EventLogging

type LadenParkedTruck = {
    ParkedAt : LandDropOffLocation
    Cargo: Cargo
}
with 
    member this.UnloadAndReturn () =
        match this.ParkedAt with
        | WarehouseB -> ReturningTruck { Origin = WarehouseB; Destination = Factory; HoursLeftToDestination = 4 }
        | PortTerminal -> UnladenParkedTruck { ParkedAt = Factory; HoursWaited = 0}
and UnladenParkedTruck = {
    ParkedAt : LandPickupLocation
    HoursWaited : int
}
with
    member this.Wait() = 
        UnladenParkedTruck { this with HoursWaited = this.HoursWaited + 1 }
    member this.LoadAndStartDelivery (cargo: Cargo) =
        match cargo.Destination with 
        | Warehouse.A -> LadenParkedTruck { ParkedAt = PortTerminal; Cargo = cargo }
        | Warehouse.B -> DeliveringTruck { Origin = Factory; Destination = WarehouseB; HoursLeftToDestination = 4; Cargo = cargo }

and DeliveringTruck = {
    Origin: LandPickupLocation
    Destination: LandDropOffLocation
    HoursLeftToDestination: int
    Cargo: Cargo
}
with 
    member this.DriveOn () =
        match this.HoursLeftToDestination with
        | 1 -> LadenParkedTruck { ParkedAt = this.Destination; Cargo = this.Cargo }
        | _ -> DeliveringTruck { this with HoursLeftToDestination = this.HoursLeftToDestination - 1 }

and ReturningTruck = {
    Origin: LandDropOffLocation
    Destination: LandPickupLocation
    HoursLeftToDestination: int
}
with 
    member this.DriveOn () =
        match this.HoursLeftToDestination with
        | 1 -> UnladenParkedTruck { ParkedAt = this.Destination; HoursWaited = 0 }
        | _ -> ReturningTruck { this with HoursLeftToDestination = this.HoursLeftToDestination - 1 }

and TruckState =
| LadenParkedTruck of LadenParkedTruck
| UnladenParkedTruck of UnladenParkedTruck
| DeliveringTruck of DeliveringTruck
| ReturningTruck of ReturningTruck

type IdentifiableVehicle<'a> = {
    Id: VehicleId
    State: 'a
}

type Truck = 
    {
        Id: VehicleId
        State: TruckState
    }
    with 
        member this.DriveOneHour (cargoTracker: CargoTracker) (hoursElapsed: int) (sink: Event -> unit) =
            let newTracker, newState =
                match this.State with
                | LadenParkedTruck truckState ->
                    logTruckArrivalEvent hoursElapsed this.Id (LandDropOffLocation (truckState.ParkedAt)) (Some truckState.Cargo) sink
                    logTruckDepartureEvent hoursElapsed this.Id (LandDropOffLocation (truckState.ParkedAt)) (LandPickupLocation Factory) None sink
                    match truckState.ParkedAt with
                    | PortTerminal -> (cargoTracker.DropOffAtPort truckState.Cargo), truckState.UnloadAndReturn()
                    | WarehouseB -> (cargoTracker.Deliver truckState.Cargo this.Id hoursElapsed), truckState.UnloadAndReturn()
                | ReturningTruck truckState -> 
                    cargoTracker, truckState.DriveOn()
                | DeliveringTruck truckState -> 
                    cargoTracker, truckState.DriveOn()
                | UnladenParkedTruck truckState ->
                    if truckState.HoursWaited = 0 then
                        logTruckArrivalEvent hoursElapsed this.Id (LandPickupLocation Factory) None sink
                    match cargoTracker.TryPickupAtFactory this.Id with
                    | Some c, tracker ->
                        let dest = 
                            match c.Destination with
                            | Warehouse.A -> LandDropOffLocation PortTerminal
                            | Warehouse.B -> LandDropOffLocation WarehouseB
                        logTruckDepartureEvent hoursElapsed this.Id (LandPickupLocation Factory) dest (Some c) sink
                        tracker, truckState.LoadAndStartDelivery c
                    | None, tracker -> 
                        tracker, truckState.Wait()
            newTracker, { this with State = newState}

open Expecto

let testCargoA = { Id = 1; Destination = Warehouse.A }
let testCargoA' = { Id = 2; Destination = Warehouse.A }
let testCargoB = { Id = 100; Destination = Warehouse.B }

let runTruckTest initialState initialTracker expectedState trackerChecks =
    let tracker, truck = { Id = 1; State = initialState}.DriveOneHour initialTracker 1 consoleWriteSink
    Expect.equal truck.State expectedState "Truck is not in the expected state"
    trackerChecks tracker

let runTruckTestWithEmptyTracker initialState expectedState =
    runTruckTest initialState CargoTracker.Empty expectedState ignore

let truckTests =
   testList "Truck tests" [
        test "After one hour, a truck at the factory should have picked up the first available container and be at the port with it" {
            let initialTracker = CargoTracker.Empty.AddToFactoryQueue testCargoA
            let initialState = UnladenParkedTruck { ParkedAt = Factory; HoursWaited = 0 }
            let expectedState = LadenParkedTruck { ParkedAt = PortTerminal; Cargo = testCargoA }
            runTruckTest initialState initialTracker expectedState (fun tracker ->
                Expect.equal tracker.FactoryOutboundQueue [] "Factory is not empty"
                Expect.equal tracker.PortOutboundQueue [] "Port is not empty"
                Expect.equal tracker.InTransit [testCargoA, 1] "Port is not empty")
        }

        test "After one hour, a truck with cargo going to warehouse B with 4 hours remaining transit time should be in the same state but with 3 hours left" {
            let initialState = DeliveringTruck { Origin = Factory; Destination = WarehouseB; HoursLeftToDestination = 4; Cargo = testCargoB }
            let expectedState = DeliveringTruck { Origin = Factory; Destination = WarehouseB; HoursLeftToDestination = 3; Cargo = testCargoB }
            runTruckTestWithEmptyTracker initialState expectedState
        }

        test "After one hour, a truck with cargo going to warehouse B with 1 hour remaining transit time should be parked at the warehouse" {
            let initialState = DeliveringTruck { Origin = Factory; Destination = WarehouseB; HoursLeftToDestination = 1; Cargo = testCargoB }
            let expectedState = LadenParkedTruck { ParkedAt = WarehouseB; Cargo = testCargoB }
            runTruckTestWithEmptyTracker initialState expectedState
        }

        test "After one hour, a empty truck going to the factory with 1 hour remaining transit time should be parked at the factory" {
            let initialState = ReturningTruck { Origin = WarehouseB; Destination = Factory; HoursLeftToDestination = 1 }
            let expectedState = UnladenParkedTruck { ParkedAt = Factory; HoursWaited = 0}
            runTruckTestWithEmptyTracker initialState expectedState
        }

        test "After one hour, a parked truck at a factory containing cargo for warehouses A and B should be parked at the port with cargo A" {
            let initialTracker = CargoTracker.Empty.AddToFactoryQueue(testCargoA, testCargoB)
            let initialState = UnladenParkedTruck { ParkedAt = Factory; HoursWaited = 0}
            let expectedState = LadenParkedTruck { ParkedAt = PortTerminal; Cargo = testCargoA}
            runTruckTest initialState initialTracker expectedState (fun tracker ->
                Expect.equal tracker.FactoryOutboundQueue [testCargoB] ""
                Expect.equal tracker.InTransit [testCargoA, 1] "")
        }

        test "After one hour, a truck parked at the port with cargo should be back at the factory and the cargo should have been unloaded in the port" {
            let initialState = LadenParkedTruck { ParkedAt = PortTerminal; Cargo = testCargoA}
            let expectedState = UnladenParkedTruck { ParkedAt = Factory; HoursWaited = 0 }
            runTruckTest initialState CargoTracker.Empty expectedState (fun tracker ->
                Expect.equal tracker.FactoryOutboundQueue [] "Factory is not empty"
                Expect.equal tracker.PortOutboundQueue [testCargoA] "Port should contain the cargo dropped off by the truck")
        }

        test "After one hour, a parked truck at an empty factory should still be parked" {
            let initialState = UnladenParkedTruck { ParkedAt = Factory; HoursWaited = 0}
            let expectedState = UnladenParkedTruck { ParkedAt = Factory; HoursWaited = 1}
            runTruckTestWithEmptyTracker initialState expectedState
        }

        test "After one hour, a truck parked at warehouseB should be going back empty to the factory with 4 hours left" {
            let initialState = LadenParkedTruck { ParkedAt = WarehouseB; Cargo = testCargoB}
            let expectedState = ReturningTruck { Origin = WarehouseB; Destination = Factory; HoursLeftToDestination = 4 }

            runTruckTest initialState CargoTracker.Empty expectedState (fun tracker ->
                Expect.equal tracker.FactoryOutboundQueue [] "Factory is not empty"
                Expect.equal tracker.PortOutboundQueue [] ""
                Expect.equal tracker.InTransit [] "Port should the container dropped by the truck"
                Expect.equal tracker.Delivered [testCargoB, 1, 1] "Port should the container dropped by the truck")
        }
 ]

runTests defaultConfig truckTests

type LadenDockedShip = {
    DockedAt : SeaDropOffLocation
    Cargo: Cargo
}
with 
    member _.UnloadAndCastOut () =
        ReturningShip { Origin = WarehouseA; Destination = PortQuay; HoursLeftToDestination = 3 }

and UnladenDockedShip = {
    DockedAt : SeaPickupLocation
    HoursWaited : int
}
with
    member this.Wait() = 
        UnladenDockedShip { this with HoursWaited = this.HoursWaited + 1 }
    member _.LoadAndCastOut cargo =
        DeliveringShip { Origin = PortQuay; Destination = WarehouseA; HoursLeftToDestination = 3; Cargo = cargo }

and DeliveringShip = {
    Origin: SeaPickupLocation
    Destination: SeaDropOffLocation
    HoursLeftToDestination: int
    Cargo: Cargo
}
with 
    member this.SailOn () =
        match this.HoursLeftToDestination with
        | 1 -> LadenDockedShip { DockedAt = this.Destination; Cargo = this.Cargo }
        | _ -> DeliveringShip { this with HoursLeftToDestination = this.HoursLeftToDestination - 1 }

and ReturningShip = {
    Origin: SeaDropOffLocation
    Destination: SeaPickupLocation
    HoursLeftToDestination: int
}
with 
    member this.SailOn () =
        match this.HoursLeftToDestination with
        | 1 -> UnladenDockedShip { DockedAt = this.Destination; HoursWaited = 0 }
        | _ -> ReturningShip { this with HoursLeftToDestination = this.HoursLeftToDestination - 1 }

and ShipState =
| LadenDockedShip of LadenDockedShip
| UnladenDockedShip of UnladenDockedShip
| DeliveringShip of DeliveringShip
| ReturningShip of ReturningShip

type Ship = IdentifiableVehicle<ShipState>

let sailOneHour (cargoTracker: CargoTracker) (vehicle: Ship) (hoursElapsed: int) (sink: Event -> unit)=
    match vehicle.State with
    | DeliveringShip ship ->
        cargoTracker, ship.SailOn ()
    | ReturningShip ship ->
        cargoTracker, ship.SailOn ()
    | LadenDockedShip ship ->
        logShipArrivalEvent hoursElapsed vehicle.Id (SeaDropOffLocation WarehouseA) (Some ship.Cargo) sink
        logShipDepartureEvent hoursElapsed vehicle.Id (SeaDropOffLocation WarehouseA) (SeaPickupLocation PortQuay) None sink
        cargoTracker.Deliver ship.Cargo vehicle.Id hoursElapsed, ship.UnloadAndCastOut()
    | UnladenDockedShip ship ->
        if ship.HoursWaited = 0 then
            logShipArrivalEvent hoursElapsed vehicle.Id (SeaPickupLocation PortQuay) None sink
        match cargoTracker.TryPickupAtPort vehicle.Id with
        | None, tracker ->
            tracker, ship.Wait()
        | Some cargo, tracker ->
            logShipDepartureEvent hoursElapsed vehicle.Id (SeaPickupLocation PortQuay) (SeaDropOffLocation WarehouseA) (Some cargo) sink
            tracker, ship.LoadAndCastOut cargo

let shipTests =
   testList "Ship sailing tests" [

       test "After one hour, a ship sailing to warehouse A with cargo, with 4 hours remaining should still be sailing, with 3 hours remaining" {
           let initialTracker = CargoTracker.Empty
           let tracker, shipState = sailOneHour initialTracker { Id = 1; State = (DeliveringShip { Cargo = testCargoA; Origin = PortQuay; Destination = WarehouseA; HoursLeftToDestination = 4 })} 1 consoleWriteSink
           Expect.equal shipState (DeliveringShip { Cargo = testCargoA; Origin = PortQuay; Destination = WarehouseA; HoursLeftToDestination = 3 }) "Ship is not in the expected state"
           Expect.equal tracker initialTracker ""
       }

       test "After one hour, a ship returning to port, with 4 hours remaining should still be sailing, with 3 hours remaining" {
           let initialTracker = CargoTracker.Empty
           let tracker, shipState = sailOneHour initialTracker { Id = 1; State = (ReturningShip { Origin =  WarehouseA; Destination = PortQuay; HoursLeftToDestination = 4 })} 1 consoleWriteSink
           Expect.equal shipState (ReturningShip { Origin =  WarehouseA; Destination = PortQuay; HoursLeftToDestination = 3 }) "Ship is not in the expected state"
           Expect.equal tracker initialTracker ""
       }

       test "After one hour, a ship sailing to the warehouse with cargo, with 1 hours remaining should be docked at the destination" {
           let initialTracker = CargoTracker.Empty
           let tracker, shipState = sailOneHour initialTracker { Id = 1; State = (DeliveringShip { Cargo = testCargoA; Origin = PortQuay; Destination = WarehouseA; HoursLeftToDestination = 1 })} 1 consoleWriteSink
           Expect.equal shipState (LadenDockedShip { Cargo = testCargoA; DockedAt = WarehouseA }) "Ship is not in the expected state"
           Expect.equal tracker initialTracker ""
       }

       test "After one hour, a ship returning to port, with 1 hours remaining should be docked at the port" {
           let initialTracker = CargoTracker.Empty
           let tracker, shipState = sailOneHour initialTracker { Id = 1; State = (ReturningShip { Origin = WarehouseA; Destination = PortQuay; HoursLeftToDestination = 1 })} 1 consoleWriteSink
           Expect.equal shipState (UnladenDockedShip { DockedAt = PortQuay; HoursWaited = 0 }) "Ship is not in the expected state"
           Expect.equal tracker initialTracker ""
       }

       test "After one hour, a ship docked at the port with cargo available should be sailing to the warehouse with 3 hours remaining" {
           let initialTracker = (CargoTracker.Empty.DropOffAtPort testCargoA).DropOffAtPort testCargoA'
           let tracker, shipState = sailOneHour initialTracker { Id = 1; State = (UnladenDockedShip { DockedAt = PortQuay; HoursWaited = 0 })} 1 consoleWriteSink
           Expect.equal shipState (DeliveringShip { Cargo = testCargoA; Origin = PortQuay; Destination = WarehouseA; HoursLeftToDestination = 3 }) "Ship is not in the expected state"
           Expect.equal tracker.PortOutboundQueue [testCargoA'] "Port should only have A' left"
           Expect.equal tracker.InTransit [testCargoA, 1] "Port should only have A' left"
       }

       test "After one hour, a ship docked at the port with no cargo available should be waiting at the port" {
           let initialTracker = CargoTracker.Empty
           let tracker, shipState = sailOneHour initialTracker { Id = 1; State = (UnladenDockedShip { DockedAt = PortQuay; HoursWaited = 0 })} 1 consoleWriteSink
           Expect.equal shipState (UnladenDockedShip { DockedAt = PortQuay; HoursWaited = 1 }) "Ship is not in the expected state"
           Expect.equal tracker initialTracker ""
       }

       test "After one hour, a ship docked at the warehouse should be sailing empty to the port" {
           let initialTracker = CargoTracker.Empty
           let tracker, shipState = sailOneHour initialTracker { Id = 1; State = (LadenDockedShip { DockedAt = WarehouseA; Cargo = testCargoA })} 1 consoleWriteSink
           Expect.equal shipState (ReturningShip { Origin = WarehouseA; Destination = PortQuay; HoursLeftToDestination = 3 }) "Ship is not in the expected state"
           Expect.equal tracker.Delivered [testCargoA, 1, 1] "Cargo A should have been delivered at hour 1"
       }

 ]

runTests defaultConfig shipTests

type Vehicle =
    | Truck of Truck
    | Ship of Ship

let moveOneHour (cargoTracker: CargoTracker) vehicle elapsed sink =
    match vehicle with
    | Ship ship ->
        let cargoTrackerAfterOneHour, shipStateAfterOneHour = sailOneHour cargoTracker ship elapsed sink
        cargoTrackerAfterOneHour, Ship { Id = ship.Id; State = shipStateAfterOneHour }
    | Truck truck -> 
        let c, t = truck.DriveOneHour cargoTracker elapsed sink
        c, Truck t

let initialState = [
    Truck { Id = 0; State = UnladenParkedTruck { ParkedAt = Factory; HoursWaited = 1 } } // TODO: specific initial state instead of 1 to avoid arrival event
    Truck { Id = 1; State = UnladenParkedTruck { ParkedAt = Factory; HoursWaited = 1 } }
    Ship { Id = 2; State = UnladenDockedShip { DockedAt = PortQuay; HoursWaited = 1 } }
]

let createCargoTrackerFromInput (input: string) =
    CargoTracker.Empty.AddToFactoryQueue(
        input
        |> Seq.mapi (fun index character ->
            match character with
            | 'A' -> { Id = index; Destination = Warehouse.A }
            | 'B' -> { Id = index; Destination = Warehouse.B }
            | invalid -> failwithf "Invalid character %c" invalid)
        |> Seq.toArray)

let deliver destinations sink =
    let cargoTracker = destinations |> createCargoTrackerFromInput

    let rec passTimeUntilAllContainersAreDelivered (cargoTracker:CargoTracker) (vehicles : Vehicle list) hoursElapsed =
        match cargoTracker.AllCargoDelivered () with
        | Some h -> h
        | None -> 
            let letOneHourPass cargoTracker vehicleState =
                let trackerAfterOneHour, vehicleStateAfterOneHour = moveOneHour cargoTracker vehicleState hoursElapsed sink
                vehicleStateAfterOneHour, trackerAfterOneHour

            let vehicleStatesAfterOneHour, newCargoTracker = vehicles |> List.mapFold letOneHourPass cargoTracker
            passTimeUntilAllContainersAreDelivered newCargoTracker vehicleStatesAfterOneHour (hoursElapsed + 1)


    passTimeUntilAllContainersAreDelivered cargoTracker initialState 0


let deliveryTests =
    testList "Delivery tests" [

        test "Delivering A takes 5 hours" {
            let hoursElapsed = deliver "A" consoleWriteSink
            Expect.equal hoursElapsed 5 "Hours elapsed did not match expected time span"
        }

        test "Delivering B takes 5 hours" {
            let hoursElapsed = deliver "B" consoleWriteSink
            Expect.equal hoursElapsed 5 "Hours elapsed did not match expected time span"
        }

        test "Delivering AB takes 5 hours" {
            let hoursElapsed = deliver "AB" consoleWriteSink
            Expect.equal hoursElapsed 5 "Hours elapsed did not match expected time span"
        }

        test "Delivering BB takes 5 hours" {
            let hoursElapsed = deliver "BB" consoleWriteSink
            Expect.equal hoursElapsed 5 "Hours elapsed did not match expected time span"
        }

        test "Delivering ABB takes 7 hours" {
            let hoursElapsed = deliver "ABB" consoleWriteSink
            Expect.equal hoursElapsed 7 "Hours elapsed did not match expected time span"
        }
    ]

runTests defaultConfig deliveryTests

open System.IO

let outputDir = Path.Combine(__SOURCE_DIRECTORY__, "output")
if not (Directory.Exists(outputDir)) then
    Directory.CreateDirectory(outputDir) |> ignore

let deliverAndLog cargoListInput =
    let filePath = (Path.Combine(outputDir, cargoListInput + ".log"))
    File.Delete(filePath) |> ignore
    let loggingSink evt = consoleAndFileSink filePath evt
    deliver cargoListInput loggingSink

deliverAndLog "AB"
deliverAndLog "AABABBAB"
deliverAndLog "ABBBABAAABBB"
