#load @"..\.paket\load\netcoreapp3.0\main.group.fsx"

type CargoId = int

type Warehouse =
| A
| B

type Cargo =
    {
        Id: CargoId
        Destination: Warehouse
    }

type LandAccessibleLocation =
| Factory
| PortTerminal
| WarehouseB

type SeaAccessibleLocation =
| PortQuay
| WarehouseA

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

    let private config = JsonConfig.create (unformatted = true, serializeNone = SerializeNone.Omit)

    let serializeEvent evt = Json.serializeEx config evt

    let consoleWriteSink evt = serializeEvent evt |> System.Console.WriteLine

    let inline private truckLocationToLocation (t: LandAccessibleLocation) =
        match t with
        | WarehouseB -> B
        | Factory -> FACTORY
        | PortTerminal -> PORT

    let inline private shipLocationToLocation (d: SeaAccessibleLocation) =
        match d with
        | WarehouseA -> A
        | PortQuay -> PORT

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

    let logTruckDepartureEvent (time: int) (vehicleId: VehicleId) (location: LandAccessibleLocation) (destination: LandAccessibleLocation) (cargo: Cargo option) =
        logDepartureEvent TRUCK time vehicleId (location |> truckLocationToLocation) (destination |> truckLocationToLocation) cargo

    let logTruckArrivalEvent (time: int) (vehicleId: VehicleId) (location: LandAccessibleLocation) (cargo: Cargo option) =
        logArrivalEvent TRUCK time vehicleId (location |> truckLocationToLocation) cargo

    let logShipDepartureEvent (time: int) (vehicleId: VehicleId) (location: SeaAccessibleLocation) (destination: SeaAccessibleLocation) (cargo: Cargo option) =
        logDepartureEvent SHIP time vehicleId (location |> shipLocationToLocation) (destination |> shipLocationToLocation) cargo

    let logShipArrivalEvent (time: int) (vehicleId: VehicleId) (location: SeaAccessibleLocation) (cargo: Cargo option) =
        logArrivalEvent SHIP time vehicleId (location |> shipLocationToLocation) cargo


type Transit<'a> when 'a : equality =
    {
        Origin : 'a
        Destination : 'a
        HoursLeftToDestination: int
    }
    with
        member this.IsArrivingAt loc = this.HoursLeftToDestination = 1 && this.Destination = loc
        member this.ProgressOneHour () = { this with HoursLeftToDestination = this.HoursLeftToDestination - 1 }

type TruckLocation =
    | Parked of LandAccessibleLocation * int
    | OnTheRoad of Transit<LandAccessibleLocation>

type ShipLocation =
    | Docked of SeaAccessibleLocation * int
    | AtSea of Transit<SeaAccessibleLocation>

type TruckState =
    {
        Location : TruckLocation
        Cargo: Cargo option
    }
    
//type PickupLocation = 
//| PortQuay
//| FactoryLoadingBay
    
//type CargoState =
//| WaitingCargo of Cargo * PickupLocation
//| InTransitCargo of Cargo * VehicleId
//| DeliveredCargo of Cargo * VehicleId * int

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
        { this with FactoryOutboundQueue = this.FactoryOutboundQueue |> List.append (cargo |> Array.toList) }
    member this.TryPickupAtFactory vehicleId  =
        match this.FactoryOutboundQueue with 
        | [] -> None, this
        | firstAvailableCargo::rest -> Some firstAvailableCargo, { this with FactoryOutboundQueue = rest; InTransit = (firstAvailableCargo, vehicleId)::this.InTransit }
    member this.TryPickupAtPort vehicleId  =
        match this.PortOutboundQueue with 
        | [] -> None, this
        | firstAvailableCargo::rest -> Some firstAvailableCargo, { this with PortOutboundQueue = rest; InTransit = (firstAvailableCargo, vehicleId)::this.InTransit }
    member this.DropOffAtPort cargo =
        { this with PortOutboundQueue = this.PortOutboundQueue |> List.append [cargo]; InTransit = this.InTransit |> List.filter (fun (c, _) -> c.Id <> cargo.Id) }
    member this.Deliver cargo vehicleId time =
        { this with Delivered = (cargo, vehicleId, time) :: this.Delivered; InTransit = this.InTransit |> List.filter (fun (c, _) -> c.Id <> cargo.Id) }
    member this.AllCargoDelivered () =
        match this.FactoryOutboundQueue, this.InTransit, this.PortOutboundQueue with
        | [], [], [] ->
            Some (this.Delivered |> List.map(fun (_,_,hourDelivered) -> hourDelivered) |> List.max)
        | _ -> None

let FactoryToPortTransitTime = 1
let PortToFactoryTransitTime = FactoryToPortTransitTime
let FactoryToWarehouseBTransitTime = 5
let WarehouseBToFactoryTransitTime = FactoryToWarehouseBTransitTime

open EventLogging

let driveOneHour (cargoTracker: CargoTracker) (truckState: TruckState) (truckId: VehicleId ) (hoursElapsed: int) (sink: Event -> unit) =
    match truckState.Location with
    | Parked (Factory, hours) ->
        if hoursElapsed > 0 && hours = 0 then
            logTruckArrivalEvent hoursElapsed truckId Factory None sink

        let cargo, tracker = cargoTracker.TryPickupAtFactory truckId
        match cargo with
        | None -> tracker, { truckState with Location = Parked (Factory, hours + 1) }
        | Some cargoToPickup ->
            match cargoToPickup.Destination with
            | Warehouse.A ->
                logTruckDepartureEvent hoursElapsed truckId Factory PortTerminal (Some cargoToPickup) sink
                tracker, { truckState with Cargo = (Some cargoToPickup); Location = Parked (PortTerminal, 0) }
            | Warehouse.B ->
                logTruckDepartureEvent hoursElapsed truckId Factory WarehouseB (Some cargoToPickup) sink
                tracker, { truckState with Cargo = Some cargoToPickup; Location = OnTheRoad { Origin = Factory; Destination = WarehouseB; HoursLeftToDestination = FactoryToWarehouseBTransitTime - 1 } }
    | Parked (PortTerminal, _) -> 
        logTruckArrivalEvent (hoursElapsed) truckId PortTerminal truckState.Cargo sink
        logTruckDepartureEvent hoursElapsed truckId PortTerminal Factory None sink
        cargoTracker.DropOffAtPort(truckState.Cargo.Value), { truckState with Cargo = None; Location = Parked (Factory, 0) }
    | Parked (WarehouseB, _) -> 
        logTruckArrivalEvent (hoursElapsed) truckId WarehouseB truckState.Cargo sink
        logTruckDepartureEvent (hoursElapsed) truckId WarehouseB Factory None sink
        (cargoTracker.Deliver truckState.Cargo.Value truckId hoursElapsed), { truckState with Cargo = None; Location = OnTheRoad { Origin = WarehouseB; Destination = Factory; HoursLeftToDestination = WarehouseBToFactoryTransitTime - 1 }}
    | OnTheRoad t when t.IsArrivingAt Factory ->
        cargoTracker, { truckState with Location = Parked (Factory, 0)}
    | OnTheRoad t when t.IsArrivingAt WarehouseB ->
        cargoTracker, { truckState with Location = Parked (WarehouseB, 0)}
    | OnTheRoad t ->
        cargoTracker, { truckState with Location = OnTheRoad (t.ProgressOneHour()) }

open Expecto

let testCargoA = { Id = 1; Destination = Warehouse.A }
let testCargoA' = { Id = 2; Destination = Warehouse.A }
let testCargoB = { Id = 100; Destination = Warehouse.B }

let truckTests =

   testList "Truck driving tests" [
        test "After driving one hour, a truck going to the port should be waiting at the port with the cargo picked up at the factory" {
            let initialTracker = CargoTracker.Empty.AddToFactoryQueue testCargoA
            let tracker, truckState = driveOneHour initialTracker { Cargo = None; Location = Parked (Factory, 0)} 1 1 consoleWriteSink
            Expect.equal truckState { Cargo = Some testCargoA; Location = Parked (PortTerminal, 0)} "Truck is not in the expected state"
            Expect.equal tracker.FactoryOutboundQueue [] "Factory is not empty"
            Expect.equal tracker.PortOutboundQueue [] "Port is not empty"
            Expect.equal tracker.InTransit [testCargoA, 1] "Port is not empty"
        }

        test "After driving one hour, a truck with cargo going to warehouse B with 5 hours remaining transit time should be in the same state but with 4 hours left" {
            let tracker, truckState = driveOneHour CargoTracker.Empty { Cargo = Some testCargoB; Location = OnTheRoad { Origin = Factory; Destination = WarehouseB; HoursLeftToDestination = 5 }} 1 1 consoleWriteSink
            Expect.equal truckState { Cargo = Some testCargoB; Location = OnTheRoad { Origin = Factory; Destination = WarehouseB; HoursLeftToDestination = 4 } } "Truck is not in the expected state"
            Expect.equal tracker.FactoryOutboundQueue [] "Factory is not empty"
            Expect.equal tracker.PortOutboundQueue [] "Port is not empty"
        }

        test "After driving one hour, a truck with cargo going to warehouse B with 1 hour remaining transit time should be stopped at the warehouse" {
            let tracker, truckState = driveOneHour CargoTracker.Empty { Cargo = Some testCargoB; Location = OnTheRoad { Origin = Factory; Destination = WarehouseB; HoursLeftToDestination = 1 }} 1 1 consoleWriteSink
            Expect.equal truckState { Cargo = Some testCargoB; Location = Parked (WarehouseB, 0) } "Truck is not in the expected state"
            Expect.equal tracker.FactoryOutboundQueue [] "Factory is not empty"
            Expect.equal tracker.PortOutboundQueue [] "Port is not empty"
        }

        test "After driving one hour, a empty truck going to an empty factory with 1 hour remaining transit time should be empty and idling" {
            let tracker, truckState = driveOneHour CargoTracker.Empty { Cargo = None; Location = OnTheRoad { Origin = PortTerminal; Destination = Factory; HoursLeftToDestination = 1 }} 1 1 consoleWriteSink
            Expect.equal truckState { Cargo = None; Location = Parked (Factory, 0) } "Truck is not in the expected state"
            Expect.equal tracker.FactoryOutboundQueue [] "Factory is not empty"
            Expect.equal tracker.PortOutboundQueue [] "Port is not empty"
        }

        test "After driving one hour, an empty truck going to a factory containing cargo for warehouse A and 1 hour remaining transit time should be stopped at the factory" {
            let initialTracker = CargoTracker.Empty.AddToFactoryQueue testCargoA
            let tracker, truckState = driveOneHour initialTracker { Cargo = None; Location = OnTheRoad { Origin = PortTerminal; Destination = Factory; HoursLeftToDestination = 1 } } 1 1 consoleWriteSink
            Expect.equal truckState { Cargo = None; Location = Parked (Factory, 0) }  "Truck is not in the expected state"
            Expect.equal tracker initialTracker ""
        }

        test "After driving one hour, a parked truck at a factory containing cargo for warehouses A and B should be parked at the port with the cargo" {
            let initialTracker = CargoTracker.Empty.AddToFactoryQueue(testCargoA, testCargoB)
            let tracker, truckState = driveOneHour initialTracker { Cargo = None; Location = Parked (Factory, 0) } 1 1 consoleWriteSink
            Expect.equal truckState { Cargo = Some testCargoA; Location = Parked (PortTerminal, 0) } "Truck is not in the expected state"
            Expect.equal tracker.FactoryOutboundQueue [testCargoB] ""
            Expect.equal tracker.InTransit [testCargoA, 1] ""
        }

        test "After one hour, a truck stopped at the port with cargo should be stopped at the factory and the cargo should be in the port" {
            let initialTracker = CargoTracker.Empty
            let tracker, truckState = driveOneHour initialTracker { Cargo = Some testCargoA; Location = Parked (PortTerminal, 0) } 1 1 consoleWriteSink
            Expect.equal truckState { Cargo = None; Location = Parked (Factory, 0) } "Truck is not in the expected state"
            Expect.equal tracker.FactoryOutboundQueue [] "Factory is not empty"
            Expect.equal tracker.PortOutboundQueue [testCargoA] "Port should contain the cargo dropped off by the truck"
            Expect.equal tracker.InTransit [] ""
        }

        test "After one hour, a parked truck at an empty factory should still be parked" {
            let initialTracker = CargoTracker.Empty
            let tracker, truckState = driveOneHour initialTracker { Cargo = None; Location = Parked (Factory, 0) } 1 1 consoleWriteSink
            Expect.equal truckState { Cargo = None; Location = Parked (Factory, 1) } "Truck is not in the expected state"
            Expect.equal tracker initialTracker ""
        }

        test "After one hour, a truck stopped at warehouseB should be empty, going to the factory with 4 hours left" {
            let initialTracker = CargoTracker.Empty
            let tracker, truckState = driveOneHour initialTracker { Cargo = Some testCargoB; Location = Parked (WarehouseB, 0) } 1 1 consoleWriteSink
            Expect.equal truckState { Cargo = None; Location = OnTheRoad { Origin = WarehouseB; Destination = Factory; HoursLeftToDestination = 4 } } "Truck is not in the expected state"
            Expect.equal tracker.FactoryOutboundQueue [] "Factory is not empty"
            Expect.equal tracker.PortOutboundQueue [] ""
            Expect.equal tracker.InTransit [] "Port should the container dropped by the truck"
            Expect.equal tracker.Delivered [testCargoB, 1, 0] "Port should the container dropped by the truck"
        }
 ]

runTests defaultConfig truckTests

type LadenDockedShip = {
    DockedAt : SeaAccessibleLocation
    Cargo: Cargo
}
with 
    member _.UnloadAndCastOut () =
        SailingShip { Transit = { Origin = WarehouseA; Destination = PortQuay; HoursLeftToDestination = 3 }; Cargo = None }

and UnladenDockedShip = {
    DockedAt : SeaAccessibleLocation
    HoursWaited : int
}
with
    member this.Wait() = 
        UnladenDockedShip { this with HoursWaited = this.HoursWaited + 1 }
    member this.LoadAndCastOut cargo =
        SailingShip { Transit = { Origin = PortQuay; Destination = WarehouseA; HoursLeftToDestination = 3 }; Cargo = Some cargo }

and SailingShip = {
    Transit: Transit<SeaAccessibleLocation>
    Cargo: Cargo option
}
with 
    member this.SailOn () =
        match this.Transit.HoursLeftToDestination with
        | 1 -> 
            match this.Cargo with
            | Some c -> LadenDockedShip { DockedAt = this.Transit.Destination; Cargo = c }
            | None -> UnladenDockedShip { DockedAt = this.Transit.Destination; HoursWaited = 0 }
        | _ -> SailingShip { this with Transit = this.Transit.ProgressOneHour() }

and ShipState =
| LadenDockedShip of LadenDockedShip
| UnladenDockedShip of UnladenDockedShip
| SailingShip of SailingShip

let sailOneHour (cargoTracker: CargoTracker) (shipState : ShipState) (shipId: VehicleId) (hoursElapsed: int) (sink: Event -> unit)=
    match shipState with
    | SailingShip ship ->
        cargoTracker, ship.SailOn ()
    | LadenDockedShip ship ->
        logShipArrivalEvent hoursElapsed shipId WarehouseA (Some ship.Cargo) sink
        logShipDepartureEvent hoursElapsed shipId WarehouseA PortQuay None sink
        cargoTracker.Deliver ship.Cargo shipId hoursElapsed, ship.UnloadAndCastOut()
    | UnladenDockedShip ship ->
        if ship.HoursWaited = 0 then
            logShipArrivalEvent hoursElapsed shipId PortQuay None sink
        match cargoTracker.TryPickupAtPort shipId with
        | None, tracker ->
            tracker, ship.Wait()
        | Some cargo, tracker ->
            logShipDepartureEvent hoursElapsed shipId PortQuay WarehouseA (Some cargo) sink
            tracker, ship.LoadAndCastOut cargo

//let shipTests =
//    testList "Ship sailing tests" [

//        test "After one hour, a ship sailing to warehouse A with cargo, with 4 hours remaining should still be sailing, with 3 hours remaining" {
//            let port, shipState = sailOneHour [] (SailingShip { Cargo = Some testCargoA; Transit = { Origin = PortQuay; Destination = WarehouseA; HoursLeftToDestination = 4} }) 1 1 consoleWriteSink
//            Expect.equal shipState (SailingShip { Cargo = Some testCargoA; Transit = { Origin = PortQuay; Destination = WarehouseA; HoursLeftToDestination = 3} }) "Ship is not in the expected state"
//            Expect.equal port [] "Port is not empty"
//        }

//        test "After one hour, a ship sailing to port with cargo, with 4 hours remaining should still be sailing, with 3 hours remaining" {
//            let port, shipState = sailOneHour [] (SailingShip { Cargo = Some testCargoA; Transit = { Origin = WarehouseA; Destination = PortQuay; HoursLeftToDestination = 4} }) 1 1 consoleWriteSink
//            Expect.equal shipState (SailingShip { Cargo = Some testCargoA; Transit = { Origin = WarehouseA; Destination = PortQuay; HoursLeftToDestination = 3} }) "Ship is not in the expected state"
//            Expect.equal port [] "Port is not empty"
//        }

//        test "After one hour, a ship sailing to the warehouse with cargo, with 1 hours remaining should be docked at the destination" {
//            let port, shipState = sailOneHour [] (SailingShip { Cargo = Some testCargoA; Transit = { Origin = PortQuay; Destination = WarehouseA; HoursLeftToDestination = 1} }) 1 1 consoleWriteSink
//            Expect.equal shipState (LadenDockedShip { Cargo = testCargoA; DockedAt = WarehouseA }) "Ship is not in the expected state"
//            Expect.equal port [] "Port is not empty"
//        }

//        test "After one hour, a ship sailing to the port without cargo, with 1 hours remaining should be docked at the destination" {
//            let port, shipState = sailOneHour [] (SailingShip { Cargo = None; Transit = { Origin = WarehouseA; Destination = PortQuay; HoursLeftToDestination = 1} }) 1 1 consoleWriteSink
//            Expect.equal shipState (UnladenDockedShip { DockedAt = PortQuay; HoursWaited = 0 }) "Ship is not in the expected state"
//            Expect.equal port [] "Port is not empty"
//        }

//        test "After one hour, a ship docked at the port with cargo available should be sailing to the warehouse with 3 hours remaining" {
//            let port, shipState = sailOneHour [testCargoA; testCargoA'] (UnladenDockedShip { DockedAt = PortQuay; HoursWaited = 0 }) 1 1 consoleWriteSink
//            Expect.equal shipState (SailingShip { Cargo = Some testCargoA; Transit = { Origin = PortQuay; Destination = WarehouseA; HoursLeftToDestination = 3} }) "Ship is not in the expected state"
//            Expect.equal port [testCargoA'] "Port should have one container left"
//        }

//        test "After one hour, a ship docked at the port with no cargo available should be waiting at the port" {
//            let port, shipState = sailOneHour [] (UnladenDockedShip { DockedAt = PortQuay; HoursWaited = 0 }) 1 1 consoleWriteSink
//            Expect.equal shipState (UnladenDockedShip { DockedAt = PortQuay; HoursWaited = 1 }) "Ship is not in the expected state"
//            Expect.equal port [] "Port is not empty"
//        }

//        test "After one hour, a ship docked at the warehouse should be sailing empty to the port" {
//            let port, shipState = sailOneHour [] (LadenDockedShip { DockedAt = WarehouseA; Cargo = testCargoA }) 1 1 consoleWriteSink
//            Expect.equal shipState (SailingShip { Cargo = None; Transit = { Origin = WarehouseA; Destination = PortQuay; HoursLeftToDestination = 3 }}) "Ship is not in the expected state"
//            Expect.equal port [] "Port is not empty"
//        }

//  ]

//runTests defaultConfig shipTests

type IdentifiableVehicle<'a> = {
    Id: VehicleId
    Vehicle: 'a
}

type Vehicle =
    | Truck of IdentifiableVehicle<TruckState>
    | Ship of IdentifiableVehicle<ShipState>

let moveOneHour (cargoTracker: CargoTracker) vehicle elapsed sink =
    match vehicle with
    | Ship { Id = id; Vehicle = state } ->
        let cargoTrackerAfterOneHour, shipStateAfterOneHour = sailOneHour cargoTracker state id elapsed sink
        cargoTrackerAfterOneHour, Ship { Id = id; Vehicle = shipStateAfterOneHour }
    | Truck { Id = id; Vehicle = truckState } ->
        let cargoTrackerAfterOneHour, truckStateAfterOneHour = driveOneHour cargoTracker truckState id elapsed sink
        cargoTrackerAfterOneHour, Truck { Id = id; Vehicle = truckStateAfterOneHour }

let initialState = [
    Truck { Id = 0; Vehicle = { Cargo = None; Location = Parked (Factory, 0) } }
    Truck { Id = 1; Vehicle = { Cargo = None; Location = Parked (Factory, 0) } }
    Ship { Id = 2; Vehicle = UnladenDockedShip { DockedAt = PortQuay; HoursWaited = 0 } }
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
        match cargoTracker.AllCargoDelivered ()with
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

deliver "AB" consoleWriteSink

// AABABBAB
deliver "AABABBAB" consoleWriteSink

// ABBBABAAABBB
deliver "ABBBABAAABBB" consoleWriteSink