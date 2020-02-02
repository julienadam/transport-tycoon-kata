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

type Location =
| Factory
| Port
| WarehouseA
| WarehouseB

type AvailableTruckLocation =
| Factory
| Port
| WarehouseB

type AvailableShipLocation =
| Port
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

    let inline private truckLocationToLocation (t: AvailableTruckLocation) =
        match t with
        | WarehouseB -> B
        | Factory -> FACTORY
        | AvailableTruckLocation.Port -> PORT

    let inline private shipLocationToLocation (d: AvailableShipLocation) =
        match d with
        | WarehouseA -> A
        | Port -> PORT

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

    let logTruckDepartureEvent (time: int) (vehicleId: VehicleId) (location: AvailableTruckLocation) (destination: AvailableTruckLocation) (cargo: Cargo option) =
        logDepartureEvent TRUCK time vehicleId (location |> truckLocationToLocation) (destination |> truckLocationToLocation) cargo

    let logTruckArrivalEvent (time: int) (vehicleId: VehicleId) (location: AvailableTruckLocation) (cargo: Cargo option) =
        logArrivalEvent TRUCK time vehicleId (location |> truckLocationToLocation) cargo

    let logShipDepartureEvent (time: int) (vehicleId: VehicleId) (location: AvailableShipLocation) (destination: AvailableShipLocation) (cargo: Cargo option) =
        logDepartureEvent SHIP time vehicleId (location |> shipLocationToLocation) (destination |> shipLocationToLocation) cargo

    let logShipArrivalEvent (time: int) (vehicleId: VehicleId) (location: AvailableShipLocation) (cargo: Cargo option) =
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
    | Parked of AvailableTruckLocation * int
    | OnTheRoad of Transit<AvailableTruckLocation>

type ShipLocation =
    | Docked of AvailableShipLocation * int
    | AtSea of Transit<AvailableShipLocation>

type TruckState =
    {
        Location : TruckLocation
        Cargo: Cargo option
    }

type ShipState =
    {
        Location : ShipLocation
        Cargo : Cargo option
    }

type VehicleState =
| TruckState of TruckState
| ShipState of ShipState

type Vehicle =
    | Truck of VehicleId * TruckState
    | Ship of VehicleId * ShipState
    with 
        member this.IsInTransitWithCargo () =
            match this with 
            | Truck (_,state) -> match state.Cargo, state.Location with | Some _, OnTheRoad _ -> true | _ -> false
            | Ship (_,state) -> match state.Cargo, state.Location with | Some _, AtSea _ -> true | _ -> false

let FactoryToPortTransitTime = 1
let PortToFactoryTransitTime = FactoryToPortTransitTime
let FactoryToWarehouseBTransitTime = 5
let WarehouseBToFactoryTransitTime = FactoryToWarehouseBTransitTime

open EventLogging

let driveOneHour (factoryOutboundQueue: Cargo list) (portOutboundQueue: Cargo list) (truckState: TruckState) (truckId: VehicleId ) (hoursElapsed: int) (sink: Event -> unit) =
    match truckState.Location, factoryOutboundQueue with
    | Parked (Factory, hours), [] ->
        if hoursElapsed > 0 && hours = 0 then
            logTruckArrivalEvent hoursElapsed truckId Factory None sink
        factoryOutboundQueue, portOutboundQueue, { truckState with Location = Parked (Factory, hours + 1) }
    | Parked (Factory, _), cargoToPickup::restOfFactoryQueue ->
        if hoursElapsed > 0 then
            logTruckArrivalEvent hoursElapsed truckId Factory None sink
        match cargoToPickup.Destination with
        | Warehouse.A ->
            logTruckDepartureEvent hoursElapsed truckId Factory AvailableTruckLocation.Port (Some cargoToPickup) sink
            restOfFactoryQueue, portOutboundQueue, { truckState with Cargo = (Some cargoToPickup); Location = Parked (AvailableTruckLocation.Port, 0) }
        | Warehouse.B ->
            logTruckDepartureEvent hoursElapsed truckId Factory WarehouseB (Some cargoToPickup) sink
            restOfFactoryQueue, portOutboundQueue, { truckState with Cargo = Some cargoToPickup; Location = OnTheRoad { Origin = AvailableTruckLocation.Factory; Destination = WarehouseB; HoursLeftToDestination = FactoryToWarehouseBTransitTime - 1 } }
    | Parked (AvailableTruckLocation.Port, _), _ -> 
        logTruckArrivalEvent (hoursElapsed) truckId AvailableTruckLocation.Port truckState.Cargo sink
        logTruckDepartureEvent hoursElapsed truckId AvailableTruckLocation.Port Factory None sink
        factoryOutboundQueue, (portOutboundQueue |> List.append [truckState.Cargo.Value]), { truckState with Cargo = None; Location = Parked (Factory, 0) }
    | Parked (WarehouseB, _), _ -> 
        logTruckArrivalEvent (hoursElapsed) truckId WarehouseB truckState.Cargo sink
        logTruckDepartureEvent (hoursElapsed) truckId WarehouseB Factory None sink
        factoryOutboundQueue, portOutboundQueue, { truckState with Cargo = None; Location = OnTheRoad { Origin = WarehouseB; Destination = Factory; HoursLeftToDestination = WarehouseBToFactoryTransitTime }}
    | OnTheRoad t, _ when t.IsArrivingAt Factory ->
        factoryOutboundQueue, portOutboundQueue, { truckState with Location = Parked (Factory, 0)}
    | OnTheRoad t, _ when t.IsArrivingAt WarehouseB ->
        factoryOutboundQueue, portOutboundQueue, { truckState with Location = Parked (WarehouseB, 0)}
    | OnTheRoad t, _ ->
        factoryOutboundQueue, portOutboundQueue, { truckState with Location = OnTheRoad (t.ProgressOneHour()) }

open Expecto

let testCargoA = { Id = 1; Destination = Warehouse.A }
let testCargoA' = { Id = 2; Destination = Warehouse.A }
let testCargoB = { Id = 100; Destination = Warehouse.B }

let truckTests =

    testList "Truck driving tests" [
        test "After driving one hour, a truck going to the port should be waiting at the port with the cargo picked up at the factory" {
        let factory, port, truckState = driveOneHour [testCargoA] [] { Cargo = None; Location = Parked (Factory, 0)} 1 1 consoleWriteSink
        Expect.equal truckState { Cargo = Some testCargoA; Location = Parked (AvailableTruckLocation.Port, 0)} "Truck is not in the expected state"
        Expect.equal factory [] "Factory is not empty"
        Expect.equal port [] "Port does not have exactly one container"
        }

        test "After driving one hour, a truck with cargo going to warehouse B with 5 hours remaining transit time should be in the same state but with 4 hours left" {
            let factory, port, truckState = driveOneHour [] [] { Cargo = Some testCargoB; Location = OnTheRoad { Origin = Factory; Destination = WarehouseB; HoursLeftToDestination = 5 }} 1 1 consoleWriteSink
            Expect.equal truckState { Cargo = Some testCargoB; Location = OnTheRoad { Origin = Factory; Destination = WarehouseB; HoursLeftToDestination = 4 } } "Truck is not in the expected state"
            Expect.equal factory [] "Factory is not empty"
            Expect.equal port [] "Port is not empty"
        }

        test "After driving one hour, a truck with cargo going to warehouse B with 1 hour remaining transit time should be stopped at the warehouse" {
            let factory, port, truckState = driveOneHour [] [] { Cargo = Some testCargoB; Location = OnTheRoad { Origin = Factory; Destination = WarehouseB; HoursLeftToDestination = 1 }} 1 1 consoleWriteSink
            Expect.equal truckState { Cargo = Some testCargoB; Location = Parked (WarehouseB, 0) } "Truck is not in the expected state"
            Expect.equal factory [] "Factory is not empty"
            Expect.equal port [] "Port is not empty"
        }

        test "After driving one hour, a empty truck going to an empty factory with 1 hour remaining transit time should be empty and idling" {
            let factory, port, truckState = driveOneHour [] [] { Cargo = None; Location = OnTheRoad { Origin = AvailableTruckLocation.Port; Destination = Factory; HoursLeftToDestination = 1 }} 1 1 consoleWriteSink
            Expect.equal truckState { Cargo = None; Location = Parked (Factory, 0) } "Truck is not in the expected state"
            Expect.equal factory [] "Factory is not empty"
            Expect.equal port [] "Port is not empty"
        }

        test "After driving one hour, an empty truck going to a factory containing cargo for warehouse A and 1 hour remaining transit time should be stopped at the factory" {
            let factory, port, truckState = driveOneHour [ testCargoA ] [] { Cargo = None; Location = OnTheRoad { Origin = AvailableTruckLocation.Port; Destination = Factory; HoursLeftToDestination = 1 } } 1 1 consoleWriteSink
            Expect.equal truckState { Cargo = None; Location = Parked (Factory, 0) }  "Truck is not in the expected state"
            Expect.equal factory [testCargoA] "Container should still be at the factory"
            Expect.equal port [] "Port is not empty"
        }

        test "After driving one hour, an empty truck going to a factory containing cargo for warehouses A and B and 1 hour remaining transit time should be stopped at the factory" {
            let factory, port, truckState = driveOneHour [ testCargoA; testCargoB ] [] { Cargo = None; Location = OnTheRoad { Origin = AvailableTruckLocation.Port; Destination = Factory; HoursLeftToDestination = 1 } } 1 1 consoleWriteSink
            Expect.equal truckState { Cargo = None; Location = Parked (Factory, 0) } "Truck is not in the expected state"
            Expect.equal factory [testCargoA; testCargoB] "Factory should still have both containers"
            Expect.equal port [] "Port is not empty"
        }

        test "After driving one hour, an idling truck at a factory containing cargo for warehouses A and B should be stopped at the port with the cargo" {
            let factory, port, truckState = driveOneHour [ testCargoA; testCargoB ] [] { Cargo = None; Location = Parked (Factory, 0) } 1 1 consoleWriteSink
            Expect.equal truckState { Cargo = Some testCargoA; Location = Parked (AvailableTruckLocation.Port, 0) } "Truck is not in the expected state"
            Expect.equal factory [testCargoB] "Factory should have a single container for warehouse B"
            Expect.equal port [] "Port should be empty"
        }

        test "After one hour, a truck stopped at the port with cargo should be stopped at the factory and the cargo should be in the port" {
            let factory, port, truckState = driveOneHour [] [] { Cargo = Some testCargoA; Location = Parked (AvailableTruckLocation.Port, 0) } 1 1 consoleWriteSink
            Expect.equal truckState { Cargo = None; Location = Parked (Factory, 0) } "Truck is not in the expected state"
            Expect.equal factory [] "Factory is not empty"
            Expect.equal port [testCargoA] "Port should the container dropped by the truck"
        }

        test "After one hour, an idling truck at an empty factory should still be idling" {
            let factory, port, truckState = driveOneHour [] [] { Cargo = None; Location = Parked (Factory, 0) } 1 1 consoleWriteSink
            Expect.equal truckState { Cargo = None; Location = Parked (Factory, 1) } "Truck is not in the expected state"
            Expect.equal factory [] "Warehouse is not empty"
            Expect.equal port [] "Port is not empty"
        }

        test "After one hour, a truck stopped at warehouseB should be empty, going to the factory with 5 hours left" {
            let factory, port, truckState = driveOneHour [] [] { Cargo = Some testCargoB; Location = Parked (WarehouseB, 0) } 1 1 consoleWriteSink
            Expect.equal truckState { Cargo = None; Location = OnTheRoad { Origin = WarehouseB; Destination = Factory; HoursLeftToDestination = 5 } } "Truck is not in the expected state"
            Expect.equal factory [] "Factory is not empty"
            Expect.equal port [] "Port is not empty"
        }
  ]

runTests defaultConfig truckTests

let sailOneHour (portOutboundQueue: Cargo list) (shipState : ShipState) (shipId: VehicleId) (hoursElapsed: int) (sink: Event -> unit)=
    match portOutboundQueue, shipState.Location with
    | [], AtSea t when t.IsArrivingAt Port ->
        logShipArrivalEvent (hoursElapsed + 1) shipId Port None sink
        [], { shipState with Location = Docked (Port, 0) }
    | firstContainer::otherContainers, AtSea t when t.IsArrivingAt Port  ->
        logShipArrivalEvent (hoursElapsed + 1) shipId Port None sink
        logShipDepartureEvent (hoursElapsed + 1) shipId Port WarehouseA (Some firstContainer) sink
        otherContainers, { shipState with Location = AtSea { Origin = Port; Destination = WarehouseA; HoursLeftToDestination = 4 }; Cargo = Some firstContainer }
    | _, AtSea t when t.IsArrivingAt WarehouseA ->
        logShipArrivalEvent (hoursElapsed + 1) shipId WarehouseA shipState.Cargo sink
        logShipDepartureEvent (hoursElapsed + 1) shipId WarehouseA Port None sink
        portOutboundQueue, { shipState with Location = AtSea { Origin = WarehouseA; Destination = Port; HoursLeftToDestination = 4 }; Cargo = None}
    | _, AtSea t ->
        portOutboundQueue, { shipState with Location = AtSea (t.ProgressOneHour ()) }
    | firstContainer::otherContainers, Docked (Port, 0) ->
        logShipDepartureEvent hoursElapsed shipId Port WarehouseA (Some firstContainer) sink
        otherContainers, { shipState with Location = AtSea { Origin = Port; Destination = WarehouseA; HoursLeftToDestination = 3 }; Cargo = Some firstContainer }
    | [], Docked (Port, 0) ->
        portOutboundQueue, shipState

let shipTests =
    testList "Ship sailing tests" [

        test "After one hour, a ship sailing to warehouse A with cargo, with 4 hours remaining will still be sailing, with 3 hours remaining" {
            let port, shipState = sailOneHour [] { Cargo = Some testCargoA; Location = AtSea { Origin = Port; Destination = WarehouseA; HoursLeftToDestination = 4 }} 1 1 consoleWriteSink
            Expect.equal shipState { Cargo = Some testCargoA; Location = AtSea { Origin = Port; Destination = WarehouseA; HoursLeftToDestination = 3 }} "Ship is not in the expected state"
            Expect.equal port [] "Port is not empty"
        }

        test "After one hour, a ship sailing to warehouse A with cargo, with 1 hours remaining should be sailing back empty to the port, with 4 hours remaining" {
            let port, shipState = sailOneHour [] { Cargo = Some testCargoA; Location = AtSea { Origin = Port; Destination = WarehouseA; HoursLeftToDestination = 1 }} 1 1 consoleWriteSink
            Expect.equal shipState { Cargo = None; Location = AtSea { Origin = WarehouseA; Destination = Port; HoursLeftToDestination = 4 } } "Ship is not in the expected state"
            Expect.equal port [] "Port is not empty"
        }

        test "After one hour, a ship sailing to an empty port, with 1 hours remaining should be idling " {
            let port, shipState = sailOneHour [] { Cargo = None; Location = AtSea { Origin = WarehouseA; Destination = Port; HoursLeftToDestination = 1 } } 1 1 consoleWriteSink
            Expect.equal shipState { Cargo = None; Location = Docked (Port, 0) } "Ship is not in the expected state"
            Expect.equal port [] "Port is not empty"
        }

        test "After one hour, a ship sailing to a port with cargo, with 1 hours remaining should be going to warehouse A with the first cargo, with 4 hours remaining" {
            let port, shipState = sailOneHour [testCargoA; testCargoA'] { Cargo = None; Location = AtSea { Origin = WarehouseA; Destination = Port; HoursLeftToDestination = 1 } } 1 1 consoleWriteSink
            Expect.equal shipState { Cargo = Some testCargoA; Location = AtSea { Origin = Port; Destination = WarehouseA; HoursLeftToDestination = 4 } } "Ship is not in the expected state"
            Expect.equal port [testCargoA'] "Port should still have one container"
        }

        test "After one hour, a ship idling in port with cargo available should be going to warehouse A with the first cargo, with 3 hours remaining" {
            let port, shipState = sailOneHour [testCargoA; testCargoA'] { Cargo = None; Location = Docked (Port, 0) } 1 1 consoleWriteSink
            Expect.equal shipState { Cargo = Some testCargoA; Location = AtSea { Origin = Port; Destination = WarehouseA; HoursLeftToDestination = 3 }} "Ship is not in the expected state"
            Expect.equal port [testCargoA'] "Port should still have one container"
        }
  ]

runTests defaultConfig shipTests

let moveOneHour factoryOutboundQueue portOutboundQueue vehicle elapsed sink =
    match vehicle with
    | Ship (id, shipState) ->
        let portOutboundQueueAfterOneHour, shipStateAfterOneHour = sailOneHour portOutboundQueue shipState id elapsed sink
        factoryOutboundQueue, portOutboundQueueAfterOneHour, Ship (id, shipStateAfterOneHour)
    | Truck (id, truckState) ->
        let factoryOutboundQueueAfterOneHour, portOutboundQueueAfterOneHour, truckStateAfterOneHour = driveOneHour factoryOutboundQueue portOutboundQueue truckState id elapsed sink
        factoryOutboundQueueAfterOneHour, portOutboundQueueAfterOneHour, Truck (id, truckStateAfterOneHour)

let initialState = [
    Truck (0, { Cargo = None; Location = Parked (Factory, 0) })
    Truck (1, { Cargo = None; Location = Parked (Factory, 0) })
    Ship (2, { Cargo = None; Location = Docked (Port, 0) })
]

let createCargoListFromInput (input: string) =
    input
    |> Seq.mapi (fun index character ->
        match character with
        | 'A' -> { Id = index; Destination = Warehouse.A }
        | 'B' -> { Id = index; Destination = Warehouse.B }
        | invalid -> failwithf "Invalid character %c" invalid)
    |> Seq.toList

let deliver destinations sink =
    let cargoList = destinations |> createCargoListFromInput
    let rec passTimeUntilAllContainersAreDelivered (factoryOutboundQueue: Cargo list) (portOutboundQueue: Cargo list) (vehicles : Vehicle list) hoursElapsed =
        if vehicles |> List.exists (fun v -> v.IsInTransitWithCargo()) || not (factoryOutboundQueue |> List.isEmpty) || not (portOutboundQueue |> List.isEmpty) then
            let letOneHourPass (currentFactoryOutboundQueue, currentPortOutboundQueue) vehicleState =
                let factoryOutBoundQueueAfterOneHour, portOutboundQueueAfterOneHour, vehicleStateAfterOneHour = moveOneHour currentFactoryOutboundQueue currentPortOutboundQueue vehicleState hoursElapsed sink
                vehicleStateAfterOneHour, (factoryOutBoundQueueAfterOneHour,portOutboundQueueAfterOneHour)

            let vehicleStatesAfterOneHour, (newFactoryOutboundQueue, newPortOutboundQueue) = vehicles |> List.mapFold letOneHourPass (factoryOutboundQueue, portOutboundQueue)
            passTimeUntilAllContainersAreDelivered newFactoryOutboundQueue newPortOutboundQueue vehicleStatesAfterOneHour (hoursElapsed + 1)
        else
            hoursElapsed

    passTimeUntilAllContainersAreDelivered cargoList [] initialState 0


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