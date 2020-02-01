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

type VehicleId = int

module EventLogging =

    type EventType =
        | DEPART
        | ARRIVE

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
            cargo: CargoLogInfo list
        }

    open FSharp.Json

    let private config = JsonConfig.create (unformatted = true, serializeNone = SerializeNone.Omit)

    let inline private serializeEvent evt = Json.serializeEx config evt

    let inline private locationToLocationLogInfo (d: Location) =
        match d with
        | WarehouseA -> A
        | WarehouseB -> B
        | Factory -> FACTORY
        | Port -> PORT

    let inline private cargoToCargoInfo (cargo: Cargo) =
        {
            cargo_id = cargo.Id
            destination = match cargo.Destination with | Warehouse.A -> A | Warehouse.B -> B
            origin = FACTORY
        }

    let private logEvent eventType kind (time: int) (vehicleId: VehicleId) (location: Location) (destination: Location option) (cargo: Cargo option) =
        let event =
            {
                event = eventType
                time = time
                transport_id = vehicleId
                kind = kind
                location = location |> locationToLocationLogInfo
                destination = destination |> Option.map locationToLocationLogInfo
                cargo = match cargo with | None -> [] | Some c -> [c |> cargoToCargoInfo]
            }
        serializeEvent event |> System.Console.WriteLine

    let private logDepartureEvent kind (time: int) (vehicleId: VehicleId) (location: Location) (destination: Location) (cargo: Cargo option) =
        logEvent DEPART kind time vehicleId location (Some destination) cargo

    let private logArrivalEvent kind (time: int) (vehicleId: VehicleId) (location: Location) (cargo: Cargo option) =
         logEvent ARRIVE kind time vehicleId location None cargo

    let logTruckDepartureEvent (time: int) (vehicleId: VehicleId) (location: Location) (destination: Location) (cargo: Cargo option) =
        logDepartureEvent TRUCK time vehicleId location destination cargo

    let logTruckArrivalEvent (time: int) (vehicleId: VehicleId) (location: Location) (cargo: Cargo option) =
        logArrivalEvent TRUCK time vehicleId location cargo

    let logShipDepartureEvent (time: int) (vehicleId: VehicleId) (location: Location) (destination: Location) (cargo: Cargo option) =
        logDepartureEvent SHIP time vehicleId location destination cargo

    let logShipArrivalEvent (time: int) (vehicleId: VehicleId) (location: Location) (cargo: Cargo option) =
        logArrivalEvent TRUCK time vehicleId location cargo


type Transit =
    {
        Origin : Location
        Destination : Location
        HoursLeftToDestination: int
    }
    with
        member this.IsArrivingAt loc = this.HoursLeftToDestination = 1 && this.Destination = loc
        member this.ProgressOneHour () = { this with HoursLeftToDestination = this.HoursLeftToDestination - 1 }

type VehicleLocation =
    | StoppedAt of Location
    | InTransit of Transit

type TruckState =
    {
        Location : VehicleLocation
        Cargo: Cargo option
    }

type ShipState =
    {
        Location : VehicleLocation
        Cargo : Cargo option
    }

type VehicleState =
| TruckState of TruckState
| ShipState of ShipState

type Vehicle =
    | Truck of VehicleId * TruckState
    | Ship of VehicleId * ShipState

let FactoryToPortTransitTime = 1
let PortToFactoryTransitTime = FactoryToPortTransitTime
let FactoryToWarehouseBTransitTime = 5
let WarehouseBToFactoryTransitTime = FactoryToWarehouseBTransitTime

open EventLogging

let driveOneHour (factoryOutboundQueue: Cargo list) (portOutboundQueue: Cargo list) (truckState: TruckState) (truckId: VehicleId ) (hoursElapsed: int) =
    match truckState.Location, factoryOutboundQueue with
    | StoppedAt Factory, [] ->
        factoryOutboundQueue, portOutboundQueue, truckState
    | StoppedAt Factory, cargoToPickup::restOfFactoryQueue ->
        match cargoToPickup.Destination with
        | Warehouse.A ->
            logTruckDepartureEvent hoursElapsed truckId Factory Port (Some cargoToPickup)
            logTruckArrivalEvent (hoursElapsed + 1) truckId Port (Some cargoToPickup)
            logTruckDepartureEvent (hoursElapsed + 1) truckId Port Factory None
            restOfFactoryQueue, portOutboundQueue |> List.append [ cargoToPickup ], { truckState with Cargo = None; Location = InTransit { Origin = Port; Destination = Factory; HoursLeftToDestination = FactoryToPortTransitTime }}
        | Warehouse.B ->
            logTruckDepartureEvent hoursElapsed truckId Factory WarehouseB (Some cargoToPickup)
            restOfFactoryQueue, portOutboundQueue, { truckState with Cargo = Some cargoToPickup; Location = InTransit { Origin = Factory; Destination = WarehouseB; HoursLeftToDestination = FactoryToWarehouseBTransitTime - 1 } }
    | StoppedAt _, _ -> failwith "Cannot be stopped anywhere except the factory"
    | InTransit t, [] when t.IsArrivingAt Factory ->
        logTruckArrivalEvent hoursElapsed truckId Factory None
        factoryOutboundQueue, portOutboundQueue, { truckState with Location = StoppedAt Factory}
    | InTransit t, cargoToPickup :: restOfFactoryQueue when t.IsArrivingAt Factory ->
        logTruckArrivalEvent hoursElapsed truckId Factory None
        let loc =
            match cargoToPickup.Destination with
            | Warehouse.A -> { Destination = Port; Origin = Factory; HoursLeftToDestination = FactoryToPortTransitTime }
            | Warehouse.B -> { Destination = WarehouseB; Origin = Factory; HoursLeftToDestination = FactoryToWarehouseBTransitTime }
        logTruckDepartureEvent hoursElapsed truckId Factory loc.Destination (Some cargoToPickup)
        restOfFactoryQueue, portOutboundQueue, { truckState with Cargo = Some cargoToPickup; Location = InTransit loc }
    | InTransit t, _ when t.IsArrivingAt Port ->
        logTruckArrivalEvent hoursElapsed truckId Port truckState.Cargo
        logTruckDepartureEvent hoursElapsed truckId Port Factory None
        factoryOutboundQueue, List.concat [portOutboundQueue; [ truckState.Cargo.Value ]] , { truckState with Cargo = None; Location = InTransit { Origin = Port; Destination = Factory; HoursLeftToDestination = PortToFactoryTransitTime } }
    | InTransit t, _ when t.IsArrivingAt WarehouseB ->
        logTruckArrivalEvent hoursElapsed truckId WarehouseB truckState.Cargo
        logTruckDepartureEvent hoursElapsed truckId WarehouseB Factory None
        factoryOutboundQueue, portOutboundQueue, { truckState with Cargo = None; Location = InTransit { Origin = WarehouseB; Destination = Factory; HoursLeftToDestination = WarehouseBToFactoryTransitTime }}
    | InTransit t, _ ->
        factoryOutboundQueue, portOutboundQueue, { truckState with Location = InTransit(t.ProgressOneHour()) }

open Expecto

let testCargoA = { Id = 1; Destination = Warehouse.A }
let testCargoA' = { Id = 2; Destination = Warehouse.A }
let testCargoB = { Id = 100; Destination = Warehouse.B }

let truckTests =

    testList "Truck driving tests" [
        test "After driving one hour, a truck with cargo going to the port should be empty, going back to the factory and the cargo should be at the port warehouse" {
        let warehouse, port, truckState = driveOneHour [] [] { Cargo = Some testCargoA; Location = InTransit { Origin = Factory; Destination = Port; HoursLeftToDestination = 1 }} 1 1
        Expect.equal truckState { Cargo = None; Location = InTransit { Origin = Port; Destination = Factory; HoursLeftToDestination = 1 }} "Truck is not in the expected state"
        Expect.equal warehouse [] "Warehouse is not empty"
        Expect.equal port [testCargoA] "Port does not have exactly one container"
        }

        test "After driving one hour, a truck with cargo going to warehouse B with 5 hours remaining transit time should be in the same state but with 4 hours left" {
            let warehouse, port, truckState = driveOneHour [] [] { Cargo = Some testCargoB; Location = InTransit { Origin = Factory; Destination = WarehouseB; HoursLeftToDestination = 5 }} 1 1
            Expect.equal truckState { Cargo = Some testCargoB; Location = InTransit { Origin = Factory; Destination = WarehouseB; HoursLeftToDestination = 4 } } "Truck is not in the expected state"
            Expect.equal warehouse [] "Warehouse is not empty"
            Expect.equal port [] "Port is not empty"
        }

        test "After driving one hour, a truck with cargo going to warehouse B with 1 hour remaining transit time should be empty, going back to the factory with 5 hours left" {
            let warehouse, port, truckState = driveOneHour [] [] { Cargo = Some testCargoB; Location = InTransit { Origin = Factory; Destination = WarehouseB; HoursLeftToDestination = 1 }} 1 1
            Expect.equal truckState { Cargo = None; Location = InTransit { Origin = WarehouseB; Destination = Factory; HoursLeftToDestination = 5 } } "Truck is not in the expected state"
            Expect.equal warehouse [] "Warehouse is not empty"
            Expect.equal port [] "Port is not empty"
        }

        test "After driving one hour, a empty truck going to an empty factory with 1 hour remaining transit time should be empty and idling" {
            let warehouse, port, truckState = driveOneHour [] [] { Cargo = None; Location = InTransit { Origin = Port; Destination = Factory; HoursLeftToDestination = 1 }} 1 1
            Expect.equal truckState { Cargo = None; Location = StoppedAt Factory } "Truck is not in the expected state"
            Expect.equal warehouse [] "Warehouse is not empty"
            Expect.equal port [] "Port is not empty"
        }

        test "After driving one hour, an empty truck going to a factory containing cargo for warehouse A and 1 hour remaining transit time should be on the way to the port with the cargo and 1 hour remaining" {
            let warehouse, port, truckState = driveOneHour [ testCargoA ] [] { Cargo = None; Location = InTransit { Origin = Port; Destination = Factory; HoursLeftToDestination = 1 } } 1 1
            Expect.equal truckState { Cargo = Some testCargoA; Location = InTransit { Origin = Factory; Destination = Port; HoursLeftToDestination = 1 } } "Truck is not in the expected state"
            Expect.equal warehouse [] "Warehouse is not empty"
            Expect.equal port [] "Port is not empty"
        }

        test "After driving one hour, an empty truck going to a factory containing cargo for warehouses A and B and 1 hour remaining transit time should be on the way to the port with cargo A and 1 hour remaining, the factory should have a remaining container for warehouse B" {
            let warehouse, port, truckState = driveOneHour [ testCargoA; testCargoB ] [] { Cargo = None; Location = InTransit { Origin = Port; Destination = Factory; HoursLeftToDestination = 1 } } 1 1
            Expect.equal truckState { Cargo = Some testCargoA; Location = InTransit { Origin = Factory; Destination = Port; HoursLeftToDestination = 1 } } "Truck is not in the expected state"
            Expect.equal warehouse [testCargoB] "Warehouse should have a single container for warehouse B"
            Expect.equal port [] "Port is not empty"
        }

        test "After driving one hour, an idling truck at a factory containing cargo for warehouses A and B should have dropped the cargo at the port and be on the way back" {
            let warehouse, port, truckState = driveOneHour [ testCargoA; testCargoB ] [] { Cargo = None; Location = StoppedAt Factory } 1 1
            Expect.equal truckState { Cargo = None; Location = InTransit { Origin = Port; Destination = Factory; HoursLeftToDestination = 1 } } "Truck is not in the expected state"
            Expect.equal warehouse [testCargoB] "Warehouse should have a single container for warehouse B"
            Expect.equal port [testCargoA] "Port should have a single container for warehouse A"
        }

        test "After one hour, an idling truck at an empty factory should still be idling" {
            let warehouse, port, truckState = driveOneHour [] [] { Cargo = None; Location = StoppedAt Factory } 1 1
            Expect.equal truckState { Cargo = None; Location = StoppedAt Factory } "Truck is not in the expected state"
            Expect.equal warehouse [] "Warehouse is not empty"
            Expect.equal port [] "Port is not empty"
        }
  ]

runTests defaultConfig truckTests

let sailOneHour (portOutboundQueue: Cargo list) (shipState : ShipState) (shipId: VehicleId) (hoursElapsed: int)=
    match portOutboundQueue, shipState.Location with
    | [], InTransit t when t.IsArrivingAt Port ->
        logShipArrivalEvent hoursElapsed shipId Port None
        [], { shipState with Location = StoppedAt Port }
    | firstContainer::otherContainers, InTransit t when t.IsArrivingAt Port  ->
        logShipArrivalEvent hoursElapsed shipId Port None
        logShipDepartureEvent hoursElapsed shipId Port WarehouseA (Some firstContainer)
        otherContainers, { shipState with Location = InTransit { Origin = Port; Destination = WarehouseA; HoursLeftToDestination = 4 }; Cargo = Some firstContainer }
    | _, InTransit t when t.IsArrivingAt WarehouseA ->
        logShipArrivalEvent hoursElapsed shipId WarehouseA shipState.Cargo
        logShipDepartureEvent hoursElapsed shipId WarehouseA Port None
        portOutboundQueue, { shipState with Location = InTransit { Origin = WarehouseA; Destination = Port; HoursLeftToDestination = 4 }; Cargo = None}
    | _, InTransit t ->
        portOutboundQueue, { shipState with Location = InTransit (t.ProgressOneHour ()) }
    | firstContainer::otherContainers, StoppedAt Port ->
        logShipDepartureEvent hoursElapsed shipId Port WarehouseA (Some firstContainer)
        otherContainers, { shipState with Location = InTransit { Origin = Port; Destination = WarehouseA; HoursLeftToDestination = 3 }; Cargo = Some firstContainer }
    | [], StoppedAt Port ->
        portOutboundQueue, shipState

let shipTests =
    testList "Ship sailing tests" [

        test "After one hour, a ship sailing to warehouse A with cargo, with 4 hours remaining will still be sailing, with 3 hours remaining" {
            let port, shipState = sailOneHour [] { Cargo = Some testCargoA; Location = InTransit { Origin = Port; Destination = WarehouseA; HoursLeftToDestination = 4 }} 1 1
            Expect.equal shipState { Cargo = Some testCargoA; Location = InTransit { Origin = Port; Destination = WarehouseA; HoursLeftToDestination = 3 }} "Ship is not in the expected state"
            Expect.equal port [] "Port is not empty"
        }

        test "After one hour, a ship sailing to warehouse A with cargo, with 1 hours remaining should be sailing back empty to the port, with 4 hours remaining" {
            let port, shipState = sailOneHour [] { Cargo = Some testCargoA; Location = InTransit { Origin = Port; Destination = WarehouseA; HoursLeftToDestination = 1 }} 1 1
            Expect.equal shipState { Cargo = None; Location = InTransit { Origin = WarehouseA; Destination = Port; HoursLeftToDestination = 4 } } "Ship is not in the expected state"
            Expect.equal port [] "Port is not empty"
        }

        test "After one hour, a ship sailing to an empty port, with 1 hours remaining should be idling " {
            let port, shipState = sailOneHour [] { Cargo = None; Location = InTransit { Origin = WarehouseA; Destination = Port; HoursLeftToDestination = 1 } } 1 1
            Expect.equal shipState { Cargo = None; Location = StoppedAt Port } "Ship is not in the expected state"
            Expect.equal port [] "Port is not empty"
        }

        test "After one hour, a ship sailing to a port with cargo, with 1 hours remaining should be going to warehouse A with the first cargo, with 4 hours remaining" {
            let port, shipState = sailOneHour [testCargoA; testCargoA'] { Cargo = None; Location = InTransit { Origin = WarehouseA; Destination = Port; HoursLeftToDestination = 1 } } 1 1
            Expect.equal shipState { Cargo = Some testCargoA; Location = InTransit { Origin = Port; Destination = WarehouseA; HoursLeftToDestination = 4 } } "Ship is not in the expected state"
            Expect.equal port [testCargoA'] "Port should still have one container"
        }

        test "After one hour, a ship idling in port with cargo available should be going to warehouse A with the first cargo, with 3 hours remaining" {
            let port, shipState = sailOneHour [testCargoA; testCargoA'] { Cargo = None; Location = StoppedAt Port } 1 1
            Expect.equal shipState { Cargo = Some testCargoA; Location = InTransit { Origin = Port; Destination = WarehouseA; HoursLeftToDestination = 3 }} "Ship is not in the expected state"
            Expect.equal port [testCargoA'] "Port should still have one container"
        }
  ]

runTests defaultConfig shipTests

let moveOneHour factoryOutboundQueue portOutboundQueue vehicle elapsed =
    match vehicle with
    | Ship (id, shipState) ->
        let portOutboundQueueAfterOneHour, shipStateAfterOneHour = sailOneHour portOutboundQueue shipState id elapsed
        factoryOutboundQueue, portOutboundQueueAfterOneHour, Ship (id, shipStateAfterOneHour)
    | Truck (id, truckState) ->
        let factoryOutboundQueueAfterOneHour, portOutboundQueueAfterOneHour, truckStateAfterOneHour = driveOneHour factoryOutboundQueue portOutboundQueue truckState id elapsed
        factoryOutboundQueueAfterOneHour, portOutboundQueueAfterOneHour, Truck (id, truckStateAfterOneHour)

let initialState = [
    Ship (2, { Cargo = None; Location = StoppedAt Port })
    Truck (0, { Cargo = None; Location = StoppedAt Factory })
    Truck (1, { Cargo = None; Location = StoppedAt Factory })
]

let createCargoListFromInput (input: string) =
    input
    |> Seq.mapi (fun index character ->
        match character with
        | 'A' -> { Id = index; Destination = Warehouse.A }
        | 'B' -> { Id = index; Destination = Warehouse.B }
        | invalid -> failwithf "Invalid character %c" invalid)
    |> Seq.toList

let deliver destinations =
    let cargoList = destinations |> createCargoListFromInput
    let rec passTimeUntilAllContainersAreDelivered (factoryOutboundQueue: Cargo list) (portOutboundQueue: Cargo list) (vehicles : Vehicle list) hoursElapsed =
        // printfn "%i\r\n%O\r\n%O\r\n\%O" hoursElapsed factoryOutboundQueue portOutboundQueue vehicleStates

        if vehicles |> List.exists (fun v -> match v with | Truck (_, truckState) -> truckState.Cargo.IsSome | Ship (_, shipState) -> shipState.Cargo.IsSome) || not (factoryOutboundQueue |> List.isEmpty) || not (portOutboundQueue |> List.isEmpty) then
            let letOneHourPass (currentFactoryOutboundQueue, currentPortOutboundQueue) vehicleState =
                let factoryOutBoundQueueAfterOneHour, portOutboundQueueAfterOneHour, vehicleStateAfterOneHour = moveOneHour currentFactoryOutboundQueue currentPortOutboundQueue vehicleState hoursElapsed
                vehicleStateAfterOneHour, (factoryOutBoundQueueAfterOneHour,portOutboundQueueAfterOneHour)

            let vehicleStatesAfterOneHour, (newFactoryOutboundQueue, newPortOutboundQueue) = vehicles |> List.mapFold letOneHourPass (factoryOutboundQueue, portOutboundQueue)
            passTimeUntilAllContainersAreDelivered newFactoryOutboundQueue newPortOutboundQueue vehicleStatesAfterOneHour (hoursElapsed + 1)
        else
            hoursElapsed

    passTimeUntilAllContainersAreDelivered cargoList [] initialState 0


let deliveryTests =
    testList "Delivery tests" [

        test "Delivering A takes 5 hours" {
            let hoursElapsed = deliver "A"
            Expect.equal hoursElapsed 5 "Hours elapsed did not match expected time span"
        }

        test "Delivering B takes 5 hours" {
            let hoursElapsed = deliver "B"
            Expect.equal hoursElapsed 5 "Hours elapsed did not match expected time span"
        }

        test "Delivering AB takes 5 hours" {
            let hoursElapsed = deliver "AB"
            Expect.equal hoursElapsed 5 "Hours elapsed did not match expected time span"
        }

        test "Delivering BB takes 5 hours" {
            let hoursElapsed = deliver "BB"
            Expect.equal hoursElapsed 5 "Hours elapsed did not match expected time span"
        }

        test "Delivering ABB takes 7 hours" {
            let hoursElapsed = deliver "ABB"
            Expect.equal hoursElapsed 7 "Hours elapsed did not match expected time span"
        }
    ]

runTests defaultConfig deliveryTests

deliver "AB"

// AABABBAB
deliver "AABABBAB"

// ABBBABAAABBB
deliver "ABBBABAAABBB"