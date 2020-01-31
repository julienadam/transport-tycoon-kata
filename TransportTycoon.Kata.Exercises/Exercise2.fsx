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

type TruckDestination =
| WarehouseB
| Port
| Factory

type ShipDestination =
| WarehouseA
| Port

type VehicleId = int


module EventLogging =

    type EventType = 
        | DEPART
        | ARRIVE

    type TransportKind =
        | TRUCK
        | SHIP

    type Location =
        | FACTORY
        | PORT
        | A
        | B

    type CargoLogInfo = 
        {
            cargo_id: int
            destination: Location
            origin: Location
        }

    type Event =
        {
            event: EventType
            time: int
            transport_id: int
            kind: TransportKind
            location: Location
            destination: Location option
            cargo: CargoLogInfo list
        }
        
    open FSharp.Json
        
    let private config = JsonConfig.create (unformatted = true, serializeNone = SerializeNone.Omit)
   
    let private logEvent evt =
         Json.serializeEx config evt

    let inline private truckDestinationToLocation (d: TruckDestination) =
        match d with
        | WarehouseB -> B
        | Factory -> FACTORY
        | TruckDestination.Port -> PORT


    let inline private shipDestinationToLocation (d: ShipDestination) =
        match d with
        | WarehouseA -> A
        | Port -> PORT

    let inline private cargoToCargoInfo (cargo:Cargo) =
        {
            cargo_id = cargo.Id
            destination = match cargo.Destination with | Warehouse.A -> A | Warehouse.B -> B
            origin = FACTORY
        }

    let logTruckDepartureEvent (time: int) (vehicleId: VehicleId) (location: TruckDestination) (destination: TruckDestination) (cargo: Cargo option) =
        let event = 
            {
                event = DEPART
                time = time
                transport_id = vehicleId
                kind = TRUCK
                location = location |> truckDestinationToLocation
                destination = Some (destination |> truckDestinationToLocation)
                cargo = match cargo with | None -> [] | Some c -> [c |> cargoToCargoInfo]
            }
        logEvent event |> System.Console.WriteLine

    let logTruckArrivalEvent (time: int) (vehicleId: VehicleId) (location: TruckDestination) (cargo: Cargo option) =
        let event = 
            {
                event = ARRIVE
                time = time
                transport_id = vehicleId
                kind = TRUCK
                location = location |> truckDestinationToLocation
                destination = None
                cargo = match cargo with | None -> [] | Some c -> [c |> cargoToCargoInfo]
            }
        logEvent event |> System.Console.WriteLine

    let logShipDepartureEvent (time: int) (vehicleId: VehicleId) (location: ShipDestination) (destination: ShipDestination) (cargo: Cargo option) =
        let event = 
            {
                event = DEPART
                time = time
                transport_id = vehicleId
                kind = SHIP
                location = location |> shipDestinationToLocation
                destination = Some (destination |> shipDestinationToLocation)
                cargo = match cargo with | None -> [] | Some c -> [c |> cargoToCargoInfo]
            }
        logEvent event |> System.Console.WriteLine

    let logShipArrivalEvent (time: int) (vehicleId: VehicleId) (location: ShipDestination) (cargo: Cargo option) =
        let event = 
            {
                event = ARRIVE
                time = time
                transport_id = vehicleId
                kind = SHIP
                location = location |> shipDestinationToLocation
                destination = None
                cargo = match cargo with | None -> [] | Some c -> [c |> cargoToCargoInfo]
            }
        logEvent event |> System.Console.WriteLine


type TruckState =
    {
        Destination : TruckDestination option
        HoursLeftToDestination: int
        Cargo: Cargo option
    }

type ShipState =
    {
        Destination : ShipDestination option
        HoursLeftToDestination : int
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
    match truckState, factoryOutboundQueue with
    | { Cargo = None; HoursLeftToDestination = 0 }, [] ->
        factoryOutboundQueue, portOutboundQueue, truckState
    | { Cargo = None; HoursLeftToDestination = 0 }, cargoToPickup::restOfFactoryQueue ->
        match cargoToPickup.Destination with
        | Warehouse.A ->
            logTruckDepartureEvent hoursElapsed truckId Factory TruckDestination.Port (Some cargoToPickup)
            restOfFactoryQueue, portOutboundQueue |> List.append [ cargoToPickup ], { truckState with Cargo = None; Destination = Some Factory; HoursLeftToDestination = FactoryToPortTransitTime }
        | Warehouse.B ->
            logTruckDepartureEvent hoursElapsed truckId Factory TruckDestination.WarehouseB (Some cargoToPickup)
            restOfFactoryQueue, portOutboundQueue, { truckState with Cargo = Some cargoToPickup; Destination = Some WarehouseB; HoursLeftToDestination = FactoryToWarehouseBTransitTime - 1 }
    | { Cargo = None; HoursLeftToDestination = 1 }, [] ->
        logTruckArrivalEvent hoursElapsed truckId Factory None
        factoryOutboundQueue, portOutboundQueue, { truckState with HoursLeftToDestination = 0; Cargo = None; Destination = None }
    | { Cargo = None; HoursLeftToDestination = 1 }, cargoToPickup :: restOfFactoryQueue ->
        logTruckArrivalEvent hoursElapsed truckId Factory None
        let newDestination, transitTime = match cargoToPickup.Destination with | Warehouse.A -> TruckDestination.Port, FactoryToPortTransitTime | Warehouse.B -> WarehouseB, FactoryToWarehouseBTransitTime
        logTruckDepartureEvent hoursElapsed truckId Factory newDestination (Some cargoToPickup)
        restOfFactoryQueue, portOutboundQueue, { truckState with Cargo = Some cargoToPickup; Destination = Some newDestination; HoursLeftToDestination = transitTime }
    | { Cargo = None }, _ ->
        factoryOutboundQueue, portOutboundQueue, { truckState with HoursLeftToDestination = truckState.HoursLeftToDestination - 1 }
    | { Cargo = Some _; HoursLeftToDestination = 0 }, _ ->
        factoryOutboundQueue, portOutboundQueue, truckState
    | { Cargo = Some _; HoursLeftToDestination = 1; Destination = Some Factory }, _ ->
        failwith "Not possible"
    | { Cargo = Some cargo; HoursLeftToDestination = 1; Destination = Some TruckDestination.Port }, _ ->
        logTruckArrivalEvent hoursElapsed truckId TruckDestination.Port (Some cargo)
        logTruckDepartureEvent hoursElapsed truckId TruckDestination.Port TruckDestination.Factory None
        factoryOutboundQueue, List.concat [portOutboundQueue; [ cargo ]] , { truckState with Cargo = None; Destination = Some Factory; HoursLeftToDestination = PortToFactoryTransitTime }
    | { Cargo = Some cargo; HoursLeftToDestination = 1; Destination = Some WarehouseB }, _ ->
        logTruckArrivalEvent hoursElapsed truckId TruckDestination.WarehouseB (Some cargo)
        logTruckDepartureEvent hoursElapsed truckId TruckDestination.WarehouseB Factory None
        factoryOutboundQueue, portOutboundQueue, { truckState with Cargo = None; Destination = Some Factory; HoursLeftToDestination = WarehouseBToFactoryTransitTime }
    | { Cargo = Some _; HoursLeftToDestination = 1; Destination = None }, _ ->
        factoryOutboundQueue, portOutboundQueue, truckState
    | { Cargo = Some _ } , _ ->
        factoryOutboundQueue, portOutboundQueue, { truckState with HoursLeftToDestination = truckState.HoursLeftToDestination - 1 }


open Expecto

let testCargoA = { Id = 1; Destination = Warehouse.A }
let testCargoA' = { Id = 2; Destination = Warehouse.A }
let testCargoB = { Id = 100; Destination = Warehouse.B }
let testCargoB' = { Id = 101; Destination = Warehouse.B }

let truckTests =

    testList "Truck driving tests" [
        test "After driving one hour, a truck with cargo going to the port should be empty, going back to the factory and the cargo should be at the port warehouse" {
        let warehouse, port, truckState = driveOneHour [] [] { Cargo = Some testCargoA; Destination = Some TruckDestination.Port; HoursLeftToDestination = 1 } 1 1
        Expect.equal truckState { Cargo = None; Destination = Some TruckDestination.Factory; HoursLeftToDestination = 1 } "Truck is not in the expected state"
        Expect.equal warehouse [] "Warehouse is not empty"
        Expect.equal port [testCargoA] "Port does not have exactly one container"
        }

        test "After driving one hour, a truck with cargo going to warehouse B with 5 hours remaining transit time should be in the same state but with 4 hours left" {
            let warehouse, port, truckState = driveOneHour [] [] { Cargo = Some testCargoB; Destination = Some TruckDestination.WarehouseB; HoursLeftToDestination = 5 } 1 1
            Expect.equal truckState { Cargo = Some testCargoB; Destination = Some TruckDestination.WarehouseB; HoursLeftToDestination = 4 } "Truck is not in the expected state"
            Expect.equal warehouse [] "Warehouse is not empty"
            Expect.equal port [] "Port is not empty"
        }

        test "After driving one hour, a truck with cargo going to warehouse B with 1 hour remaining transit time should be empty, going back to the factory with 5 hours left" {
            let warehouse, port, truckState = driveOneHour [] [] { Cargo = Some testCargoB; Destination = Some TruckDestination.WarehouseB; HoursLeftToDestination = 1 } 1 1
            Expect.equal truckState { Cargo = None; Destination = Some TruckDestination.Factory; HoursLeftToDestination = 5 } "Truck is not in the expected state"
            Expect.equal warehouse [] "Warehouse is not empty"
            Expect.equal port [] "Port is not empty"
        }

        test "After driving one hour, a empty truck going to an empty factory with 1 hour remaining transit time should be empty and idling" {
            let warehouse, port, truckState = driveOneHour [] [] { Cargo = None; Destination = Some TruckDestination.Factory; HoursLeftToDestination = 1 } 1 1
            Expect.equal truckState { Cargo = None; Destination = None; HoursLeftToDestination = 0 } "Truck is not in the expected state"
            Expect.equal warehouse [] "Warehouse is not empty"
            Expect.equal port [] "Port is not empty"
        }

        test "After driving one hour, an empty truck going to a factory containing cargo for warehouse A and 1 hour remaining transit time should be on the way to the port with the cargo and 1 hour remaining" {
            let warehouse, port, truckState = driveOneHour [ testCargoA ] [] { Cargo = None; Destination = Some TruckDestination.Factory; HoursLeftToDestination = 1 } 1 1
            Expect.equal truckState { Cargo = Some testCargoA; Destination = Some TruckDestination.Port; HoursLeftToDestination = 1 } "Truck is not in the expected state"
            Expect.equal warehouse [] "Warehouse is not empty"
            Expect.equal port [] "Port is not empty"
        }
        
        test "After driving one hour, an empty truck going to a factory containing cargo for warehouses A and B and 1 hour remaining transit time should be on the way to the port with cargo A and 1 hour remaining, the factory should have a remaining container for warehouse B" {
            let warehouse, port, truckState = driveOneHour [ testCargoA; testCargoB ] [] { Cargo = None; Destination = Some TruckDestination.Factory; HoursLeftToDestination = 1 } 1 1
            Expect.equal truckState { Cargo = Some testCargoA; Destination = Some TruckDestination.Port; HoursLeftToDestination = 1 } "Truck is not in the expected state"
            Expect.equal warehouse [testCargoB] "Warehouse should have a single container for warehouse B"
            Expect.equal port [] "Port is not empty"
        }
               
        test "After driving one hour, an idling truck at a factory containing cargo for warehouses A and B should have dropped the cargo at the port and be on the way back" {
            let warehouse, port, truckState = driveOneHour [ testCargoA; testCargoB ] [] { Cargo = None; Destination = None; HoursLeftToDestination = 0 } 1 1
            Expect.equal truckState { Cargo = None; Destination = Some TruckDestination.Factory; HoursLeftToDestination = 1 } "Truck is not in the expected state"
            Expect.equal warehouse [testCargoB] "Warehouse should have a single container for warehouse B"
            Expect.equal port [testCargoA] "Port should have a single container for warehouse A"
        }
        
        test "After one hour, an idling truck at an empty factory should still be idling" {
            let warehouse, port, truckState = driveOneHour [] [] { Cargo = None; Destination = None; HoursLeftToDestination = 0 } 1 1
            Expect.equal truckState { Cargo = None; Destination = None; HoursLeftToDestination = 0 } "Truck is not in the expected state"
            Expect.equal warehouse [] "Warehouse is not empty"
            Expect.equal port [] "Port is not empty"
        }
  ]

runTests defaultConfig truckTests

let sailOneHour (portOutboundQueue: Cargo list) (shipState : ShipState) (shipId: VehicleId) (hoursElapsed: int)=
    let inline isArrivingAtPort s = s.HoursLeftToDestination = 1 && s.Destination = Some ShipDestination.Port
    let inline isArrivingAtWarehouseA s = s.HoursLeftToDestination = 1 && s.Destination = Some ShipDestination.WarehouseA
    let inline isAtSea s = s.HoursLeftToDestination > 1
    let inline isIdling s = s.Destination = None

    match portOutboundQueue, shipState with
    | [], s when s |> isArrivingAtPort ->
        [], { shipState with Destination = None; HoursLeftToDestination = 0 }
    | firstContainer::otherContainers, s when s |> isArrivingAtPort ->
        logShipArrivalEvent hoursElapsed shipId Port None
        logShipDepartureEvent hoursElapsed shipId Port WarehouseA (Some firstContainer)
        otherContainers, { shipState with Destination = Some ShipDestination.WarehouseA; HoursLeftToDestination = 4; Cargo = Some firstContainer }
    | _, s when s |> isArrivingAtWarehouseA ->
        logShipArrivalEvent hoursElapsed shipId WarehouseA s.Cargo
        logShipDepartureEvent hoursElapsed shipId WarehouseA Port None
        portOutboundQueue, { s with Destination = Some ShipDestination.Port; HoursLeftToDestination = 4; Cargo = None}
    | _, s when s |> isAtSea ->
        portOutboundQueue, { s with HoursLeftToDestination = s.HoursLeftToDestination - 1 }
    | firstContainer::otherContainers, s when s |> isIdling ->
        logShipDepartureEvent hoursElapsed shipId Port WarehouseA (Some firstContainer)
        otherContainers, { shipState with Destination = Some ShipDestination.WarehouseA; HoursLeftToDestination = 4; Cargo = Some firstContainer }
    | [], s when s |> isIdling ->
        portOutboundQueue, shipState

let shipTests =
    testList "Ship sailing tests" [

        test "After one hour, a ship sailing to warehouse A with cargo, with 4 hours remaining will still be sailing, with 3 hours remaining" {
            let port, shipState = sailOneHour [] { Cargo = Some testCargoA; Destination = Some ShipDestination.WarehouseA; HoursLeftToDestination = 4 } 1 1
            Expect.equal shipState { Cargo = Some testCargoA; Destination = Some ShipDestination.WarehouseA; HoursLeftToDestination = 3 } "Ship is not in the expected state"
            Expect.equal port [] "Port is not empty"
        }

        test "After one hour, a ship sailing to warehouse A with cargo, with 1 hours remaining should be sailing back empty to the port, with 4 hours remaining" {
            let port, shipState = sailOneHour [] { Cargo = Some testCargoA; Destination = Some ShipDestination.WarehouseA; HoursLeftToDestination = 1 } 1 1
            Expect.equal shipState { Cargo = None; Destination = Some ShipDestination.Port; HoursLeftToDestination = 4 } "Ship is not in the expected state"
            Expect.equal port [] "Port is not empty"
        }

        test "After one hour, a ship sailing to an empty port, with 1 hours remaining should be idling " {
            let port, shipState = sailOneHour [] { Cargo = None; Destination = Some ShipDestination.Port; HoursLeftToDestination = 1 } 1 1
            Expect.equal shipState { Cargo = None; Destination = None; HoursLeftToDestination = 0 } "Ship is not in the expected state"
            Expect.equal port [] "Port is not empty"
        }

        test "After one hour, a ship sailing to a port with cargo, with 1 hours remaining should be going to warehouse A with the first cargo, with 4 hours remaining" {
            let port, shipState = sailOneHour [testCargoA; testCargoA'] { Cargo = None; Destination = Some ShipDestination.Port; HoursLeftToDestination = 1 } 1 1
            Expect.equal shipState { Cargo = Some testCargoA; Destination = Some ShipDestination.WarehouseA; HoursLeftToDestination = 4 } "Ship is not in the expected state"
            Expect.equal port [testCargoA'] "Port should still have one container"
        }

        test "After one hour, a ship idling in port with cargo available should be going to warehouse A with the first cargo, with 4 hours remaining" {
            let port, shipState = sailOneHour [testCargoA; testCargoA'] { Cargo = None; Destination = None; HoursLeftToDestination = 0 } 1 1
            Expect.equal shipState { Cargo = Some testCargoA; Destination = Some ShipDestination.WarehouseA; HoursLeftToDestination = 4} "Ship is not in the expected state"
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
    Truck (0, { Cargo = None; Destination = None; HoursLeftToDestination = 0 })
    Truck (1, { Cargo = None; Destination = None; HoursLeftToDestination = 0 })
    Ship (2, { Cargo = None; Destination = None; HoursLeftToDestination = 0 })
]

let deliver destinations =
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

    passTimeUntilAllContainersAreDelivered destinations [] initialState 0


let deliveryTests =
    testList "Delivery tests" [
            
        test "Delivering A takes 5 hours" {
            let hoursElapsed = deliver [ testCargoA ]
            Expect.equal hoursElapsed 5 "Hours elapsed did not match expected time span"
        }

        test "Delivering B takes 5 hours" {
            let hoursElapsed = deliver [ testCargoB ]
            Expect.equal hoursElapsed 5 "Hours elapsed did not match expected time span"
        }

        test "Delivering AB takes 5 hours" {
            let hoursElapsed = deliver [ testCargoA; testCargoB ]
            Expect.equal hoursElapsed 5 "Hours elapsed did not match expected time span"
        }
        
        test "Delivering BB takes 5 hours" {
            let hoursElapsed = deliver [ testCargoB; testCargoB' ]
            Expect.equal hoursElapsed 5 "Hours elapsed did not match expected time span"
        }

        test "Delivering ABB takes 7 hours" {
            let hoursElapsed = deliver [ testCargoA; testCargoB; testCargoB' ]
            Expect.equal hoursElapsed 7 "Hours elapsed did not match expected time span"
        }
    ]

runTests defaultConfig deliveryTests

let createCargoListFromInput (input: string) =
    input
    |> Seq.mapi (fun index character ->
        match character with
        | 'A' -> { Id = index; Destination = Warehouse.A }
        | 'B' -> { Id = index; Destination = Warehouse.B }
        | invalid -> failwithf "Invalid character %c" invalid)
    |> Seq.toList

deliver ("AB" |> createCargoListFromInput)

// AABABBAB
deliver ("AABABBAB" |> createCargoListFromInput)

// ABBBABAAABBB
deliver ("ABBBABAAABBB" |> createCargoListFromInput)