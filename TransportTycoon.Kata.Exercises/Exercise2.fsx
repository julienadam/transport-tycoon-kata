#load @"..\.paket\load\netcoreapp3.0\main.group.fsx"

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

    type Cargo = 
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
            cargo: Cargo list
        }
        
    open FSharp.Json
        
    let private config = JsonConfig.create (unformatted = true, serializeNone = SerializeNone.Omit)
   
    let logEvent evt =
         Json.serializeEx config evt

    
    let private sampleDepartEvent = 
        {
            event = EventType.DEPART
            time = 12
            transport_id = 3
            kind = TransportKind.SHIP
            location = Location.FACTORY
            destination = Some Location.PORT
            cargo = [
                { cargo_id = 42; destination = Location.B; origin = Location.A }
                { cargo_id = 43; destination = Location.A; origin = Location.FACTORY }
            ]
        }
    
    let private sampleArriveEvent = 
        {
            event = EventType.DEPART
            time = 12
            transport_id = 3
            kind = TransportKind.SHIP
            location = Location.FACTORY
            destination = None
            cargo = [
                { cargo_id = 42; destination = Location.B; origin = Location.A }
                { cargo_id = 43; destination = Location.A; origin = Location.FACTORY }
            ]
        }

    //logEvent sampleDepartEvent
    //logEvent sampleArriveEvent
    
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
| Idle

type ShipDestination =
| WarehouseA
| Port
| Idle

type TruckState =
    {
        Destination : TruckDestination
        HoursLeftToDestination: int
        Cargo: Cargo option
    }

type ShipState =
    {
        Destination : ShipDestination
        HoursLeftToDestination : int
        Cargo : Cargo option
    }

type VehicleState =
| TruckState of TruckState
| ShipState of ShipState


let FactoryToPortTransitTime = 1
let PortToFactoryTransitTime = FactoryToPortTransitTime
let FactoryToWarehouseBTransitTime = 5
let WarehouseBToFactoryTransitTime = FactoryToWarehouseBTransitTime 


let driveOneHour (factoryOutboundQueue: Cargo list) (portOutboundQueue: Cargo list) (truckState : TruckState) =
    match truckState, factoryOutboundQueue with
    | state, factoryQueue when state.Cargo.IsNone ->
        match state.HoursLeftToDestination, factoryQueue with 
        | 0, [] -> factoryOutboundQueue, portOutboundQueue, truckState
        | 0, cargoToPickup::restOfFactoryQueue -> 
            match cargoToPickup.Destination with 
            | Warehouse.A ->
                restOfFactoryQueue, portOutboundQueue |> List.append [ cargoToPickup ], { state with Cargo = None; Destination = TruckDestination.Factory; HoursLeftToDestination = PortToFactoryTransitTime }
            | Warehouse.B -> 
                restOfFactoryQueue, portOutboundQueue, { state with Cargo = Some cargoToPickup; Destination = TruckDestination.WarehouseB; HoursLeftToDestination = FactoryToWarehouseBTransitTime - 1 }
        | 1, _ -> 
            match factoryQueue with 
            | [] -> factoryOutboundQueue, portOutboundQueue, { state with HoursLeftToDestination = 0; Cargo = None; Destination = TruckDestination.Idle }
            | cargoToPickup :: restOfFactoryQueue -> 
                let newDestination, transitTime = match cargoToPickup.Destination with | Warehouse.A -> TruckDestination.Port, FactoryToPortTransitTime | Warehouse.B -> TruckDestination.WarehouseB, FactoryToWarehouseBTransitTime
                restOfFactoryQueue, portOutboundQueue, { state with Cargo = Some cargoToPickup; Destination = newDestination; HoursLeftToDestination = transitTime }
        | hoursLeftToDestination, _ -> factoryOutboundQueue, portOutboundQueue, { state with HoursLeftToDestination = hoursLeftToDestination - 1 }
    | state, factoryQueue when state.Cargo.IsSome ->
        match state.Destination, state.HoursLeftToDestination with
        | _, hoursLeftToDestination when hoursLeftToDestination > 1 -> factoryOutboundQueue, portOutboundQueue, { state with HoursLeftToDestination = hoursLeftToDestination - 1 }
        | _, 0 -> factoryOutboundQueue, portOutboundQueue, truckState
        | currentDestination, 1 ->
            match currentDestination with   
            | Factory -> 
                failwith "Not possible"
            | TruckDestination.Port ->
                factoryQueue, List.concat [portOutboundQueue; [ state.Cargo.Value ]] , { state with Cargo = None; Destination = TruckDestination.Factory; HoursLeftToDestination = PortToFactoryTransitTime }
            | TruckDestination.WarehouseB ->
                factoryQueue, portOutboundQueue, { state with Cargo = None; Destination = TruckDestination.Factory; HoursLeftToDestination = WarehouseBToFactoryTransitTime }
            | TruckDestination.Idle ->
                factoryOutboundQueue, portOutboundQueue, truckState


open Expecto

let testCargoA = { Id = 1; Destination = Warehouse.A }
let testCargoA' = { Id = 2; Destination = Warehouse.A }
let testCargoB = { Id = 100; Destination = Warehouse.B }
let testCargoB' = { Id = 101; Destination = Warehouse.B }
let testCargoB'' = { Id = 102; Destination = Warehouse.B }

let truckTests =

    testList "Truck driving tests" [
        test "After driving one hour, a truck with cargo going to the port should be empty, going back to the factory and the cargo should be at the port warehouse" {
        let warehouse, port, truckState = driveOneHour [] [] { Cargo = Some testCargoA; Destination = TruckDestination.Port; HoursLeftToDestination = 1 }
        Expect.equal truckState { Cargo = None; Destination = TruckDestination.Factory; HoursLeftToDestination = 1 } "Truck is not in the expected state"
        Expect.equal warehouse [] "Warehouse is not empty"
        Expect.equal port [testCargoA] "Port does not have exactly one container"
        }

        test "After driving one hour, a truck with cargo going to warehouse B with 5 hours remaining transit time should be in the same state but with 4 hours left" {
            let warehouse, port, truckState = driveOneHour [] [] { Cargo = Some testCargoB; Destination = TruckDestination.WarehouseB; HoursLeftToDestination = 5 }
            Expect.equal truckState { Cargo = Some testCargoB; Destination = TruckDestination.WarehouseB; HoursLeftToDestination = 4 } "Truck is not in the expected state"
            Expect.equal warehouse [] "Warehouse is not empty"
            Expect.equal port [] "Port is not empty"
        }

        test "After driving one hour, a truck with cargo going to warehouse B with 1 hour remaining transit time should be empty, going back to the factory with 5 hours left" {
            let warehouse, port, truckState = driveOneHour [] [] { Cargo = Some testCargoB; Destination = TruckDestination.WarehouseB; HoursLeftToDestination = 1 }
            Expect.equal truckState { Cargo = None; Destination = TruckDestination.Factory; HoursLeftToDestination = 5 } "Truck is not in the expected state"
            Expect.equal warehouse [] "Warehouse is not empty"
            Expect.equal port [] "Port is not empty"
        }

        test "After driving one hour, a empty truck going to an empty factory with 1 hour remaining transit time should be empty and idling" {
            let warehouse, port, truckState = driveOneHour [] [] { Cargo = None; Destination = TruckDestination.Factory; HoursLeftToDestination = 1 }
            Expect.equal truckState { Cargo = None; Destination = TruckDestination.Idle; HoursLeftToDestination = 0 } "Truck is not in the expected state"
            Expect.equal warehouse [] "Warehouse is not empty"
            Expect.equal port [] "Port is not empty"
        }

        test "After driving one hour, an empty truck going to a factory containing cargo for warehouse A and 1 hour remaining transit time should be on the way to the port with the cargo and 1 hour remaining" {
            let warehouse, port, truckState = driveOneHour [ testCargoA ] [] { Cargo = None; Destination = TruckDestination.Factory; HoursLeftToDestination = 1 }
            Expect.equal truckState { Cargo = Some testCargoA; Destination = TruckDestination.Port; HoursLeftToDestination = 1 } "Truck is not in the expected state"
            Expect.equal warehouse [] "Warehouse is not empty"
            Expect.equal port [] "Port is not empty"
        }
        
        test "After driving one hour, an empty truck going to a factory containing cargo for warehouses A and B and 1 hour remaining transit time should be on the way to the port with cargo A and 1 hour remaining, the factory should have a remaining container for warehouse B" {
            let warehouse, port, truckState = driveOneHour [ testCargoA; testCargoB ] [] { Cargo = None; Destination = TruckDestination.Factory; HoursLeftToDestination = 1 }
            Expect.equal truckState { Cargo = Some testCargoA; Destination = TruckDestination.Port; HoursLeftToDestination = 1 } "Truck is not in the expected state"
            Expect.equal warehouse [testCargoB] "Warehouse should have a single container for warehouse B"
            Expect.equal port [] "Port is not empty"
        }
               
        test "After driving one hour, an idling truck at a factory containing cargo for warehouses A and B should have dropped the cargo at the port and be on the way back" {
            let warehouse, port, truckState = driveOneHour [ testCargoA; testCargoB ] [] { Cargo = None; Destination = TruckDestination.Idle; HoursLeftToDestination = 0 }
            Expect.equal truckState { Cargo = None; Destination = TruckDestination.Factory; HoursLeftToDestination = 1 } "Truck is not in the expected state"
            Expect.equal warehouse [testCargoB] "Warehouse should have a single container for warehouse B"
            Expect.equal port [testCargoA] "Port should have a single container for warehouse A"
        }
        
        test "After one hour, an idling truck at an empty factory should still be idling" {
            let warehouse, port, truckState = driveOneHour [] [] { Cargo = None; Destination = TruckDestination.Idle; HoursLeftToDestination = 0 }
            Expect.equal truckState { Cargo = None; Destination = TruckDestination.Idle; HoursLeftToDestination = 0 } "Truck is not in the expected state"
            Expect.equal warehouse [] "Warehouse is not empty"
            Expect.equal port [] "Port is not empty"
        }
  ]

runTests defaultConfig truckTests

let sailOneHour (portOutboundQueue: Cargo list) (shipState : ShipState) =
    let inline isArrivingAtPort s = s.HoursLeftToDestination = 1 && s.Destination = ShipDestination.Port
    let inline isArrivingAtWarehouseA s = s.HoursLeftToDestination = 1 && s.Destination = ShipDestination.WarehouseA
    let inline isAtSea s = s.HoursLeftToDestination > 1
    let inline isIdling s = s.Destination = ShipDestination.Idle

    match portOutboundQueue, shipState with
    | [], s when s |> isArrivingAtPort ->
        [], { shipState with Destination = ShipDestination.Idle; HoursLeftToDestination = 0 }
    | firstContainer::otherContainers, s when s |> isArrivingAtPort ->
        otherContainers, { shipState with Destination = ShipDestination.WarehouseA; HoursLeftToDestination = 4; Cargo = Some firstContainer }
    | _, s when s |> isArrivingAtWarehouseA ->
        portOutboundQueue, { s with Destination = ShipDestination.Port; HoursLeftToDestination = 4; Cargo = None}
    | _, s when s |> isAtSea ->
        portOutboundQueue, { s with HoursLeftToDestination = s.HoursLeftToDestination - 1 }
    | firstContainer::otherContainers, s when s |> isIdling ->
        otherContainers, { shipState with Destination = ShipDestination.WarehouseA; HoursLeftToDestination = 4; Cargo = Some firstContainer }
    | [], s when s |> isIdling ->
        portOutboundQueue, shipState
     
let shipTests =
    testList "Ship sailing tests" [
        
        test "After one hour, a ship sailing to warehouse A with cargo, with 4 hours remaining will still be sailing, with 3 hours remaining" {
            let port, shipState = sailOneHour [] { Cargo = Some testCargoA; Destination = ShipDestination.WarehouseA; HoursLeftToDestination = 4 }
            Expect.equal shipState { Cargo = Some testCargoA; Destination = ShipDestination.WarehouseA; HoursLeftToDestination = 3 } "Ship is not in the expected state"
            Expect.equal port [] "Port is not empty"
        }

        test "After one hour, a ship sailing to warehouse A with cargo, with 1 hours remaining should be sailing back empty to the port, with 4 hours remaining" {
            let port, shipState = sailOneHour [] { Cargo = Some testCargoA; Destination = ShipDestination.WarehouseA; HoursLeftToDestination = 1 }
            Expect.equal shipState { Cargo = None; Destination = ShipDestination.Port; HoursLeftToDestination = 4 } "Ship is not in the expected state"
            Expect.equal port [] "Port is not empty"
        }

        test "After one hour, a ship sailing to an empty port, with 1 hours remaining should be idling " {
            let port, shipState = sailOneHour [] { Cargo = None; Destination = ShipDestination.Port; HoursLeftToDestination = 1 }
            Expect.equal shipState { Cargo = None; Destination = ShipDestination.Idle; HoursLeftToDestination = 0 } "Ship is not in the expected state"
            Expect.equal port [] "Port is not empty"
        }

        test "After one hour, a ship sailing to a port with cargo, with 1 hours remaining should be going to warehouse A with the first cargo, with 4 hours remaining" {
            let port, shipState = sailOneHour [testCargoA; testCargoA'] { Cargo = None; Destination = ShipDestination.Port; HoursLeftToDestination = 1 }
            Expect.equal shipState { Cargo = Some testCargoA; Destination = ShipDestination.WarehouseA; HoursLeftToDestination = 4 } "Ship is not in the expected state"
            Expect.equal port [testCargoA'] "Port should still have one container"
        }

        test "After one hour, a ship idling in port with cargo available should be going to warehouse A with the first cargo, with 4 hours remaining" {
            let port, shipState = sailOneHour [testCargoA; testCargoA'] { Cargo = None; Destination = ShipDestination.Idle; HoursLeftToDestination = 0 }
            Expect.equal shipState { Cargo = Some testCargoA; Destination = ShipDestination.WarehouseA; HoursLeftToDestination = 4} "Ship is not in the expected state"
            Expect.equal port [testCargoA'] "Port should still have one container"
        }
  ]

runTests defaultConfig shipTests

let moveOneHour factoryOutboundQueue portOutboundQueue vehicleState =
    match vehicleState with
    | ShipState shipState -> 
        let portOutboundQueueAfterOneHour, shipStateAfterOneHour = sailOneHour portOutboundQueue shipState
        factoryOutboundQueue, portOutboundQueueAfterOneHour, ShipState shipStateAfterOneHour
    | TruckState truckState -> 
        let factoryOutboundQueueAfterOneHour, portOutboundQueueAfterOneHour, truckStateAfterOneHour = driveOneHour factoryOutboundQueue portOutboundQueue truckState
        factoryOutboundQueueAfterOneHour, portOutboundQueueAfterOneHour, TruckState truckStateAfterOneHour

let initialState = [
    TruckState { Cargo = None; Destination = TruckDestination.Idle; HoursLeftToDestination = 0 }
    TruckState { Cargo = None; Destination = TruckDestination.Idle; HoursLeftToDestination = 0 }
    ShipState { Cargo = None; Destination = ShipDestination.Idle; HoursLeftToDestination = 0 }
]

let deliver destinations =
    let rec passTimeUntilAllContainersAreDelivered (factoryOutboundQueue: Cargo list) (portOutboundQueue: Cargo list) (vehicleStates : VehicleState list) hoursElapsed =
        // printfn "%i\r\n%O\r\n%O\r\n\%O" hoursElapsed factoryOutboundQueue portOutboundQueue vehicleStates

        if vehicleStates |> List.exists (fun v -> match v with | TruckState ts -> ts.Cargo.IsSome | ShipState ss -> ss.Cargo.IsSome) || not (factoryOutboundQueue |> List.isEmpty) || not (portOutboundQueue |> List.isEmpty) then
            let letOneHourPass (currentFactoryOutboundQueue, currentPortOutboundQueue) vehicleState =
                let factoryOutBoundQueueAfterOneHour, portOutboundQueueAfterOneHour, vehicleStateAfterOneHour = moveOneHour currentFactoryOutboundQueue currentPortOutboundQueue vehicleState
                vehicleStateAfterOneHour, (factoryOutBoundQueueAfterOneHour,portOutboundQueueAfterOneHour)

            let vehicleStatesAfterOneHour, (newFactoryOutboundQueue, newPortOutboundQueue) = vehicleStates |> List.mapFold letOneHourPass (factoryOutboundQueue, portOutboundQueue)
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

let createCargoListFromInput (input:string) =
    input 
    |> Seq.mapi (fun i c -> match c with | 'A' -> { Id = i; Destination = Warehouse.A } | 'B' -> { Id = i; Destination = Warehouse.B } | x -> failwithf "Invalid character %c" x)
    |> Seq.toList

// AABABBAB
deliver ("AABABBAB" |> createCargoListFromInput)

// ABBBABAAABBB
deliver ("ABBBABAAABBB" |> createCargoListFromInput)