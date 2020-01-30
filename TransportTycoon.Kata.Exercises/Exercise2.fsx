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

    logEvent sampleDepartEvent
    logEvent sampleArriveEvent
    
type Warehouse =
| A
| B

type TruckDestination =
| WarehouseB
| Port
| Factory
| None

type ShipDestination =
| WarehouseA
| Port
| None

type TruckState =
    {
        Destination : TruckDestination
        HoursLeftToDestination: int
        HasCargo : bool
    }

type ShipState =
    {
        Destination : ShipDestination
        HoursLeftToDestination : int
        HasCargo : bool
    }

type VehicleState =
| TruckState of TruckState
| ShipState of ShipState


let FactoryToPortTransitTime = 1
let PortToFactoryTransitTime = FactoryToPortTransitTime
let FactoryToWarehouseBTransitTime = 5
let WarehouseBToFactoryTransitTime = FactoryToWarehouseBTransitTime 


let driveOneHour (factoryOutboundQueue: Warehouse list) (portOutboundQueue: Warehouse list) (truckState : TruckState) =
    match truckState, factoryOutboundQueue with
    | state, factoryQueue when not state.HasCargo ->
        match state.HoursLeftToDestination, factoryQueue with 
        | 0, [] -> factoryOutboundQueue, portOutboundQueue, truckState
        | 0, cargoToPickup::restOfFactoryQueue -> 
            match cargoToPickup with 
            | Warehouse.A ->
                restOfFactoryQueue, portOutboundQueue |> List.append [ Warehouse.A ], { state with HasCargo = false; Destination = TruckDestination.Factory; HoursLeftToDestination = PortToFactoryTransitTime }
            | Warehouse.B -> 
                restOfFactoryQueue, portOutboundQueue, { state with HasCargo = true; Destination = TruckDestination.WarehouseB; HoursLeftToDestination = FactoryToWarehouseBTransitTime - 1 }
        | 1, _ -> 
            match factoryQueue with 
            | [] -> factoryOutboundQueue, portOutboundQueue, { state with HoursLeftToDestination = 0; HasCargo = false; Destination = TruckDestination.None }
            | cargoToPickup :: restOfFactoryQueue -> 
                let newDestination, transitTime = match cargoToPickup with | Warehouse.A -> TruckDestination.Port, FactoryToPortTransitTime | Warehouse.B -> TruckDestination.WarehouseB, FactoryToWarehouseBTransitTime
                restOfFactoryQueue, portOutboundQueue, { state with HasCargo = true; Destination = newDestination; HoursLeftToDestination = transitTime }
        | hoursLeftToDestination, _ -> factoryOutboundQueue, portOutboundQueue, { state with HoursLeftToDestination = hoursLeftToDestination - 1 }
    | state, factoryQueue when state.HasCargo ->
        match state.Destination, state.HoursLeftToDestination with
        | _, hoursLeftToDestination when hoursLeftToDestination > 1 -> factoryOutboundQueue, portOutboundQueue, { state with HoursLeftToDestination = hoursLeftToDestination - 1 }
        | _, 0 -> factoryOutboundQueue, portOutboundQueue, truckState
        | currentDestination, 1 ->
            match currentDestination with   
            | Factory -> 
                failwith "Not possible"
            | TruckDestination.Port ->
                factoryQueue, List.concat [portOutboundQueue; [ Warehouse.A ]] , { state with HasCargo = false; Destination = TruckDestination.Factory; HoursLeftToDestination = PortToFactoryTransitTime }
            | TruckDestination.WarehouseB ->
                factoryQueue, portOutboundQueue, { state with HasCargo = false; Destination = TruckDestination.Factory; HoursLeftToDestination = WarehouseBToFactoryTransitTime }
            | TruckDestination.None ->
                factoryOutboundQueue, portOutboundQueue, truckState


open Expecto

let truckTests =
    testList "Truck driving tests" [
        test "After driving one hour, a truck with cargo going to the port should be empty, going back to the factory and the cargo should be at the port warehouse" {
        let warehouse, port, truckState = driveOneHour [] [] { HasCargo = true; Destination = TruckDestination.Port; HoursLeftToDestination = 1 }
        Expect.equal truckState { HasCargo = false; Destination = TruckDestination.Factory; HoursLeftToDestination = 1 } "Truck is not in the expected state"
        Expect.equal warehouse [] "Warehouse is not empty"
        Expect.equal port [Warehouse.A] "Port does not have exactly one container"
        }

        test "After driving one hour, a truck with cargo going to warehouse B with 5 hours remaining transit time should be in the same state but with 4 hours left" {
            let warehouse, port, truckState = driveOneHour [] [] { HasCargo = true; Destination = TruckDestination.WarehouseB; HoursLeftToDestination = 5 }
            Expect.equal truckState { HasCargo = true; Destination = TruckDestination.WarehouseB; HoursLeftToDestination = 4 } "Truck is not in the expected state"
            Expect.equal warehouse [] "Warehouse is not empty"
            Expect.equal port [] "Port is not empty"
        }

        test "After driving one hour, a truck with cargo going to warehouse B with 1 hour remaining transit time should be empty, going back to the factory with 5 hours left" {
            let warehouse, port, truckState = driveOneHour [] [] { HasCargo = true; Destination = TruckDestination.WarehouseB; HoursLeftToDestination = 1 }
            Expect.equal truckState { HasCargo = false; Destination = TruckDestination.Factory; HoursLeftToDestination = 5 } "Truck is not in the expected state"
            Expect.equal warehouse [] "Warehouse is not empty"
            Expect.equal port [] "Port is not empty"
        }

        test "After driving one hour, a empty truck going to an empty factory with 1 hour remaining transit time should be empty and idling" {
            let warehouse, port, truckState = driveOneHour [] [] { HasCargo = false; Destination = TruckDestination.Factory; HoursLeftToDestination = 1 }
            Expect.equal truckState { HasCargo = false; Destination = TruckDestination.None; HoursLeftToDestination = 0 } "Truck is not in the expected state"
            Expect.equal warehouse [] "Warehouse is not empty"
            Expect.equal port [] "Port is not empty"
        }

        test "After driving one hour, an empty truck going to a factory containing cargo for warehouse A and 1 hour remaining transit time should be on the way to the port with the cargo and 1 hour remaining" {
            let warehouse, port, truckState = driveOneHour [ Warehouse.A ] [] { HasCargo = false; Destination = TruckDestination.Factory; HoursLeftToDestination = 1 }
            Expect.equal truckState { HasCargo = true; Destination = TruckDestination.Port; HoursLeftToDestination = 1 } "Truck is not in the expected state"
            Expect.equal warehouse [] "Warehouse is not empty"
            Expect.equal port [] "Port is not empty"
        }
        
        test "After driving one hour, an empty truck going to a factory containing cargo for warehouses A and B and 1 hour remaining transit time should be on the way to the port with cargo A and 1 hour remaining, the factory should have a remaining container for warehouse B" {
            let warehouse, port, truckState = driveOneHour [ Warehouse.A; Warehouse.B ] [] { HasCargo = false; Destination = TruckDestination.Factory; HoursLeftToDestination = 1 }
            Expect.equal truckState { HasCargo = true; Destination = TruckDestination.Port; HoursLeftToDestination = 1 } "Truck is not in the expected state"
            Expect.equal warehouse [Warehouse.B] "Warehouse should have a single container for warehouse B"
            Expect.equal port [] "Port is not empty"
        }
               
        test "After driving one hour, an idling truck at a factory containing cargo for warehouses A and B should have dropped the cargo at the port and be on the way back" {
            let warehouse, port, truckState = driveOneHour [ Warehouse.A; Warehouse.B ] [] { HasCargo = false; Destination = TruckDestination.None; HoursLeftToDestination = 0 }
            Expect.equal truckState { HasCargo = false; Destination = TruckDestination.Factory; HoursLeftToDestination = 1 } "Truck is not in the expected state"
            Expect.equal warehouse [Warehouse.B] "Warehouse should have a single container for warehouse B"
            Expect.equal port [Warehouse.A] "Port should have a single container for warehouse A"
        }
        
        test "After one hour, an idling truck at an empty factory should still be idling" {
            let warehouse, port, truckState = driveOneHour [] [] { HasCargo = false; Destination = TruckDestination.None; HoursLeftToDestination = 0 }
            Expect.equal truckState { HasCargo = false; Destination = TruckDestination.None; HoursLeftToDestination = 0 } "Truck is not in the expected state"
            Expect.equal warehouse [] "Warehouse is not empty"
            Expect.equal port [] "Port is not empty"
        }
  ]

runTests defaultConfig truckTests

let sailOneHour (portOutboundQueue: Warehouse list) (shipState : ShipState) =
    let inline isArrivingAtPort s = s.HoursLeftToDestination = 1 && s.Destination = ShipDestination.Port
    let inline isArrivingAtWarehouseA s = s.HoursLeftToDestination = 1 && s.Destination = ShipDestination.WarehouseA
    let inline isAtSea s = s.HoursLeftToDestination > 1
    let inline isIdling s = s.Destination = ShipDestination.None

    match portOutboundQueue, shipState with
    | [], s when s |> isArrivingAtPort ->
        [], { shipState with Destination = ShipDestination.None; HoursLeftToDestination = 0 }
    | _::t, s when s |> isArrivingAtPort ->
        t, { shipState with Destination = ShipDestination.WarehouseA; HoursLeftToDestination = 4; HasCargo = true }
    | _, s when s |> isArrivingAtWarehouseA ->
        portOutboundQueue, { s with Destination = ShipDestination.Port; HoursLeftToDestination = 4; HasCargo = false }
    | _, s when s |> isAtSea ->
        portOutboundQueue, { s with HoursLeftToDestination = s.HoursLeftToDestination - 1 }
    | _::t, s when s |> isIdling ->
        t, { shipState with Destination = ShipDestination.WarehouseA; HoursLeftToDestination = 4; HasCargo = true }
    | [], s when s |> isIdling ->
        portOutboundQueue, shipState
     
let shipTests =
    testList "Ship sailing tests" [
        
        test "After one hour, a ship sailing to warehouse A with cargo, with 4 hours remaining will still be sailing, with 3 hours remaining" {
            let port, shipState = sailOneHour [] { HasCargo = true; Destination = ShipDestination.WarehouseA; HoursLeftToDestination = 4 }
            Expect.equal shipState { HasCargo = true; Destination = ShipDestination.WarehouseA; HoursLeftToDestination = 3 } "Ship is not in the expected state"
            Expect.equal port [] "Port is not empty"
        }

        test "After one hour, a ship sailing to warehouse A with cargo, with 1 hours remaining should be sailing back empty to the port, with 4 hours remaining" {
            let port, shipState = sailOneHour [] { HasCargo = true; Destination = ShipDestination.WarehouseA; HoursLeftToDestination = 1 }
            Expect.equal shipState { HasCargo = false; Destination = ShipDestination.Port; HoursLeftToDestination = 4 } "Ship is not in the expected state"
            Expect.equal port [] "Port is not empty"
        }

        test "After one hour, a ship sailing to an empty port, with 1 hours remaining should be idling " {
            let port, shipState = sailOneHour [] { HasCargo = false; Destination = ShipDestination.Port; HoursLeftToDestination = 1 }
            Expect.equal shipState { HasCargo = false; Destination = ShipDestination.None; HoursLeftToDestination = 0 } "Ship is not in the expected state"
            Expect.equal port [] "Port is not empty"
        }

        test "After one hour, a ship sailing to a port with cargo, with 1 hours remaining should be going to warehouse A with the first cargo, with 4 hours remaining" {
            let port, shipState = sailOneHour [Warehouse.A; Warehouse.A] { HasCargo = false; Destination = ShipDestination.Port; HoursLeftToDestination = 1 }
            Expect.equal shipState { HasCargo = true; Destination = ShipDestination.WarehouseA; HoursLeftToDestination = 4 } "Ship is not in the expected state"
            Expect.equal port [Warehouse.A] "Port should still have one container"
        }

        test "After one hour, a ship idling in port with cargo available should be going to warehouse A with the first cargo, with 4 hours remaining" {
            let port, shipState = sailOneHour [Warehouse.A; Warehouse.A] { HasCargo = false; Destination = ShipDestination.None; HoursLeftToDestination = 0 }
            Expect.equal shipState { HasCargo = true; Destination = ShipDestination.WarehouseA; HoursLeftToDestination = 4} "Ship is not in the expected state"
            Expect.equal port [Warehouse.A] "Port should still have one container"
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
    TruckState { HasCargo = false; Destination = TruckDestination.None; HoursLeftToDestination = 0 }
    TruckState { HasCargo = false; Destination = TruckDestination.None; HoursLeftToDestination = 0 }
    ShipState { HasCargo = false; Destination = ShipDestination.None; HoursLeftToDestination = 0 }
]

let deliver destinations =
    let rec passTimeUntilAllContainersAreDelivered (factoryOutboundQueue: Warehouse list) (portOutboundQueue: Warehouse list) (vehicleStates : VehicleState list) hoursElapsed =
        // printfn "%i\r\n%O\r\n%O\r\n\%O" hoursElapsed factoryOutboundQueue portOutboundQueue vehicleStates

        if vehicleStates |> List.exists (fun v -> match v with | TruckState ts -> ts.HasCargo | ShipState ss -> ss.HasCargo) || not (factoryOutboundQueue |> List.isEmpty) || not (portOutboundQueue |> List.isEmpty) then
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
            let hoursElapsed = deliver [ Warehouse.A ]
            Expect.equal hoursElapsed 5 "Hours elapsed did not match expected time span"
        }

        test "Delivering B takes 5 hours" {
            let hoursElapsed = deliver [ Warehouse.B ]
            Expect.equal hoursElapsed 5 "Hours elapsed did not match expected time span"
        }

        test "Delivering AB takes 5 hours" {
            let hoursElapsed = deliver [ Warehouse.A; Warehouse.B ]
            Expect.equal hoursElapsed 5 "Hours elapsed did not match expected time span"
        }
        
        test "Delivering BB takes 5 hours" {
            let hoursElapsed = deliver [ Warehouse.B; Warehouse.B ]
            Expect.equal hoursElapsed 5 "Hours elapsed did not match expected time span"
        }

        test "Delivering ABB takes 7 hours" {
            let hoursElapsed = deliver [ Warehouse.A; Warehouse.B; Warehouse.B ]
            Expect.equal hoursElapsed 7 "Hours elapsed did not match expected time span"
        }
    ]
    
runTests defaultConfig deliveryTests

// AABABBAB
deliver [ Warehouse.A; Warehouse.A; Warehouse.B; Warehouse.A; Warehouse.B; Warehouse.B; Warehouse.A; Warehouse.B;  ]

// ABBBABAAABBB
deliver [ Warehouse.A; Warehouse.B; Warehouse.B; Warehouse.B; Warehouse.A; Warehouse.B; Warehouse.A; Warehouse.A; Warehouse.A; Warehouse.B; Warehouse.B; Warehouse.B;  ]