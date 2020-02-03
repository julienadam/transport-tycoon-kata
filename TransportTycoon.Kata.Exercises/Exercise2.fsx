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

// type TruckState =
//     {
//         Location : TruckLocation
//         Cargo: Cargo option
//     }
    
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


type LadenParkedTruck = {
    ParkedAt : LandAccessibleLocation
    Cargo: Cargo
}
with 
    member this.UnloadAndDepart () =
        match this.ParkedAt with
        | WarehouseB -> DrivingTruck { Transit = { Origin = WarehouseB; Destination = Factory; HoursLeftToDestination = 4 }; Cargo = None }
        | PortTerminal -> UnladenParkedTruck { ParkedAt = Factory; HoursWaited = 0}
        | Factory -> failwithf "not possible" // TODO: make it impossible
and UnladenParkedTruck = {
    ParkedAt : LandAccessibleLocation
    HoursWaited : int
}
with
    member this.Wait() = 
        UnladenParkedTruck { this with HoursWaited = this.HoursWaited + 1 }
    member this.LoadAndDepart (cargo: Cargo) =
        match cargo.Destination with 
        | Warehouse.A -> LadenParkedTruck { ParkedAt = PortTerminal; Cargo = cargo }
        | Warehouse.B -> DrivingTruck { Transit = { Origin = Factory; Destination = WarehouseB; HoursLeftToDestination = 4 }; Cargo = Some cargo }

and DrivingTruck = {
    Transit: Transit<LandAccessibleLocation>
    Cargo: Cargo option
}
with 
    member this.DriveOn () =
        match this.Transit.HoursLeftToDestination with
        | 1 -> 
            match this.Cargo with
            | Some c -> LadenParkedTruck { ParkedAt = this.Transit.Destination; Cargo = c }
            | None -> UnladenParkedTruck { ParkedAt = this.Transit.Destination; HoursWaited = 0 }
        | _ -> DrivingTruck { this with Transit = this.Transit.ProgressOneHour() }

and TruckState =
| LadenParkedTruck of LadenParkedTruck
| UnladenParkedTruck of UnladenParkedTruck
| DrivingTruck of DrivingTruck

type IdentifiableVehicle<'a> = {
    Id: VehicleId
    State: 'a
}

type Truck = IdentifiableVehicle<TruckState>

let driveOneHour (cargoTracker: CargoTracker) (truck: Truck) (hoursElapsed: int) (sink: Event -> unit) =
    match truck.State with
    | LadenParkedTruck truckState ->
        logTruckArrivalEvent hoursElapsed truck.Id truckState.ParkedAt (Some truckState.Cargo) sink
        logTruckDepartureEvent hoursElapsed truck.Id truckState.ParkedAt Factory None sink
        match truckState.ParkedAt with
        | PortTerminal -> (cargoTracker.DropOffAtPort truckState.Cargo), truckState.UnloadAndDepart()
        | WarehouseB -> (cargoTracker.Deliver truckState.Cargo truck.Id hoursElapsed), truckState.UnloadAndDepart()
        | _ -> failwithf "impossible" // TODO: make it impossible
    | DrivingTruck truckState -> 
        cargoTracker, truckState.DriveOn()
    | UnladenParkedTruck truckState ->
        if truckState.HoursWaited = 0 then
            logTruckArrivalEvent hoursElapsed truck.Id Factory None sink
        match cargoTracker.TryPickupAtFactory truck.Id with
        | Some c, tracker ->
            let dest = 
                match c.Destination with
                | Warehouse.A -> PortTerminal
                | Warehouse.B -> WarehouseB
            logTruckDepartureEvent hoursElapsed truck.Id Factory dest (Some c) sink
            tracker, truckState.LoadAndDepart c
        | None, tracker -> 
            tracker, truckState.Wait()

open Expecto

let testCargoA = { Id = 1; Destination = Warehouse.A }
let testCargoB = { Id = 100; Destination = Warehouse.B }

let truckTests =

   testList "Truck tests" [
        test "After one hour, a truck at the factory should have picked up the first available container and be at the port with it" {
            let initialTracker = CargoTracker.Empty.AddToFactoryQueue testCargoA
            let tracker, truckState = driveOneHour initialTracker { Id = 1; State = (UnladenParkedTruck { ParkedAt = Factory; HoursWaited = 0})} 1 consoleWriteSink
            Expect.equal truckState (LadenParkedTruck { ParkedAt = PortTerminal; Cargo = testCargoA }) "Truck is not in the expected state"
            Expect.equal tracker.FactoryOutboundQueue [] "Factory is not empty"
            Expect.equal tracker.PortOutboundQueue [] "Port is not empty"
            Expect.equal tracker.InTransit [testCargoA, 1] "Port is not empty"
        }

        test "After one hour, a truck with cargo going to warehouse B with 4 hours remaining transit time should be in the same state but with 3 hours left" {
            let tracker, truckState = driveOneHour CargoTracker.Empty { Id = 1; State = (DrivingTruck { Transit = { Origin = Factory; Destination = WarehouseB; HoursLeftToDestination = 4 }; Cargo = Some testCargoB })} 1 consoleWriteSink
            Expect.equal truckState (DrivingTruck { Transit = { Origin = Factory; Destination = WarehouseB; HoursLeftToDestination = 3 }; Cargo = Some testCargoB }) "Truck is not in the expected state"
            Expect.equal tracker CargoTracker.Empty ""
        }

        test "After one hour, a truck with cargo going to warehouse B with 1 hour remaining transit time should be parked at the warehouse" {
            let tracker, truckState = driveOneHour CargoTracker.Empty { Id = 1; State = (DrivingTruck { Transit = { Origin = Factory; Destination = WarehouseB; HoursLeftToDestination = 1 }; Cargo = Some testCargoB })} 1 consoleWriteSink
            Expect.equal truckState (LadenParkedTruck { ParkedAt = WarehouseB; Cargo = testCargoB }) "Truck is not in the expected state"
            Expect.equal tracker CargoTracker.Empty ""
        }

        test "After one hour, a empty truck going to the factory with 1 hour remaining transit time should be parked at the factory" {
            let tracker, truckState = driveOneHour CargoTracker.Empty { Id = 1; State = (DrivingTruck { Transit = { Origin = WarehouseB; Destination = Factory; HoursLeftToDestination = 1 }; Cargo = None })} 1 consoleWriteSink
            Expect.equal truckState (UnladenParkedTruck { ParkedAt = Factory; HoursWaited = 0}) "Truck is not in the expected state"
            Expect.equal tracker CargoTracker.Empty ""
        }

        test "After one hour, a parked truck at a factory containing cargo for warehouses A and B should be parked at the port with cargo A" {
            let initialTracker = CargoTracker.Empty.AddToFactoryQueue(testCargoA, testCargoB)
            let tracker, truckState = driveOneHour initialTracker { Id = 1; State = (UnladenParkedTruck { ParkedAt = Factory; HoursWaited = 0})} 1 consoleWriteSink
            Expect.equal truckState (LadenParkedTruck { ParkedAt = PortTerminal; Cargo = testCargoA}) "Truck is not in the expected state"
            Expect.equal tracker.FactoryOutboundQueue [testCargoB] ""
            Expect.equal tracker.InTransit [testCargoA, 1] ""
        }

        test "After one hour, a truck parked at the port with cargo should be back at the factory and the cargo should have been unloaded in the port" {
            let initialTracker = CargoTracker.Empty
            let tracker, truckState = driveOneHour initialTracker { Id = 1; State = (LadenParkedTruck { ParkedAt = PortTerminal; Cargo = testCargoA})} 1 consoleWriteSink
            Expect.equal truckState (UnladenParkedTruck { ParkedAt = Factory; HoursWaited = 0}) "Truck is not in the expected state"
            Expect.equal tracker.FactoryOutboundQueue [] "Factory is not empty"
            Expect.equal tracker.PortOutboundQueue [testCargoA] "Port should contain the cargo dropped off by the truck"
        }

        test "After one hour, a parked truck at an empty factory should still be parked" {
            let initialTracker = CargoTracker.Empty
            let tracker, truckState = driveOneHour initialTracker { Id = 1; State = (UnladenParkedTruck { ParkedAt = Factory; HoursWaited = 0})} 1 consoleWriteSink
            Expect.equal truckState (UnladenParkedTruck { ParkedAt = Factory; HoursWaited = 1}) "Truck is not in the expected state"
            Expect.equal tracker initialTracker ""
        }

        test "After one hour, a truck parked at warehouseB should be going back empty to the factory with 4 hours left" {
            let initialTracker = CargoTracker.Empty
            let tracker, truckState = driveOneHour initialTracker { Id = 1; State = (LadenParkedTruck { ParkedAt = WarehouseB; Cargo = testCargoB})} 1 consoleWriteSink
            Expect.equal truckState (DrivingTruck { Transit = { Origin = WarehouseB; Destination = Factory; HoursLeftToDestination = 4 }; Cargo = None }) "Truck is not in the expected state"
            Expect.equal tracker.FactoryOutboundQueue [] "Factory is not empty"
            Expect.equal tracker.PortOutboundQueue [] ""
            Expect.equal tracker.InTransit [] "Port should the container dropped by the truck"
            Expect.equal tracker.Delivered [testCargoB, 1, 1] "Port should the container dropped by the truck"
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

type Ship = IdentifiableVehicle<ShipState>

let sailOneHour (cargoTracker: CargoTracker) (vehicle: Ship) (hoursElapsed: int) (sink: Event -> unit)=
    match vehicle.State with
    | SailingShip ship ->
        cargoTracker, ship.SailOn ()
    | LadenDockedShip ship ->
        logShipArrivalEvent hoursElapsed vehicle.Id WarehouseA (Some ship.Cargo) sink
        logShipDepartureEvent hoursElapsed vehicle.Id WarehouseA PortQuay None sink
        cargoTracker.Deliver ship.Cargo vehicle.Id hoursElapsed, ship.UnloadAndCastOut()
    | UnladenDockedShip ship ->
        if ship.HoursWaited = 0 then
            logShipArrivalEvent hoursElapsed vehicle.Id PortQuay None sink
        match cargoTracker.TryPickupAtPort vehicle.Id with
        | None, tracker ->
            tracker, ship.Wait()
        | Some cargo, tracker ->
            logShipDepartureEvent hoursElapsed vehicle.Id PortQuay WarehouseA (Some cargo) sink
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

type Vehicle =
    | Truck of Truck
    | Ship of Ship

let moveOneHour (cargoTracker: CargoTracker) vehicle elapsed sink =
    match vehicle with
    | Ship ship ->
        let cargoTrackerAfterOneHour, shipStateAfterOneHour = sailOneHour cargoTracker ship elapsed sink
        cargoTrackerAfterOneHour, Ship { Id = ship.Id; State = shipStateAfterOneHour }
    | Truck truck ->
        let cargoTrackerAfterOneHour, truckStateAfterOneHour = driveOneHour cargoTracker truck elapsed sink
        cargoTrackerAfterOneHour, Truck { Id = truck.Id; State = truckStateAfterOneHour }

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