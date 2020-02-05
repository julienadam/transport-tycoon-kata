#load @"Exercise2.fsx"

open Exercise2
open Exercise2.EventLogging
open Exercise2.CargoTracking

// Unit tests

open Expecto

let testCargoA = { Id = 1; Destination = Warehouse.A }
let testCargoA' = { Id = 2; Destination = Warehouse.A }
let testCargoB = { Id = 100; Destination = Warehouse.B }

module TruckTests = 

    open TruckTransport

    let runTruckTest (initialState: TruckState) initialTracker expectedState trackerChecks =
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


module ShipTests = 
    open ShipTransport

    let runShipTest initialState initialTracker expectedState trackerChecks =
        let tracker, ship = { Id = 1; State = initialState }.SailOneHour initialTracker 1 consoleWriteSink
        Expect.equal ship.State expectedState "Ship is not in the expected state"
        trackerChecks tracker

    let runShipTestWithEmptyTracker initialState expectedState =
        runShipTest initialState CargoTracker.Empty expectedState ignore

    let shipTests =
        testList "Ship sailing tests" [

            test "After one hour, a ship sailing to warehouse A with cargo, with 4 hours remaining should still be sailing, with 3 hours remaining" {
               let initialState = DeliveringShip { Hold = ShipHold([testCargoA]); Origin = PortQuay; Destination = WarehouseA; HoursLeftToDestination = 4 }
               let expectedState = DeliveringShip { Hold = ShipHold([testCargoA]); Origin = PortQuay; Destination = WarehouseA; HoursLeftToDestination = 3 }
               runShipTestWithEmptyTracker initialState expectedState
            }

            test "After one hour, a ship returning to port, with 4 hours remaining should still be sailing, with 3 hours remaining" {
               let initialState = ReturningShip { Origin =  WarehouseA; Destination = PortQuay; HoursLeftToDestination = 4 }
               let expectedState = ReturningShip { Origin =  WarehouseA; Destination = PortQuay; HoursLeftToDestination = 3 }
               runShipTestWithEmptyTracker initialState expectedState
            }

            test "After one hour, a ship sailing to the warehouse with cargo, with 1 hours remaining should be docked at the destination" {
               let initialState = DeliveringShip { Hold = ShipHold([testCargoA]); Origin = PortQuay; Destination = WarehouseA; HoursLeftToDestination = 1 }
               let expectedState = LadenDockedShip { Hold = ShipHold([testCargoA]); DockedAt = WarehouseA }
               runShipTestWithEmptyTracker initialState expectedState
            }

            test "After one hour, a ship returning to port, with 1 hours remaining should be docked at the port" {
               let initialState = ReturningShip { Origin = WarehouseA; Destination = PortQuay; HoursLeftToDestination = 1 }
               let expectedState = UnladenDockedShip { DockedAt = PortQuay; HoursWaited = 0 }
               runShipTestWithEmptyTracker initialState expectedState
            }

            test "After one hour, a ship docked at the port with cargo available should be loading the cargo" {
               let initialTracker = (CargoTracker.Empty.DropOffAtPort testCargoA).DropOffAtPort testCargoA'
               let initialState = UnladenDockedShip { DockedAt = PortQuay; HoursWaited = 0 }
               let expectedState = LoadingShip { DockedAt = PortQuay; Hold = ShipHold([testCargoA; testCargoA']) }
               runShipTest initialState initialTracker expectedState (fun tracker ->
                    Expect.equal tracker.PortOutboundQueue [] "Port should only have A' left"
                    Expect.equal tracker.InTransit [(testCargoA, 1); (testCargoA', 1)] "Port should only have A' left")
            }

            test "After one hour, a ship loading at the port should be sailing to the warehouse with the cargo and 5 hours left" {
               let initialState = LoadingShip { DockedAt = PortQuay; Hold = ShipHold([testCargoA; testCargoA']) }
               let expectedState = DeliveringShip { Origin = PortQuay; Destination = WarehouseA; Hold = ShipHold([testCargoA; testCargoA']); HoursLeftToDestination = 5 }
               runShipTestWithEmptyTracker initialState expectedState
            }

            test "After one hour, a ship docked at the port with no cargo available should be waiting at the port" {
               let initialState = UnladenDockedShip { DockedAt = PortQuay; HoursWaited = 0 }
               let expectedState = UnladenDockedShip { DockedAt = PortQuay; HoursWaited = 1 }
               runShipTestWithEmptyTracker initialState expectedState
            }

            test "After one hour, a ship docked at the warehouse should be unloading" {
               let initialState = LadenDockedShip { DockedAt = WarehouseA; Hold = ShipHold([testCargoA]) }
               let expectedState = UnloadingShip { DockedAt = WarehouseA; Hold = ShipHold([testCargoA]) }
               runShipTestWithEmptyTracker initialState expectedState
            }

            test "After one hour, an unloading ship docked at the warehouse should be returning with 5 hours left" {
               let initialState = UnloadingShip { DockedAt = WarehouseA; Hold = ShipHold([testCargoA]) }
               let expectedState = ReturningShip { Origin = WarehouseA; Destination = PortQuay; HoursLeftToDestination = 5 }
               runShipTestWithEmptyTracker initialState expectedState
            }
    ]

module DeliveryTests = 

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

runTests defaultConfig DeliveryTests.deliveryTests

runTests defaultConfig ShipTests.shipTests

runTests defaultConfig TruckTests.truckTests
