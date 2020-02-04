#r "paket:
nuget Fake.IO.FileSystem
nuget Fake.DotNet.Paket
nuget Fake.DotNet.Fsi
nuget Fake.Core.Target //"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.IO
open Fake.DotNet
open Fake.IO.Globbing.Operators

// Properties
let outDir = "./output/"

// Targets
Target.create "Clean" (fun _ ->
  Shell.cleanDir outDir
)

Target.create "PaketRestore" (fun _ ->
  Paket.restore (id)
)

Target.create "Exercice2" (fun _ ->
  let script = System.IO.Path.Combine("TransportTycoon.Kata.Exercises","Exercise2.fsx")
  Fsi.exec (fun p -> 
    { p with 
        TargetProfile = Fsi.Profile.Netcore
        ToolPath = Fsi.FsiTool.Internal
        WorkingDirectory = "TransportTycoon.Kata.Exercises"
    }) script [] |> ignore
)

let tracePythonScript = @"paket-files\Softwarepark\exercises\transport-tycoon\trace\trace.py"
let pythonExe = "python.exe"

Target.create "Exercice2Traces" (fun _ ->
  let addX s = sprintf "\"%s\"" s
  !! "TransportTycoon.Kata.Exercises/output/*.log"
  |> (fun items -> 
    items |> Seq.iter (fun x ->
      let result =
        CreateProcess.fromRawCommand pythonExe [tracePythonScript; x]
        |> CreateProcess.ensureExitCode
        |> CreateProcess.redirectOutput
        |> Proc.run

      match result.ExitCode with
      | 0 -> 
        let traceFile = System.IO.Path.ChangeExtension(x, ".trace")
        System.IO.File.WriteAllText(traceFile, result.Result.Output) |> ignore
      | exitCode -> failwithf "Python call failed with exit code %i" exitCode)
  )
)

// Dependencies
open Fake.Core.TargetOperators

"Clean"
  ==> "PaketRestore"
    ==> "Exercice2"
      ==>"Exercice2Traces"

// start build
Target.runOrDefault "Exercice2Traces"