namespace global
open System


module Runs =

    let makeWorldFromCauseSpec
                    (monitor:obj -> unit)
                    (outputDir:FileDir)
                    (parentWorld:world)
                    (causeSpec:causeSpec) =
        result {
    
            let! cause = causeSpec |> Causes.fromCauseSpec monitor

            let! binSpecWorld = World.createFromParent parentWorld
                                                       cause
            let dataStore = binSpecWorld
                            |> WorldDto.toDto
                            |> WorldStorage.WorldDto


            let ws = new WorldStorageDirectory(outputDir) :> IWorldStorage

            return! dataStore |> ws.AddNewDataStoreItem         
        }
