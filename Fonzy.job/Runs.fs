namespace global
open System


module Runs =

    let makeWorldFromCauseSpec
                    (monitor:obj -> unit)
                    (outputDir:FileDir)
                    (parentWorld:world)
                    (causeSpec:causeSpec) =
        result {
    
            let! cause = causeSpec |> Causes.fromCauseSpec

            let! newWorld = World.createFromParent 
                                        monitor 
                                        parentWorld
                                        cause
            let dataStore = newWorld
                            |> WorldDto.toDto
                            |> WorldStorage.WorldDto

            let ws = new WorldStorageDirectory(outputDir) :> IWorldStorage

            let storageRes = dataStore |> ws.AddNewDataStoreItem
            return newWorld
        }
