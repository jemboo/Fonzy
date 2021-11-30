namespace global
open System


module Runs =

    let makeWorldFromCauseSpec
                    (monitor:'a->unit)
                    (rootDir:FileDir)
                    (causeSpec:causeSpec) =
        result {
    
            let! cause = causeSpec |> Causes.fromCauseSpec monitor

            let! binSpecWorld = World.createFromParent World.empty
                                                       cause
            let dataStore = binSpecWorld
                            |> WorldDto.toDto
                            |> WorldStorage.WorldDto

            let! outputFolder = binSpecWorld.id |> WorldId.value |> string |> FileFolder.create ""

            let! outputDir = rootDir |> FileDir.appendFolder outputFolder
            let ws = new WorldStorageDirectory(outputDir) :> IWorldStorage

            return! dataStore |> ws.AddNewDataStoreItem         
        }
