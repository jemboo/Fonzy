namespace global
open System


module Runs =

    let runCauseSpec
                    (outputDir:FileDir) 
                    (causeSpec:causeSpec) =
        result {
    
            let directoryDataSource = new WorldStorageDirectory(outputDir) 
                                            :> IWorldStorage
            let! cause = causeSpec |> Causes.fromCauseSpec

            let! binSpecWorld = World.createFromParent World.empty
                                                       cause
            let dataStore = binSpecWorld
                            |> WorldDto.toDto
                            |> WorldStorage.WorldDto

            return! dataStore |> directoryDataSource.AddNewDataStoreItem         
        }
