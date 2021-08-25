namespace global
open System


module Runs =

    let runCauseSpec
                    (outputDir:FilePath) 
                    (causeSpec:causeSpec) =
        result {
    
            let directoryDataSource = new DirectoryDataSource(outputDir) 
                                            :> IDataSource
            let! cause = causeSpec |> Causes.fromCauseSpec

            let! binSpecWorld = World.createFromParent World.empty
                                                       cause
            let dataStore = binSpecWorld
                            |> WorldDto.toDto
                            |> DataStoreItem.WorldDto

            return! dataStore |> directoryDataSource.AddNewDataStoreItem         
        }
