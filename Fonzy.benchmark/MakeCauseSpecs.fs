namespace global
open System

module MakeCauseSpecs =

    let sorterPerfBins (outputDir:FileDir) 
                       (seed:RandomSeed) =
        let runId = Guid.NewGuid()
        let runFolder = runId |> string |> FileFolder.fromString
        let runDir = outputDir |> FileDir.appendFolder runFolder


        Console.WriteLine(FileDir.value outputDir)


