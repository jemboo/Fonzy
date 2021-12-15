namespace global
open System

module Reports =

    let reportSwitchWeights (inputDir:FileDir) = 
        let _getSwitchWeights (id:Guid) = 
            let fpath = FilePath.fromParts 
                                inputDir 
                                (id |> string |> FileName.fromString) 
                                (".txt" |> FileExt.fromString)
                        |> Result.ExtractOrThrow
            let fileDtoStream = FileDtoStream.openSorterShc2Dto "" fpath
                                |> Result.ExtractOrThrow
            let items = FileDtoStream.read fileDtoStream
                                |> Result.ExtractOrThrow
            items |> Seq.map(fun dto-> dto.weights)

        let reportDataSource = new WorldStorageDirectory(inputDir) 
                                    :> IWorldStorage
        let repNames = reportDataSource.GetDataSourceIds()
                      |> Result.ExtractOrThrow
                      |> Seq.toArray

        let raw = repNames |> Array.map( _getSwitchWeights >> Seq.toArray)
        let raw350 = raw |> Array.filter(fun q -> q.Length = 350)

        let merged = CollectionUtils.merge3DintsToFloats raw350
                      |> Seq.map(StringUtils.printSeqfToRow string)

        let reportFolder = FileFolder.fromString "reports"
        let reportDir = inputDir |> FileDir.appendFolder reportFolder
                                 |> Result.ExtractOrThrow

        let outPath = FilePath.fromParts 
                                reportDir 
                                (Guid.NewGuid() |> string |> FileName.fromString) 
                                (".txt" |> FileExt.fromString)
                      |> Result.ExtractOrThrow

        FileUtils.makeFileFromLines outPath merged




    let mergePerfBins (inputDir:FileDir) = 

        let _getShcDtos (id:Guid) = 
            let fpath = FilePath.fromParts 
                                inputDir 
                                (id |> string |> FileName.fromString) 
                                (".txt" |> FileExt.fromString)
                        |> Result.ExtractOrThrow
            let fileDtoStream = FileDtoStream.openSorterShc2Dto "" fpath
                                |> Result.ExtractOrThrow
            let items = FileDtoStream.read fileDtoStream
                                |> Result.ExtractOrThrow
            items |> List.toSeq


        let reportFolder = FileFolder.fromString "reports"
        let reportDir = inputDir |> FileDir.appendFolder reportFolder
                                 |> Result.ExtractOrThrow


        let outPath = FilePath.fromParts 
                                reportDir 
                                (Guid.NewGuid() |> string |> FileName.fromString) 
                                (".txt" |> FileExt.fromString)
                      |> Result.ExtractOrThrow


        let reportDataSource = new WorldStorageDirectory(inputDir) 
                                    :> IWorldStorage
        let repNames = reportDataSource.GetDataSourceIds()
                      |> Result.ExtractOrThrow


        let wab = repNames |> Seq.map(_getShcDtos) |> Seq.concat |> Seq.map(string)

        let merged = repNames |> Seq.map(_getShcDtos) |> Seq.concat |> Seq.groupBy(fun dto ->  (dto.sorterId, dto.generation))// (dto.mut, dto.temp, dto.sorterId, dto.generation))
        let mfs = merged |> Seq.map(fun tup -> (snd tup) |> SorterShcMergedDto.merge)
                         |> Seq.toList
                         |> Result.sequence
                         |> Result.ExtractOrThrow
        let fms = mfs |> List.filter(fun m -> m.mergeCt > 1)
        let ct = merged |> Seq.length
        FileUtils.makeDirectory reportDir |> Result.ExtractOrThrow |> ignore
        FileUtils.makeFileFromLines outPath wab



    
    
    let sortPerfBins (inputDir:FileDir) = 

        let sortedFolder = FileFolder.fromString "sorted"
        let sortedDir = inputDir |> FileDir.appendFolder sortedFolder
                                 |> Result.ExtractOrThrow


        let _getShcDtos (id:Guid) = 
            let fpath = FilePath.fromParts 
                                inputDir 
                                (id |> string |> FileName.fromString) 
                                (".txt" |> FileExt.fromString)
                        |> Result.ExtractOrThrow
            let fileDtoStream = FileDtoStream.openSorterShc2Dto "" fpath
                                |> Result.ExtractOrThrow
            let items = FileDtoStream.read fileDtoStream
                                |> Result.ExtractOrThrow
            items |> List.toSeq

        
        let mvShcDto (dtos: sorterShc2Dto seq) =
            let seqA = dtos |> Seq.toArray
            let fpath = FilePath.fromParts 
                                sortedDir 
                                (seqA.[0].generation |> string |> FileName.fromString) 
                                (".txt" |> FileExt.fromString)
                        |> Result.ExtractOrThrow
            let fileDtoStream = FileDtoStream.openSorterShc2Dto "" fpath
                                |> Result.ExtractOrThrow


            let res = fileDtoStream |> FileDtoStream.append (seqA)
                                    |> Result.ExtractOrThrow
            ()

        let reportDataSource = new WorldStorageDirectory(inputDir) 
                                    :> IWorldStorage
        let repNames = reportDataSource.GetDataSourceIds()
                      |> Result.ExtractOrThrow

        let wab = repNames |> Seq.map(_getShcDtos) 
                           |> Seq.concat 
                           |> Seq.groupBy(fun shc -> shc.generation)
                           |> Seq.map(snd >> mvShcDto)
                           |> Seq.toArray

        ()