namespace global
open System
open System.IO

module ShcRep =

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



    let mergePerfBinsForSorterShc2Dto (inputDir:FileDir)
                                      (mergePath:FilePath) = 

        let _getShcDtos (fpath:FilePath) =
            let fileDtoStream = FileDtoStream.openSorterShc2Dto "" fpath
                                |> Result.ExtractOrThrow
            let items = FileDtoStream.read fileDtoStream
                                |> Result.ExtractOrThrow
            items |> List.toSeq


        let _mergeShcDtos (shcDtos:sorterShc2Dto seq) =
            let gps = shcDtos |> Seq.groupBy(fun dto ->  (dto.generation, dto.shcId))
            let merged = gps |> Seq.map(fun tup -> (snd tup) |> SorterShcMergedDto.merge)
                             |> Seq.toList
                             |> Result.sequence
                             |> Result.ExtractOrThrow
            merged


        let genFiles = FileUtils.getFilePathsInDirectory inputDir "*.txt"
                       |> Result.ExtractOrThrow

        
        let mergedDtos = genFiles |> Seq.map(_getShcDtos >> _mergeShcDtos)

        let fileDtoStream = FileDtoStream.openSorterShcMergedDto "" mergePath
                            |> Result.ExtractOrThrow

        let res = mergedDtos |> Seq.map(fun dtos -> FileDtoStream.append dtos fileDtoStream)
                             |> Seq.toList
                             |> Result.sequence
                             |> Result.ExtractOrThrow

        "finished"




    let sorterShcMergedDtoToPivotTable 
                         (degree:Degree)
                         (reportPath:FilePath) 
                         (pivotPath:FilePath) = 

        let stageWeight = StageWeight.fromFloat 1.0

        let energyF = fun (pb:SortingEval.sorterPerfBin) ->  
            SorterFitness.weighted degree stageWeight pb.usedSwitchCount pb.usedStageCount
            |> Energy.value

        let sorterShcRep (dto:sorterShcMergedDto) =
            SorterShcMergedDto.toReport energyF dto

        let reportDtoStream = FileDtoStream.openSorterShcMergedDto "" reportPath
                            |> Result.ExtractOrThrow

        let repSq = FileDtoStream.read2 sorterShcRep reportDtoStream
                    |> Result.ExtractOrThrow

        use sw = new StreamWriter(pivotPath |> FilePath.value, false)
        fprintfn sw "%s" SorterShcMergedDto.pivotTableHdrs
        repSq |> Seq.iter(fprintfn sw "%s")
        sw.Dispose()

        "finished"


    
    let mergeShc2sByGeneration (inputDir:FileDir) = 

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