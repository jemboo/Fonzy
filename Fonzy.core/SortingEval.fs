﻿namespace global
open System
open FSharp.Stats

module SortingEval =

    type noGrouping  = 
        {
            switchEventRollout:switchEventRollout; 
            sortableRollout:sortableSetRollout;
        }

    type groupBySwitch = 
        {
            switchUses:switchUses; 
            sortableRollout:sortableSetRollout;
        }

    type switchEventRecords =
        | NoGrouping of noGrouping
        | BySwitch of groupBySwitch

    type sorterPerf = 
        { 
            usedSwitchCount:SwitchCount; 
            usedStageCount:StageCount;
            successful:bool Option
        }

    module SorterPerf =
        let dflt = 
            {
                usedSwitchCount = SwitchCount.fromInt 0;
                usedStageCount = StageCount.fromInt 0;
                successful = None
            }
        let report (perf:sorterPerf) =
            let fbo (v:bool option) =
                match v with
                | Some tv -> sprintf "%b" tv
                | None -> "none"

            sprintf "%s\t%d\t%d" 
                        (fbo perf.successful)
                        (StageCount.value perf.usedStageCount)
                        (SwitchCount.value perf.usedSwitchCount)

        let isSucessful (perf:sorterPerf) =
            match perf.successful with
            | Some tv -> tv
            | None -> false


    type sorterPerfBin = 
        { 
            usedSwitchCount:SwitchCount; 
            usedStageCount:StageCount;
            sorterCount:SorterCount;
            successCount:int;
            failCount:int;
        }


    module SwitchEventRecords =

        let getSortableSetRollout (switchEventRecords:switchEventRecords) =
            match switchEventRecords with
            | NoGrouping seNg -> seNg.sortableRollout
            | BySwitch seGs -> seGs.sortableRollout


        let getSwitchUses (switchEventRecords:switchEventRecords) =
            match switchEventRecords with
            | NoGrouping seNg -> seNg.switchEventRollout 
                                    |> SwitchEventRollout.toSwitchUses
            | BySwitch seGs -> seGs.switchUses


        let getAllSortsWereComplete (switchEventRecords:switchEventRecords) =
            match switchEventRecords with
            | NoGrouping seNg -> seNg.sortableRollout 
                                    |> SortableSetRollout.isSorted
            | BySwitch seGs ->  seGs.sortableRollout
                                    |> SortableSetRollout.isSorted


        let getUsedSwitchCount (switchEventRecords:switchEventRecords) =
            result {
                let switchUses = getSwitchUses switchEventRecords
                return switchUses |> SwitchUses.usedSwitchCount
            }


    type sortingResult =
        {
            sorterId:SorterId;
            sorter:sorter; 
            switchEventRecords:switchEventRecords;
        }

    type sorterCoverage = 
        { 
            sorterId:SorterId;
            perf:sorterPerf;
            usedSwitches:Switch[];
        }
        
                     
    module SorterCoverage = 

        let fromSwitchEventRecords (checkSuccess:bool)
                                   (r:sortingResult) =
            result {
                    let switchUses = 
                            r.switchEventRecords |> SwitchEventRecords.getSwitchUses
                    let usedSwitchArray = 
                            r.sorter |> SwitchUses.getUsedSwitches switchUses
                    let! usedSwitchCount = SwitchCount.create "" usedSwitchArray.Length
                    let usedStageCount = Stage.getStageCount r.sorter.degree usedSwitchArray
                    let success = 
                        match checkSuccess with
                        | true -> r.switchEventRecords 
                                  |> SwitchEventRecords.getAllSortsWereComplete
                                  |> Some
                        | false -> None

                    let perfBin = {
                                    sorterPerf.usedStageCount = usedStageCount;
                                    successful = success;
                                    usedSwitchCount=usedSwitchCount 
                                   }
                    return {
                                sorterCoverage.perf = perfBin; 
                                sorterId = r.sorterId;
                                usedSwitches = usedSwitchArray;
                           }
               }


    module SorterPerfBin =
        
        let merge (bins:sorterPerfBin seq) =
            let _makeKey (bin:sorterPerfBin) =
                (bin.usedSwitchCount, bin.usedStageCount)
            let _add (taggedBins:(SwitchCount*StageCount)*sorterPerfBin array) =
                let (wc, tc), bins = taggedBins
                let totSorterCt = 
                        bins 
                          |> Seq.map(fun bin -> bin.sorterCount)
                          |> Seq.fold SorterCount.add (SorterCount.fromInt 0)
                let totSuccessCt = 
                        bins 
                          |> Seq.map(fun bin -> bin.successCount)
                          |> Seq.fold (+) 0
                let totFailCt = 
                        bins 
                          |> Seq.map(fun bin -> bin.failCount)
                          |> Seq.fold (+) 0
                {
                    sorterPerfBin.usedSwitchCount = wc;
                    sorterPerfBin.usedStageCount = tc;
                    sorterPerfBin.sorterCount = totSorterCt
                    sorterPerfBin.successCount = totSuccessCt;
                    sorterPerfBin.failCount = totFailCt;
                }

            bins 
               |> Seq.toArray
               |> Array.groupBy(_makeKey)
               |> Seq.map(_add)


    
        let fromSorterPerfs (perfs:sorterPerf seq) =
            let extractSorterPerfBin ((stc, swc), (perfs:sorterPerf[])) =
                let sct = perfs |> Array.filter(fun sc -> 
                                        sc.successful = (Some true))
                                |> Array.length
                let fct = perfs |> Array.filter(fun sc -> 
                                        sc.successful = (Some false))
                                |> Array.length
                {
                    sorterPerfBin.sorterCount = SorterCount.fromInt perfs.Length
                    usedStageCount = stc;
                    usedSwitchCount = swc;
                    successCount = sct;
                    failCount = fct;
                }
            perfs
                |> Seq.toArray
                |> Array.groupBy(fun c -> (c.usedStageCount, 
                                           c.usedSwitchCount))
                |> Array.map(extractSorterPerfBin)


        let fromSorterCoverages (coverages:sorterCoverage seq) =
             coverages |> Seq.map(fun cov -> cov.perf)
                       |> fromSorterPerfs


        let toSorterPerfs (bins:sorterPerfBin seq) =
            let _sp (spBin:sorterPerfBin) =
                let ssfls =
                    {
                        sorterPerf.successful = Some true;
                        sorterPerf.usedStageCount = spBin.usedStageCount;
                        sorterPerf.usedSwitchCount = spBin.usedSwitchCount;
                    } |> Seq.replicate spBin.successCount
                let unSsfls =
                    {
                        sorterPerf.successful = Some false;
                        sorterPerf.usedStageCount = spBin.usedStageCount;
                        sorterPerf.usedSwitchCount = spBin.usedSwitchCount;
                    } |> Seq.replicate spBin.failCount
                ssfls |> Seq.append unSsfls

            bins |> Seq.map(_sp) |> Seq.concat



        let getMinMaxMeanOfSuccessful (perfM:sorterPerfBin -> double)  
                                      (bins:sorterPerfBin seq)  =
            use enumer = bins.GetEnumerator()
            let mutable min = Double.MaxValue
            let mutable max = Double.MinValue
            let mutable total = 0.0
            let mutable count = 0.0
            while enumer.MoveNext() do
                if enumer.Current.successCount > 0 then
                    let fct = (float enumer.Current.successCount)
                    let curM = perfM enumer.Current
                    if curM < min then 
                        min <- curM
                    if curM > max then
                        max <- curM
                    count <- count + fct
                    total <- total + (curM * fct)
            let mean = if count = 0.0 then 0.0 else total / count
            (min, max, mean)


        let getMinMaxMeanOfFails (perfM:sorterPerfBin -> double) 
                                 (bins:sorterPerfBin seq)  =
            use enumer = bins.GetEnumerator()
            let mutable min = Double.MaxValue
            let mutable max = Double.MinValue
            let mutable total = 0.0
            let mutable count = 0.0
            while enumer.MoveNext() do
                if enumer.Current.failCount > 0 then
                    let fct = (float enumer.Current.failCount)
                    let curM = perfM enumer.Current
                    if curM < min then 
                        min <- curM
                    if curM > max then
                        max <- curM
                    count <- count + fct
                    total <- total + (curM * fct)
            let mean = if count = 0.0 then 0.0 else total / count
            (min, max, mean)


        let getStdevOfSuccessful (perfM:sorterPerfBin -> double) 
                                 (centroid:float) 
                                 (bins:sorterPerfBin seq) =
            use enumer = bins.GetEnumerator()
            let mutable totalCt = 0.0
            let mutable totalRds = 0.0
            while enumer.MoveNext() do
                if enumer.Current.successCount > 0 then
                    let binCt = (float enumer.Current.successCount)
                    let curM = perfM enumer.Current
                    totalCt <- totalCt + binCt
                    totalRds <- totalRds + (Math.Sqrt ((curM - centroid) * (curM - centroid))) * binCt

            if (totalCt = 0.0) then 0.0 else (totalRds / totalCt)



        let getRdsBetterWorseOfSuccessful (perfM:sorterPerfBin -> double) 
                                          (centroid:float)
                                          (bins:sorterPerfBin seq) =
            use enumer = bins.GetEnumerator()
            let mutable totalCtBetter = 0.0
            let mutable totalRdsBetter = 0.0
            let mutable totalCtWorse = 0.0
            let mutable totalRdsWorse = 0.0
            while enumer.MoveNext() do
                if enumer.Current.successCount > 0 then
                    let fct = (float enumer.Current.successCount)
                    let curM = perfM enumer.Current
                    if curM < centroid then 
                        totalCtBetter <- totalCtBetter + fct
                        totalRdsBetter <- totalRdsBetter + (curM - centroid) * (curM - centroid) * fct
                    else
                        totalCtWorse <- totalCtWorse + fct
                        totalRdsWorse <- totalRdsWorse + (curM - centroid) * (curM - centroid) * fct
            let bR = if (totalCtBetter = 0.0) then 0.0 else totalRdsBetter / totalCtBetter
            let wR = if (totalCtWorse = 0.0) then 0.0 else totalRdsWorse / totalCtWorse
            (bR, wR)


        let getRdsBetterWorseOfFails (perfM:sorterPerfBin -> double)  
                                     (centroid:float)
                                     (bins:sorterPerfBin seq)  =
            use enumer = bins.GetEnumerator()
            let mutable totalCtBetter = 0.0
            let mutable totalRdsBetter = 0.0
            let mutable totalCtWorse = 0.0
            let mutable totalRdsWorse = 0.0
            while enumer.MoveNext() do
                if enumer.Current.failCount > 0 then
                    let fct = (float enumer.Current.failCount)
                    let curM = perfM enumer.Current
                    if curM < centroid then 
                        totalCtBetter <- totalCtBetter + fct
                        totalRdsBetter <- totalRdsBetter + (curM - centroid) * (curM - centroid) * fct
                    else
                        totalCtWorse <- totalCtWorse + fct
                        totalRdsWorse <- totalRdsWorse + (curM - centroid) * (curM - centroid) * fct
            let bR = if (totalCtBetter = 0.0) then 0.0 else totalRdsBetter / totalCtBetter
            let wR = if (totalCtWorse = 0.0) then 0.0 else totalRdsWorse / totalCtWorse
            (bR, wR)



        let getStDev (bins:sorterPerfBin seq) (perfM:sorterPerf -> double) =
            bins |> toSorterPerfs |> Seq.map(perfM) |> Seq.stDev

        
        let getMean (bins:sorterPerfBin seq) (perfM:sorterPerf -> double) =
            bins |> toSorterPerfs |> Seq.map(perfM) |> Seq.mean



            
type StageWeight = private StageWeight of float

module StageWeight =
    let value (StageWeight v) = v
    let create id = Ok (StageWeight id)
    let fromFloat (id:float) = create id |> Result.ExtractOrThrow


type sorterSaving = 
        | NotAny
        | All
        | Successful
        | Perf of StageWeight*SorterCount


module SorterFitness =

    let switchBased (degree:Degree) 
                    (switchCount:SwitchCount) = 
        let bestSwitch = SwitchCount.degreeToRecordSwitchCount degree 
                            |> SwitchCount.value |> float
        let scv = switchCount |> SwitchCount.value |> float
        (scv) / (bestSwitch) |> Energy.fromFloat


    let stageBased (degree:Degree) 
                   (stageCount:StageCount) = 
        let bestStage = StageCount.degreeToRecordStageCount degree 
                            |> StageCount.value |> float
        let scv = stageCount |> StageCount.value |> float
        (scv) / (bestStage) |> Energy.fromFloat

    let weighted (degree:Degree) 
                 (stageWeight:StageWeight) 
                 (wCt:SwitchCount)
                 (tCt:StageCount) =
        let wV = switchBased degree wCt
                    |> Energy.value
        let tV = stageBased degree tCt
                    |> Energy.value
        let tw = StageWeight.value stageWeight
        ((wV + tV * tw) / (tw + 1.0)) |> Energy.fromFloat


    let fromSorterPerf (degree:Degree)  
                       (stageWeight:StageWeight) 
                       (perf:SortingEval.sorterPerf) =
        let pv =
            weighted degree stageWeight 
                     perf.usedSwitchCount perf.usedStageCount

        match perf.successful with
        | Some v -> if v then pv else Energy.failure
        | None -> pv



module SorterSaving = 

    let chooseSorterCoverages (degree:Degree)
                              (ssaving:sorterSaving) 
                              (scs:SortingEval.sorterCoverage[]) =
        let getBest (degree:Degree) 
                    (sw:StageWeight) 
                    (sc:SorterCount) 
                    (covs:SortingEval.sorterCoverage[]) =
            let yab = covs |> Array.map(fun c -> 
                              (c, SorterFitness.fromSorterPerf degree sw c.perf))
                           |> Array.sortBy(fun tup -> snd tup  |> Energy.value)
                           |> Array.take(SorterCount.value sc)
                           |> Array.map(fst)
            yab

        match ssaving with
        | NotAny -> [||]
        | All -> scs
        | Successful -> scs |> Array.filter(fun s -> 
                               s.perf |> SortingEval.SorterPerf.isSucessful)
        | Perf (sw, sc) -> getBest degree sw sc scs
