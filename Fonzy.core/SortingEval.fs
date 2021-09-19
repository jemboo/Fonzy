namespace global
open System

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
    
        let fromSorterCoverages (coverage:sorterCoverage seq) =
            let extractSorterPerfBin ((stc, swc), (scs:sorterCoverage[])) =
                let sct = scs |> Array.filter(fun sc -> 
                                        sc.perf.successful = (Some true))
                              |> Array.length
                let fct = scs |> Array.filter(fun sc -> 
                                        sc.perf.successful = (Some false))
                              |> Array.length
                {
                    sorterPerfBin.sorterCount = SorterCount.fromInt scs.Length
                    usedStageCount = stc;
                    usedSwitchCount = swc;
                    successCount = sct;
                    failCount = fct;
                }
            coverage
                |> Seq.toArray
                |> Array.groupBy(fun c-> (c.perf.usedStageCount, 
                                          c.perf.usedSwitchCount))
                |> Array.map(extractSorterPerfBin)


            
type Fitness = private Fitness of float
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


module Fitness =
    let value (Fitness v) = v
    let create fieldName v = 
        ConstrainedType.createFloat fieldName Fitness 0.0 10.0 v
    let fromFloat v = create "" v |> Result.ExtractOrThrow
    let repStr v = match v with
                            |Some r -> sprintf "%.4f" (value r)
                            |None -> ""
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>float)
        }

    let failure = 
        Double.MaxValue |> fromFloat


// Positive valued - zero is the best value
type sorterFitness =
        | PerfBin of StageWeight

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


    let fromSorterPerf (degree:Degree)  
                       (stageWeight:StageWeight) 
                       (perf:SortingEval.sorterPerf) =
        let pv =
            let wV = switchBased degree perf.usedSwitchCount
                        |> Energy.value
            let tV = stageBased degree perf.usedStageCount
                        |> Energy.value
            let tw = StageWeight.value stageWeight
            ((wV + tV * tw) / (tw + 1.0)) |> Energy.fromFloat

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
