namespace global
open System


type StepNumber = private StepNumber of int
module StepNumber =
    let value (StepNumber v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName StepNumber 0 100000000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let increment gen = fromInt ((value gen) + 1)
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>int)
        }


type sHC<'T> = 
    {
       trackId:Guid;
       stepNumber:StepNumber; 
       seed:int; 
       curVal:'T;
       curFitness:Fitness;
       updater: StepNumber -> int -> 'T -> 'T * int
       evaluator: StepNumber -> 'T -> Fitness
    }

module SHC =

    let update (hist:List<sHC<'T>>) = 
        let shc = hist |> List.head
        let newVal, newSeed = 
                    shc.updater 
                        shc.stepNumber 
                        shc.seed 
                        shc.curVal
        let curFitness = shc.curFitness
                         |> Fitness.value
        let newFitness = shc.evaluator shc.stepNumber newVal
                         |> Fitness.value

        if (curFitness > newFitness) then
            let newShc =
                {
                    sHC.trackId = shc.trackId;
                    stepNumber = shc.stepNumber |> StepNumber.increment;
                    curVal = shc.curVal;
                    curFitness = shc.curFitness;
                    seed = newSeed;
                    updater = shc.updater;
                    evaluator =  shc.evaluator;
                }
            hist
        else
            let newShc =
                {
                    sHC.trackId = shc.trackId;
                    stepNumber = shc.stepNumber |> StepNumber.increment;
                    curVal = newVal;
                    curFitness = Fitness.fromFloat newFitness;
                    seed = newSeed;
                    updater = shc.updater;
                    evaluator =  shc.evaluator;
                }
            newShc::hist




module sorterSHC =
    
    let switchMutator (mutRate:MutationRate) 
                      (skipPrefix:SwitchCount) =
        fun (step:StepNumber) (seed:int) (sorter:Sorter) ->
            let randy = Rando.fromSeed RngType.Lcg (RandomSeed.fromInt seed)
            let mutant = sorter |> SorterGen.mutateBySwitch mutRate skipPrefix randy 
            (mutant, randy.NextPositiveInt)


    let stageMutator (mutRate:MutationRate) 
                     (skipPrefix:SwitchCount) =
        fun (step:StepNumber) (seed:int) (sorter:Sorter) ->
            let randy = Rando.fromSeed RngType.Lcg (RandomSeed.fromInt seed)
            let mutant = sorter |> SorterGen.mutateByStage  mutRate skipPrefix randy 
            (mutant, randy.NextPositiveInt)


    let sorterEval (switchPfx:seq<Switch>)
                   (degree:Degree) = 
        let pfxArray = switchPfx |> Seq.toArray
        let ssAll = SortableSetBp64.allBp64 degree
                    |> sortableSet.Bp64
        let ssOp = SortingOps.SortableSet.switchReduce
                        ssAll
                        pfxArray

        fun (step:StepNumber) (sorter:Sorter) ->
            let suPlan = Sorting.SwitchUsePlan.makeIndexes
                            (pfxArray.Length |> SwitchCount.fromInt)
                            (sorter.switches.Length |> SwitchCount.fromInt)
            let swEvRecs = SortingOps.Sorter.eval sorter
                                   ssOp
                                   suPlan
                                   Sorting.EventGrouping.BySwitch
            let switchUses = swEvRecs |> SortingEval.SwitchEventRecords.getSwitchUses
            let usedSwitchArray = 
                    sorter |> SwitchUses.getUsedSwitches switchUses
            let sorterPerf = 
                {
                    SortingEval.sorterPerf.successful = swEvRecs 
                        |> SortingEval.SwitchEventRecords.getAllSortsWereComplete
                        |> Some
                    SortingEval.sorterPerf.usedStageCount = 
                            Stage.getStageCount 
                                sorter.degree 
                                usedSwitchArray
                    SortingEval.sorterPerf.usedSwitchCount = 
                            SwitchCount.fromInt usedSwitchArray.Length
                }

            Fitness.fromSorterPerf sorterPerf sorter.degree

    let makeSorterClimber (startingVal:Sorter)
                          (eval: StepNumber -> Sorter -> Fitness)
                          (updater: StepNumber -> int -> Sorter -> Sorter * int) =
        let seed = 5
        let gu  = Guid.NewGuid()
        let step = StepNumber.fromInt 0
        let fitness = eval step startingVal
        {
           sHC.trackId = gu;
           sHC.stepNumber = step; 
           sHC.seed = seed; 
           sHC.curVal = startingVal;
           sHC.curFitness= fitness;
           sHC.updater = updater;
           sHC.evaluator = eval
        }

