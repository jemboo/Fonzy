namespace global
open System

type sHC<'T> = 
    {
       trackId:Guid;
       stepNumber:StepNumber; 
       seed:RandomSeed; 
       curVal:'T;
       curFitness:Energy;
       updater: StepNumber -> IRando -> 'T -> 'T * RandomSeed
       evaluator: StepNumber -> 'T -> Energy
       annealer:annealer
    }

module SHC =

    let update (hist:List<sHC<'T>>) = 
        let shc = hist |> List.head
        let randy = Rando.LcgFromSeed shc.seed
        let newVal, newSeed = 
                    shc.updater 
                        shc.stepNumber 
                        randy 
                        shc.curVal
        let newFitness = shc.evaluator shc.stepNumber newVal
        let curStep = shc.stepNumber
        let caster = fun () -> randy.NextFloat
        let anF = shc.annealer.chooser
        let useNewOne = anF shc.curFitness newFitness caster curStep
        if (useNewOne) then
            let newShc =
                {
                    sHC.trackId = shc.trackId;
                    stepNumber = shc.stepNumber |> StepNumber.increment;
                    curVal = newVal;
                    curFitness = newFitness;
                    seed = newSeed;
                    updater = shc.updater;
                    evaluator =  shc.evaluator;
                    annealer = shc.annealer;
                }
            newShc::hist
        else
            let newShc =
                {
                    sHC.trackId = shc.trackId;
                    stepNumber = shc.stepNumber |> StepNumber.increment;
                    curVal = shc.curVal;
                    curFitness = shc.curFitness;
                    seed = newSeed;
                    updater = shc.updater;
                    evaluator =  shc.evaluator;
                    annealer = shc.annealer;
                }
            hist



module sorterSHC =
    
    let switchMutator (mutRate:MutationRate) 
                      (skipPrefix:SwitchCount) =
        fun (step:StepNumber) (seed:int) (sorter:Sorter) ->
            let randy = Rando.fromSeed RngType.Lcg (RandomSeed.fromInt seed)
            let mutant = sorter |> SorterMutate.mutateBySwitch mutRate skipPrefix randy 
            (mutant, randy.NextPositiveInt)


    let stageMutator (mutRate:MutationRate) 
                     (skipPrefix:SwitchCount) =
        fun (step:StepNumber) (seed:int) (sorter:Sorter) ->
            let randy = Rando.fromSeed RngType.Lcg (RandomSeed.fromInt seed)
            let mutant = sorter |> SorterMutate.mutateByStage  mutRate skipPrefix randy 
            (mutant, randy.NextPositiveInt)


    let sorterEval (switchPfx:seq<Switch>)
                   (degree:Degree) =

        let pfxArray = switchPfx |> Seq.toArray
        let ssAllfordegree = SortableSetBp64.allBp64 degree
                             |> sortableSet.Bp64

        let ssOp, pfxUses = SortingOps.SortableSet.switchReduce
                                    ssAllfordegree
                                    pfxArray

        fun (step:StepNumber) (sorter:Sorter) ->
            let suPlan = Sorting.SwitchUsePlan.makeIndexes
                            pfxUses
                            (sorter.switches.Length |> SwitchCount.fromInt)
            let swEvRecs = SortingOps.Sorter.eval sorter
                                   ssOp
                                   suPlan
                                   Sorting.EventGrouping.BySwitch
            let switchUses = swEvRecs
                             |> SortingEval.SwitchEventRecords.getSwitchUses
            let usedSwitches = sorter 
                               |> SwitchUses.getUsedSwitches switchUses
            let sorterPerf = 
                {
                    SortingEval.sorterPerf.successful = swEvRecs 
                        |> SortingEval.SwitchEventRecords.getAllSortsWereComplete
                        |> Some
                    SortingEval.sorterPerf.usedStageCount = 
                            Stage.getStageCount 
                                sorter.degree 
                                usedSwitches
                    SortingEval.sorterPerf.usedSwitchCount = 
                            SwitchCount.fromInt usedSwitches.Length
                }

            SorterFitness.fromSorterPerf sorter.degree (StageWeight.fromFloat 1.0) sorterPerf 



    let makeSorterClimber (startingVal:Sorter)
                          (startingSeed:RandomSeed)
                          (eval: StepNumber -> Sorter -> Energy)
                          (updater: StepNumber -> IRando -> Sorter -> Sorter * RandomSeed) 
                          (annealer:annealer) =
        let seed = startingSeed
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
           sHC.evaluator = eval;
           annealer = annealer;
        }

