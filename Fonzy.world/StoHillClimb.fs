namespace global
open System


type sHCmutatorSpec =
    | Constant of sorterMutationType

type sHCevaluatorSpec =
    | PerfBin of StageWeight

type sHCupdaterSpec =
    | SaveEnergy
    | SavePerf
    | SaveSorter
    | SaveAll


type sorterShcSpec = 
    {
       rngGen:RngGen; 
       startingVal:Sorter;
       mutator:sHCmutatorSpec;
       updater: sHCupdaterSpec;
       evaluator: sHCevaluatorSpec;
       annealer:annealerSpec
    }


type sorterShc = 
    {
        step:StepNumber;
        rngGen:RngGen option; 
        sorter:Sorter option;
        switchUses:SwitchUses option
        perf:SortingEval.sorterPerf option
        energy:Energy option;
    }

module SorterShc =
    let getRngGen (shc:sorterShc) = 
        match shc.rngGen with
        | Some r -> r |> Ok
        | None -> Error "rngGen missing"

    let getSorter (shc:sorterShc) = 
        match shc.sorter with
        | Some r -> r |> Ok
        | None -> Error "Sorter missing"

    let getSwitchUses (shc:sorterShc) = 
        match shc.switchUses with
        | Some r -> r |> Ok
        | None -> Error "SwitchUses missing"

    let getPerf (shc:sorterShc) = 
        match shc.perf with
        | Some r -> r |> Ok
        | None -> Error "Perf missing"

    let getEnergy (shc:sorterShc) = 
        match shc.energy with
        | Some r -> r |> Ok
        | None -> Error "Energy missing"

module SorterShcSpec = 
    let ya = None

type sHC<'A> = 
    {
       id:Guid;
       archive: 'A list;
       mutator: 'A -> Result<'A, string>
       updater: 'A list -> 'A -> Result<'A list, string>
       evaluator: 'A -> Result<'A, string>
       annealer: 'A -> 'A -> Result<'A, string>
    }


module SHC =

    let update (shc:sHC<'A>) =
        result {
            let aCurr = shc.archive |> List.head
            let! aMut = aCurr |> shc.mutator
            let! aEval = aMut |> shc.evaluator
            let! aNext = shc.annealer aCurr aEval
            let! aLst = aNext |> shc.updater shc.archive
            return
                {
                    sHC.id = shc.id;
                    sHC.archive = aLst;
                    sHC.mutator = shc.mutator;
                    sHC.updater = shc.updater;
                    sHC.evaluator = shc.evaluator;
                    sHC.annealer = shc.annealer;
                }
        }


    //let update (shc:sHC<'T,'A>) = 
    //    let newVal, randy = shc.mutator shc
    //    let newEnergy = shc.evaluator shc newVal
    //    let caster = fun () -> randy.NextFloat
    //    let useNewVal = shc.annealer.chooser 
    //                        shc.curEnergy 
    //                        newEnergy 
    //                        caster 
    //                        shc.stepNumber
    //    let nextVal = if useNewVal then newVal else shc.curVal
    //    shc.updater shc nextVal

module SorterSHC =
    
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



    //let makeSorterClimber (startingVal:Sorter)
    //                      (startingSeed:RandomSeed)
    //                      (eval: StepNumber -> Sorter -> Energy)
    //                      (updater: StepNumber -> IRando -> Sorter -> Sorter * RandomSeed) 
    //                      (annealer:annealer) =
    //    let seed = startingSeed
    //    let gu  = Guid.NewGuid()
    //    let step = StepNumber.fromInt 0
    //    let fitness = eval step startingVal
    //    {
    //       sHC.trackId = gu;
    //       sHC.stepNumber = step;
    //       sHC.seed = seed; 
    //       sHC.curVal = startingVal;
    //       sHC.curEnergy= fitness;
    //       sHC.updater = updater;
    //       sHC.evaluator = eval;
    //       annealer = annealer;
    //    }













//type sHC<'T> = 
//    {
//       trackId:Guid;
//       stepNumber:StepNumber; 
//       seed:RandomSeed; 
//       curVal:'T;
//       curEnergy:Energy;
//       updater: StepNumber -> IRando -> 'T -> 'T * RandomSeed
//       evaluator: StepNumber -> 'T -> Energy
//       annealer:annealer
//    }

//module SHC =

//    let update (hist:List<sHC<'T>>) = 
//        let shc = hist |> List.head
//        let randy = Rando.LcgFromSeed shc.seed
//        let newVal, newSeed = 
//                    shc.updater 
//                        shc.stepNumber 
//                        randy 
//                        shc.curVal
//        let newFitness = shc.evaluator shc.stepNumber newVal
//        let curStep = shc.stepNumber
//        let caster = fun () -> randy.NextFloat
//        let anF = shc.annealer.chooser
//        let useNewOne = anF shc.curEnergy newFitness caster curStep
//        if (useNewOne) then
//            let newShc =
//                {
//                    sHC.trackId = shc.trackId;
//                    stepNumber = shc.stepNumber |> StepNumber.increment;
//                    curVal = newVal;
//                    curEnergy = newFitness;
//                    seed = newSeed;
//                    updater = shc.updater;
//                    evaluator =  shc.evaluator;
//                    annealer = shc.annealer;
//                }
//            newShc::hist
//        else
//            let newShc =
//                {
//                    sHC.trackId = shc.trackId;
//                    stepNumber = shc.stepNumber |> StepNumber.increment;
//                    curVal = shc.curVal;
//                    curEnergy = shc.curEnergy;
//                    seed = newSeed;
//                    updater = shc.updater;
//                    evaluator =  shc.evaluator;
//                    annealer = shc.annealer;
//                }
//            hist



