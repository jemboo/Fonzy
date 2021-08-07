namespace global
open System


type ShcId = private ShcId of Guid
module ShcId =
    let value (ShcId v) = v
    let create id = Ok (ShcId id)
    let fromGuid (id:Guid) = create id |> Result.ExtractOrThrow


type sHC<'A> = 
    {
       id:ShcId;
       archive: 'A list;
       mutator: 'A -> Result<'A, string>
       evaluator: 'A -> Result<'A, string>
       annealer: 'A -> 'A -> Result<'A, string>
       updater: 'A list -> 'A -> Result<'A list, string>
    }

module SHC =

    let newGen  (mutator:'A -> Result<'A, string>)
                (evaluator:'A -> Result<'A, string>) 
                (curGen:'A) =
        result {
            let! aMut = curGen |> mutator
            return! aMut |> evaluator
        }

    let update (shc:sHC<'A>) =
        result {
            let aCurr = shc.archive |> List.head
            let! newGen = aCurr |> newGen shc.mutator shc.evaluator
            let! aNext = shc.annealer aCurr newGen
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


type sorterShc = 
    {
        step:StepNumber;
        rngGen:RngGen; 
        sorter:Sorter;
        switchUses:SwitchUses option
        perf:SortingEval.sorterPerf option
        energy:Energy option;
    }


module SorterShc =

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





type sorterMutatorSpec =
    | Constant of sorterMutationType

type sorterEvaluatorSpec =
    | PerfBin of StageWeight

type sorterUpdaterSpec =
    | SaveEnergy
    | SavePerf
    | SaveSorter
    | SaveAll



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



type sorterShcSpec = 
    {
       rngGen:RngGen; 
       sorter:Sorter;
       mutator:sorterMutatorSpec;
       updater: sorterUpdaterSpec;
       evaluator: sorterEvaluatorSpec;
       annealer:annealerSpec
    }



module SorterShcSpec = 

    let makeId (s:sorterShcSpec) = 
        let gu = [s :> obj] |> GuidUtils.guidFromObjList
        ShcId.fromGuid gu

    let makeAnnealer (a:annealerSpec) = 
        fun (shcCurrent:sorterShc)
            (shcNew:sorterShc) -> 
        shcCurrent |> Ok


    let makeMutator (m:sorterMutatorSpec) = 
        match m with
        | Constant smt -> 
            fun (shcCurrent:sorterShc) -> 
            shcCurrent |> Ok


    let makeEvaluator (e:sorterEvaluatorSpec) = 
        match e with
        | PerfBin tw -> 
            fun (shcCurrent:sorterShc) -> 
            shcCurrent |> Ok


    let _sorterUpdate (savePerf:bool)
                     (saveSorter:bool)
                     (saveAll:bool) = 
        fun (archive:sorterShc list) 
            (shcNew:sorterShc) ->
            archive |> Ok
            
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
            


    let makeUpdater (u:sorterUpdaterSpec) =
        match u with
        | SaveEnergy -> _sorterUpdate false false false
        | SavePerf -> _sorterUpdate true false false
        | SaveSorter -> _sorterUpdate true true false
        | SaveAll -> _sorterUpdate true true true



    let make (spec:sorterShcSpec) =

        let sshcI = {
            sorterShc.step = StepNumber.fromInt 0;
            sorterShc.energy = None;
            sorterShc.perf = None;
            sorterShc.rngGen = spec.rngGen;
            sorterShc.sorter = spec.sorter;
            sorterShc.switchUses = None
        }
        result {
            let evaluator = makeEvaluator spec.evaluator
            let! sshc0 = evaluator sshcI
            return  {
               id = makeId spec;
               archive = [sshc0];
               mutator = makeMutator spec.mutator;
               evaluator = evaluator;
               annealer = makeAnnealer spec.annealer
               updater = makeUpdater spec.updater;
            }
        }


