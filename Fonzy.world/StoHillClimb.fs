namespace global
open System


type ShcId = private ShcId of Guid
module ShcId =
    let value (ShcId v) = v
    let create id = Ok (ShcId id)
    let fromGuid (id:Guid) = create id |> Result.ExtractOrThrow


type sHC<'T,'A> = 
    {
       id:ShcId;
       current: 'T;
       archive: 'A list;
       mutator: 'T -> Result<'T, string>
       evaluator: 'T -> Result<'T, string>
       annealer: 'T -> 'T -> Result<'T, string>
       updater: 'A list -> 'T -> Result<'A list, string>
       terminator: 'T -> bool
    }

module SHC =

    let newGen  (mutator:'A -> Result<'A, string>)
                (evaluator:'A -> Result<'A, string>) 
                (curGen:'A) =
        result {
            let! aMut = curGen |> mutator
            return! aMut |> evaluator
        }

    let update (shc:sHC<'T,'A>) =
        result {
            let! newGen = shc.current |> newGen shc.mutator shc.evaluator
            let! aNext = shc.annealer shc.current newGen
            let! aLst = aNext |> shc.updater shc.archive
            return
                {
                    sHC.id = shc.id;
                    sHC.current = aNext;
                    sHC.archive = aLst;
                    sHC.mutator = shc.mutator;
                    sHC.updater = shc.updater;
                    sHC.evaluator = shc.evaluator;
                    sHC.annealer = shc.annealer;
                    sHC.terminator = shc.terminator;
                }
        }

    let run (shc:sHC<'T,'A>) =
        let goOn (s) = 
            not (s.terminator s.current)
        let mutable shcCur = shc
        while (goOn shcCur) do
            shcCur <-  shcCur |> update |> Result.ExtractOrThrow
            



type sorterShc = 
    {
        step:StepNumber;
        rngGen:RngGen; 
        sorter:Sorter;
        switchPfx:Switch[];
        switchUses:switchUses option
        perf:SortingEval.sorterPerf option
        energy:Energy option;
        bestEnergy:Energy option;
    }


type sorterShcArch = 
    {
        step:StepNumber;
        rngGen:RngGen option; 
        sorter:Sorter option;
        switchUses:switchUses option
        perf:SortingEval.sorterPerf
        energy:Energy;
    }

module SorterShcArch = 

    let toFull (shc:sorterShc) =
        {
            sorterShcArch.step = shc.step;
            sorterShcArch.rngGen = Some shc.rngGen;
            sorterShcArch.sorter = Some shc.sorter;
            sorterShcArch.switchUses = shc.switchUses;
            sorterShcArch.perf = shc.perf |> Option.get;
            sorterShcArch.energy= shc.energy |> Option.get;
        }

    let toPartial (shc:sorterShc) =
        {
            sorterShcArch.step = shc.step;
            sorterShcArch.rngGen = None;
            sorterShcArch.sorter = None;
            sorterShcArch.switchUses = None;
            sorterShcArch.perf = shc.perf |> Option.get;
            sorterShcArch.energy= shc.energy |> Option.get;
        }

    let toSorterShcArch (doPartial:bool) 
                        (shc:sorterShc) =
        if doPartial then 
            toPartial shc
            else
            toFull shc


type shcStageWeightSpec =
    | Constant of StageWeight

module ShcStageWeightSpec =
    let constantWeight wgt =
        fun (shc:sorterShc) ->
            wgt

    let getStageWeight (spec:shcStageWeightSpec) =
        match spec with
        | Constant tw -> constantWeight tw


type sorterMutatorSpec =
    | Constant of sorterMutationType

type sorterEvaluatorSpec =
    | PerfBin

type sorterUpdaterSpec =
    | AlwaysFull
    | FullIfBest
    | FullByLast
    | FullByEnergy of Energy
    | NeverFull

type sorterTerminatorSpec = 
    | FixedLength of StepNumber
    | EnergyBased of Energy*StepNumber*StepNumber


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


    let sorterEvalPerfBin
                   (swPk:shcStageWeightSpec) 
                   (ssP:sortableSetSpecReduced) =

        fun (sShc:sorterShc) ->
            result {
                let! sortableSet, pfxUses = SortableSetSpecReduced.make ssP
                let stageW = ShcStageWeightSpec.getStageWeight swPk sShc       
                let suPlan = Sorting.SwitchUsePlan.makeIndexes
                                pfxUses
                                (sShc.sorter.switches.Length |> SwitchCount.fromInt)
                let swEvRecs = SortingOps.Sorter.eval sShc.sorter
                                       sortableSet
                                       suPlan
                                       Sorting.EventGrouping.BySwitch
                let switchUses = swEvRecs
                                 |> SortingEval.SwitchEventRecords.getSwitchUses
                let usedSwitches = sShc.sorter 
                                   |> SwitchUses.getUsedSwitches switchUses
                let perf = 
                    {
                        SortingEval.sorterPerf.successful = swEvRecs 
                            |> SortingEval.SwitchEventRecords.getAllSortsWereComplete
                            |> Some
                        SortingEval.sorterPerf.usedStageCount = 
                                Stage.getStageCount 
                                    sShc.sorter.degree 
                                    usedSwitches
                        SortingEval.sorterPerf.usedSwitchCount = 
                                SwitchCount.fromInt usedSwitches.Length
                    }

                let energy = perf |> SorterFitness.fromSorterPerf sShc.sorter.degree stageW
                                  |> Some
                let bestEnergy = Energy.betterEnergy energy sShc.bestEnergy
                return  {
                    sorterShc.bestEnergy = bestEnergy
                    sorterShc.energy = energy
                    sorterShc.perf = Some perf
                    sorterShc.rngGen = sShc.rngGen
                    sorterShc.sorter = sShc.sorter
                    sorterShc.step = sShc.step
                    sorterShc.switchPfx = sShc.switchPfx
                    sorterShc.switchUses = Some switchUses
                }
            }

    
    let isCurrentBest (shc:sorterShc) =
        match shc.energy, shc.bestEnergy  with
        | Some e, Some be -> Energy.value be > Energy.value e 
                        // current energy is better than best (until now)
        | None, Some _ -> true
        | _, None -> failwith "sorterShc bestEnergy missing"



type sorterShcSpec = 
    {
       rngGen:RngGen; 
       sorter:Sorter;
       switchPfx:Switch[];
       mutator:sorterMutatorSpec;
       shcSortableSetSpec:sortableSetSpecReduced;
       shcStageWeightSpec:shcStageWeightSpec;
       evaluator: sorterEvaluatorSpec;
       annealer:annealerSpec;
       updater: sorterUpdaterSpec;
       terminator:sorterTerminatorSpec;
    }

module SorterShcSpec = 

    let makeId (s:sorterShcSpec) = 
        let gu = [s :> obj] |> GuidUtils.guidFromObjList
        ShcId.fromGuid gu


    let makeAnnealer (a:annealerSpec) = 
        fun (shcCurrent:sorterShc)
            (shcNew:sorterShc) -> 
            result {
                let! curE = shcCurrent |> SorterShc.getEnergy
                let! newE = shcNew |> SorterShc.getEnergy
                let randy = shcNew.rngGen |> Rando.fromRngGen
                let chooser() =
                    randy.NextFloat
                let annF = Annealer.make a
                let pickNew = annF curE newE chooser shcNew.step
                if pickNew then return shcNew else return shcCurrent
            }


    let makeMutator (m:sorterMutatorSpec) = 
        match m with
        | sorterMutatorSpec.Constant smt -> 
            fun (shcCurrent:sorterShc) -> 
            result {
                let randy = shcCurrent.rngGen |> Rando.fromRngGen
                let sorterMut = SorterMutate.mutate smt randy shcCurrent.sorter
                return {
                    step = shcCurrent.step |> StepNumber.increment;
                    rngGen = randy |> Rando.toRngGen; 
                    sorter = sorterMut;
                    switchPfx = shcCurrent.switchPfx;
                    switchUses = None;
                    perf = None;
                    energy = None;
                    bestEnergy = None;
                }
            }


    let makeEvaluator (spec:sorterShcSpec) = 
        match spec.evaluator with
        | PerfBin -> SorterShc.sorterEvalPerfBin
                        spec.shcStageWeightSpec
                        spec.shcSortableSetSpec


    let makeUpdater (u:sorterUpdaterSpec) =
        fun (arch:sorterShcArch list) 
            (newT:sorterShc) ->
            let lastArch = arch |> List.head
            let curBest = newT |> SorterShc.isCurrentBest
            let improvement =  Energy.isBetterThan 
                                        (newT.energy |> Option.get)
                                        lastArch.energy
            let threshB (thresh:Energy)  =  
                    Energy.isBetterThan 
                            (newT.energy |> Option.get)
                            thresh
            match u with
                | AlwaysFull -> 
                    (newT |> SorterShcArch.toFull) :: arch 
                    |> Ok
                | FullIfBest ->  
                    (newT |> SorterShcArch.toSorterShcArch curBest) :: arch 
                    |> Ok
                | FullByLast ->  
                    (newT |> SorterShcArch.toSorterShcArch improvement) :: arch 
                    |> Ok
                | FullByEnergy e -> 
                    let useNew = (threshB e) && improvement
                    (newT |> SorterShcArch.toSorterShcArch useNew) :: arch 
                    |> Ok
                | NeverFull -> 
                    (newT |> SorterShcArch.toPartial) :: arch 
                    |> Ok


    let makeTerminator (spec:sorterTerminatorSpec) =
        match spec with
        | FixedLength x -> fun (shc:sorterShc) -> 
             (StepNumber.value shc.step) > (StepNumber.value x)
        | EnergyBased (e,n,x) -> fun shc -> 
            ((StepNumber.value shc.step) > (StepNumber.value n)
             &&
             (Energy.value (shc.energy |> Option.get)) < (Energy.value e))
            ||
            (StepNumber.value shc.step) > (StepNumber.value x)

    let make (spec:sorterShcSpec) =

        let sshcI = {
            sorterShc.step = StepNumber.fromInt 0;
            sorterShc.energy = None;
            sorterShc.perf = None;
            sorterShc.rngGen = spec.rngGen;
            sorterShc.switchPfx = spec.switchPfx;
            sorterShc.sorter = spec.sorter;
            sorterShc.switchUses = None;
            bestEnergy = None;
        }
        result {
            let evaluator = makeEvaluator spec
            let! sshc0 = evaluator sshcI
            let arch = sshc0 |> SorterShcArch.toFull
            return  {
               id = makeId spec;
               current = sshc0;
               archive = [arch];
               mutator = makeMutator spec.mutator;
               evaluator = evaluator;
               annealer = makeAnnealer spec.annealer
               updater = makeUpdater spec.updater;
               terminator = makeTerminator spec.terminator;
            }
        }


