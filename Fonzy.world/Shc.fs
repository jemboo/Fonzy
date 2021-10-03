namespace global
open System

type ShcId = private ShcId of Guid
module ShcId =
    let value (ShcId v) = v
    let create id = Ok (ShcId id)
    let fromGuid (id:Guid) = create id |> Result.ExtractOrThrow

type ShcCount = private ShcCount of int
module ShcCount =
    let value (ShcCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName ShcCount 1 1000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow


type shcSaveDetails =
    | Always
    | IfBest
    | BetterThanLast
    | EnergyThresh of Energy
    | Never


type shcTermSpec = 
    | FixedLength of StepNumber
    | EnergyBased of Energy*StepNumber*StepNumber


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
            let! updated = shc.current |> newGen shc.mutator shc.evaluator
            let! aNext = shc.annealer shc.current updated
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
        result {
            let mutable shcCur = shc
            while (goOn shcCur) do
                let! shcNew = shcCur |> update
                shcCur <- shcNew
            return shcCur
        }

    let runBatch (shcs:sHC<'T,'A>[]) =
        let ree = shcs |> Array.Parallel.map(run)
        ree




type shcStageWeightSpec =
    | Constant of StageWeight


type sorterMutSpec =
    | Constant of sorterMutationType


type sorterEvalSpec =
    | PerfBin


type sorterShc = 
    {
        step:StepNumber;
        revision:RevNumber;
        rngGen:RngGen; 
        sorter:sorter;
        switchPfx:Switch[];
        switchUses:switchUses option
        perf:SortingEval.sorterPerf option
        energy:Energy option;
        bestEnergy:Energy option;
    }

type sorterShcSpec = 
    {
       rngGen:RngGen; 
       sorter:sorter;
       switchPfx:Switch[];
       mutatorSpec:sorterMutSpec;
       srtblSetType:sortableSetType;
       shcStageWeightSpec:shcStageWeightSpec;
       evalSpec: sorterEvalSpec;
       annealerSpec:annealerSpec;
       updaterSpec: shcSaveDetails;
       termSpec:shcTermSpec;
    }


type sorterShcArch = 
    {
        step:StepNumber;
        revision:RevNumber;
        rngGen:RngGen option; 
        sorter:sorter option;
        switchUses:switchUses option
        perf:SortingEval.sorterPerf
        energy:Energy;
    }


module SorterShcArch = 

    let toFull (shc:sorterShc) =
        {
            sorterShcArch.step = shc.step;
            sorterShcArch.revision = shc.revision;
            sorterShcArch.rngGen = Some shc.rngGen;
            sorterShcArch.sorter = Some shc.sorter;
            sorterShcArch.switchUses = shc.switchUses;
            sorterShcArch.perf = shc.perf |> Option.get;
            sorterShcArch.energy= shc.energy |> Option.get;
        }

    let toPartial (shc:sorterShc) =
        {
            sorterShcArch.step = shc.step;
            sorterShcArch.revision = shc.revision;
            sorterShcArch.rngGen = None;
            sorterShcArch.sorter = None;
            sorterShcArch.switchUses = None;
            sorterShcArch.perf = shc.perf |> Option.get;
            sorterShcArch.energy= shc.energy |> Option.get;
        }

    let toSorterShcArch (doFull:bool) 
                        (shc:sorterShc) =
        if doFull then 
            toFull shc
            else
            toPartial shc

module ShcStageWeightSpec =
    let constantWeight wgt =
        fun (shc:sorterShc) ->
            wgt

    let getStageWeight (spec:shcStageWeightSpec) =
        match spec with
        | shcStageWeightSpec.Constant tw -> constantWeight tw


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
                   (srtblSetType:sortableSetType) =
        result {
        
            let! sortableSet, pfxUses = SortableSetMaker.makeTNoRepo 
                                                    srtblSetType
            return
                fun (sShc:sorterShc) ->
                    result {
                        let stageW = ShcStageWeightSpec.getStageWeight swPk sShc       
                        let suPlan = Sorting.SwitchUsePlan.makeIndexes
                                        pfxUses
                                        (sShc.sorter.switches.Length |> SwitchCount.fromInt)
                        let swEvRecs = SortingOps.Sorter.eval sShc.sorter
                                               sortableSet.sortableSetImpl
                                               suPlan
                                               Sorting.eventGrouping.BySwitch
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
                            sorterShc.revision = sShc.revision
                            sorterShc.switchPfx = sShc.switchPfx
                            sorterShc.switchUses = Some switchUses
                        }
                    }
          }
    
    let isCurrentBest (shc:sorterShc) =
        match shc.energy, shc.bestEnergy  with
        | Some e, Some be -> Energy.value be > Energy.value e 
                        // current energy is better than best (until now)
        | None, Some _ -> true
        | _, None -> failwith "sorterShc bestEnergy missing"


module SorterShcSpec = 

    let makeId (s:sorterShcSpec) = 
        let gu = [s :> obj] |> GuidUtils.guidFromObjList
        ShcId.fromGuid gu


    let makeAnnealer (anSpec:annealerSpec) = 
        fun (shcCurrent:sorterShc)
            (shcNew:sorterShc) -> 
            result {
                let! curE = shcCurrent |> SorterShc.getEnergy
                let! newE = shcNew |> SorterShc.getEnergy
                let randy = shcNew.rngGen |> Rando.fromRngGen
                let chooser() =
                    randy.NextFloat
                let annF = Annealer.make anSpec
                let pickNew = annF curE newE chooser shcNew.step
                if pickNew then 
                    return shcNew else
                    let newRng = shcCurrent.rngGen 
                                    |> Rando.fromRngGen
                                    |> Rando.toRngGen
                    return { shcCurrent with 
                                step = shcNew.step;
                                rngGen = newRng }
            }


    let makeMutator (m:sorterMutSpec) = 
        match m with
        | sorterMutSpec.Constant smt -> 
            fun (shcCurrent:sorterShc) -> 
            result {
                let randy = shcCurrent.rngGen |> Rando.fromRngGen
                let sorterMut = SorterMutate.mutate smt randy shcCurrent.sorter
                return {
                    step = shcCurrent.step |> StepNumber.increment;
                    revision = shcCurrent.revision |> RevNumber.increment;
                    rngGen = randy |> Rando.toRngGen; 
                    sorter = sorterMut;
                    switchPfx = shcCurrent.switchPfx;
                    switchUses = None;
                    perf = None;
                    energy = None;
                    bestEnergy = shcCurrent.bestEnergy;
                }
            }


    let makeEvaluator (spec:sorterShcSpec) = 
        match spec.evalSpec with
        | PerfBin -> SorterShc.sorterEvalPerfBin
                        spec.shcStageWeightSpec
                        spec.srtblSetType
                  

    let makeUpdater (saveDetails:shcSaveDetails) =
        fun (arch:sorterShcArch list) 
            (newT:sorterShc) ->
            let lastArch = arch |> List.head

            let improvement =  Energy.isBetterThan 
                                        (newT.energy |> Option.get)
                                        lastArch.energy

            if ((newT.revision = lastArch.revision) || 
                (not improvement)) then
                arch |> Ok else

                let curBest = newT |> SorterShc.isCurrentBest

                let threshB (thresh:Energy)  =  
                        Energy.isBetterThan 
                                (newT.energy |> Option.get)
                                thresh
                match saveDetails with
                    | Always -> 
                        (newT |> SorterShcArch.toFull) :: arch 
                        |> Ok
                    | IfBest ->  
                        (newT |> SorterShcArch.toSorterShcArch curBest) :: arch 
                        |> Ok
                    | BetterThanLast ->  
                        (newT |> SorterShcArch.toSorterShcArch improvement) :: arch 
                        |> Ok
                    | EnergyThresh e -> 
                        let useNew = (threshB e) && improvement
                        (newT |> SorterShcArch.toSorterShcArch useNew) :: arch 
                        |> Ok
                    | Never -> 
                        (newT |> SorterShcArch.toPartial) :: arch 
                        |> Ok


    let makeBadUpdater (saveDetails:shcSaveDetails) =
        fun (arch:sorterShcArch list) 
            (newT:sorterShc) ->
            "bad updater" |> Error


    let makeTerminator (spec:shcTermSpec) =
        match spec with
        | FixedLength x -> fun (shc:sorterShc) -> 
             (StepNumber.value shc.step) > (StepNumber.value x)
        | EnergyBased (e,n,x) -> fun shc -> 
            ((StepNumber.value shc.step) > (StepNumber.value n)
             &&
             (Energy.value (shc.energy |> Option.get)) < (Energy.value e))
            ||
            (StepNumber.value shc.step) > (StepNumber.value x)


    let toShc (spec:sorterShcSpec) =

        let sshcI = {
            sorterShc.step = StepNumber.fromInt 0;
            sorterShc.revision = RevNumber.fromInt 0;
            sorterShc.energy = None;
            sorterShc.perf = None;
            sorterShc.rngGen = spec.rngGen;
            sorterShc.switchPfx = spec.switchPfx;
            sorterShc.sorter = spec.sorter;
            sorterShc.switchUses = None;
            bestEnergy = None;
        }
        result {
            let! evaluator = makeEvaluator spec
            let! sshc0 = evaluator sshcI
            let arch = sshc0 |> SorterShcArch.toFull
            return  {
               id = makeId spec;
               current = sshc0;
               archive = [arch];
               mutator = makeMutator spec.mutatorSpec;
               evaluator = evaluator;
               annealer = makeAnnealer spec.annealerSpec;
               updater = makeUpdater spec.updaterSpec;
               terminator = makeTerminator spec.termSpec;
            }
        }


type sssrgType = 
    | Annealer of annealerSpec
    | Mutation of sorterMutSpec
    | RndGen
    | Sorters of sorterSetGen
    | StageWeight of shcStageWeightSpec



type sorterShcSpecRndGen = 
    {
       baseSpec:sorterShcSpec;
       sssrgType:sssrgType;
       rndGen:RngGen;
       count:ShcCount
    }


module SorterShcSpecRndGen =

    let swapAnnealers  rndG 
                       (shc:sorterShcSpec)
                       (count: ShcCount)
                       (endPt:annealerSpec) = 
        let annAc randy =
            result {
               return! 
                   match shc.annealerSpec, endPt with
                   | annealerSpec.Constant c1, annealerSpec.Constant c2  -> 
                        Combinatorics.draw1D (Temp.value c1) (Temp.value c2) randy
                        |> Seq.map(fun t -> (Temp.fromFloat t) |> annealerSpec.Constant)
                        |> Seq.take (ShcCount.value count)
                        |> Ok
                   | annealerSpec.Exp (t1, d1), annealerSpec.Exp (t2, d2) -> 
                        Combinatorics.draw2D (Temp.value t1) d1 
                                             (Temp.value t2) d2 randy
                        |> Seq.map(fun tup -> ((Temp.fromFloat (fst tup)), (snd tup)) |> annealerSpec.Exp)
                        |> Seq.take (ShcCount.value count)
                        |> Ok
                   | _ -> "annealerSpecs must me the same type" |> Error
            }

        result {
            let randy = rndG |> Rando.fromRngGen
            let! anns = annAc randy
            return anns 
                    |> Seq.map(fun an ->  
                        { shc with 
                              rngGen = (randy |> Rando.toRngGen);
                              annealerSpec = an })
        }


    let swapMut rndG 
                (shc:sorterShcSpec)
                (count: ShcCount)
                (smc:sorterMutSpec) = 
        "Not impl" |> Error


    let swapRndGen rndG 
                   (shc:sorterShcSpec)
                   (count: ShcCount) = 
        let randy = rndG |> Rando.fromRngGen
        seq {0 .. ((ShcCount.value count) - 1) }
        |> Seq.map( fun _ -> 
                { shc with 
                      rngGen = (randy |> Rando.toRngGen)})


    let swapSorters (srSrepo: (SorterSetId->sorterSet) option) 
                    rndG 
                    (baseSpec:sorterShcSpec) 
                    (count: ShcCount)
                    (ssg:sorterSetGen) = 
        result {
            let randy = rndG |> Rando.fromRngGen
            let! srtrSet = SorterSetGen.createSorterSet srSrepo ssg
            let srtrA = srtrSet.sorters |> Map.toArray |> Array.map(snd)
            return seq { 0 .. ((ShcCount.value count) - 1) }
            |> Seq.map( fun dex -> 
                    { baseSpec with 
                          rngGen = (randy |> Rando.toRngGen);
                          sorter = srtrA.[dex % srtrA.Length]})
        }


    let swapStageWeight rndG 
                       (shc:sorterShcSpec)
                       (count: ShcCount)
                       (sws:shcStageWeightSpec) = 
        "Not impl" |> Error



    let generate (sbSrepo: (SortableSetId->sorterSet) option) 
                 (srSrepo: (SorterSetId->sorterSet) option) 
                 (sssrg:sorterShcSpecRndGen) = 
        match sssrg.sssrgType with

        | Annealer annSpec -> swapAnnealers 
                                sssrg.rndGen 
                                sssrg.baseSpec 
                                sssrg.count 
                                annSpec

        | Mutation mutSpec -> swapMut 
                                sssrg.rndGen 
                                sssrg.baseSpec 
                                sssrg.count 
                                mutSpec

        | RndGen -> swapRndGen 
                               sssrg.rndGen
                               sssrg.baseSpec
                               sssrg.count |> Ok

        | Sorters ssG -> swapSorters srSrepo
                                     sssrg.rndGen 
                                     sssrg.baseSpec 
                                     sssrg.count 
                                     ssG

        | StageWeight stw -> swapStageWeight 
                                    sssrg.rndGen 
                                    sssrg.baseSpec 
                                    sssrg.count 
                                    stw


type sorterShcSet = 
    {
       members:Map<sorterShcSpec, sorterShc>;
    }