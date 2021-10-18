﻿namespace global
open System


module SorterShc =

    let sorterEvalPerfBin
                   (swPk:shcStageWeightSpec) 
                   (srtblSetType:sortableSetType) =
        result {
        
            let! sortableSet, pfxUses = SortableSetMaker.makeTNoRepo srtblSetType
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

    let makeEvaluator (spec:sorterShcSpec) = 
        match spec.evalSpec with
        | sorterEvalSpec.PerfBin -> 
                SorterShc.sorterEvalPerfBin
                        spec.shcStageWeightSpec
                        spec.srtblSetType
                  

    let makeUpdater (saveDetails:shcSaveDetails) =
        fun (arch:sorterShcArch list) 
            (newT:sorterShc) ->
            if arch.Length = 0 then
                [newT |> SorterShcArch.toFull] |> Ok
            else
                let lastArch = arch |> List.head

                let improvement = Energy.isBetterThan 
                                            (newT.energy |> Option.get)
                                            lastArch.energy

                if newT.revision = lastArch.revision || not improvement then
                    arch |> Ok 
                else
                    let curBest = newT |> SorterShc.isCurrentBest

                    let threshB (thresh:Energy) =  
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

        | sssrgType.StageWeight stw -> 
                            swapStageWeight 
                                    sssrg.rndGen 
                                    sssrg.baseSpec 
                                    sssrg.count 
                                    stw



module SHC =

    let fromSorterShcSpec (spec:sorterShcSpec) =

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
            let! evaluator = SorterShcSpec.makeEvaluator spec
            return  {
               id = SorterShcSpec.makeId spec;
               current = sshcI;
               archive = [];
               mutator = SorterShcSpec.makeMutator spec.mutatorSpec;
               evaluator = evaluator;
               annealer = SorterShcSpec.makeAnnealer spec.annealerSpec;
               updater = SorterShcSpec.makeUpdater spec.updaterSpec;
               terminator = SorterShcSpec.makeTerminator spec.termSpec;
            }
        }



module sHCset = 
    let make<'S,'T,'A> (idGen: 'S->ShcId)
                       (maker: 'S->Result<sHC<'T,'A>, string>) 
                       (specs: seq<'S>) =
        let specA = specs |> Seq.toArray
        let specMap = specA |> Array.map(fun s -> (idGen s, s))
                            |> Map.ofSeq
        let memberMap = specA |> Array.map(fun s -> (idGen s, maker s))
                              |> Map.ofSeq

        {sHCset.specs= specMap; sHCset.members = memberMap}

    let makeSorterShcSet (specs: seq<sorterShcSpec>) =
        make (SorterShcSpec.makeId) (SHC.fromSorterShcSpec) specs

    let runBatch (useP:UseParallel) 
                 (shcs:sHCset<'S,'T,'A>) = 
        let _runn (id:ShcId) (shcr:Result<sHC<'T,'A>, string>) =
            match shcr with
            | Ok shc -> (id, SHC.run shc)
            | Error m -> (id, sprintf "error creating spec: %s" m |> Error)
            

        let mms = 
            match UseParallel.value(useP) with
            | true  -> shcs.members 
                        |> Map.toArray
                        |> Array.Parallel.map(fun tup -> _runn (fst tup) (snd tup))
                        |> Map.ofSeq
            | false -> shcs.members 
                        |> Map.toArray
                        |> Array.map(fun tup -> _runn (fst tup) (snd tup))
                        |> Map.ofSeq

        {shcs with members = mms}



    let getResults (shcs:sHCset<sorterShcSpec, sorterShc, sorterShcArch>) = 
        let memberIds = shcs.members |> Map.toArray |> Array.map(fst)
        let _rpt id = 
            let spec = shcs.specs.[id]
            let aR = shcs.members.[id]
            let rpt = 
                match aR with
                | Ok m -> ("OK", m.archive |> List.toArray)
                | Error m -> ("error", [||])
            {
                sorterShcResult.spec = spec;
                sorterShcResult.msg = (fst rpt)
                sorterShcResult.archives = (snd rpt)
            }

        {sorterShcResults.members =  memberIds |> Array.map(_rpt) }