namespace global
open System


module SorterShc =

    let sorterEvalPerfBin
                   (swPk:sorterStageWeightSpec) 
                   (srtblSetType:sortableSetType) =
        result {
        
            let! sortableSet, pfxUses = SortableSetMaker.makeTNoRepo srtblSetType
            return
                fun (sShc:sorterShc) ->
                    result {
                       // Console.WriteLine(sprintf "%d" (StepNumber.value sShc.step))
                        let stageW = ShcStageWeightSpec.getStageWeight swPk sShc       
                        let suPlan = Sorting.SwitchUsePlan.makeIndexes
                                        pfxUses
                                        (sShc.sorter.switches.Length |> SwitchCount.fromInt)

                        //let trimSorter = ((StepNumber.value sShc.step) % 20000) > 10000
                        //let! trimmedSorter =
                        //    match trimSorter with
                        //    | true -> sShc.sorter |> Sorter.trimLength false (SwitchCount.fromInt (sShc.sorter.switches.Length - (Degree.value sShc.sorter.degree) / 2))
                        //    | _ -> sShc.sorter |> Ok

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
                        return {sShc with 
                                    bestEnergy = None;
                                    energy = energy;
                                    energyDelta = None
                                    perf = Some perf;
                                    switchUses = Some switchUses
                                    lastSwitchUsed = switchUses.weights 
                                                                |> Array.findIndexBack(fun v -> v > 0)
                                                                |> SwitchCount.fromInt
                               }
                    }
          }
    


module SorterShcSpec = 

    let makeEvaluator (spec:sorterShcSpec) = 
        match spec.evalSpec with
        | sorterEvalSpec.PerfBin -> 
                SorterShc.sorterEvalPerfBin
                        spec.sorterStageWeightSpec
                        spec.srtblSetType
                  


module SorterShcSpecRndGen =

    let swapAnnealers  rndG 
                       (baseSpec:sorterShcSpec)
                       (count: ShcCount)
                       (endPt:annealerSpec) = 
        let annAc randy =
            result {
               return! 
                   match baseSpec.annealerSpec, endPt with
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
                        { baseSpec with 
                              rngGen = (randy |> Rando.nextRngGen);
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
                      rngGen = (randy |> Rando.nextRngGen)})


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
                          rngGen = (randy |> Rando.nextRngGen);
                          sorter = srtrA.[dex % srtrA.Length]})
        }


    let swapStageWeight rndG 
                       (shc:sorterShcSpec)
                       (count: ShcCount)
                       (sws:sorterStageWeightSpec) = 
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

        let sshcInitial = {
            sorterShc.step = StepNumber.fromInt 0;
            isNew = true;
            advanceCount = 0;
            retreatCount = 0;
            energy = None;
            perf = None;
            rngGen = spec.rngGen;
            sorter = spec.sorter;
            switchUses = None;
            bestEnergy = None;
            energyDelta = None;
            lastSwitchUsed = 0 |> SwitchCount.fromInt;
        }
        result {
            let! evaluator = SorterShcSpec.makeEvaluator spec
            return  {
               sHC.id = SorterShcSpec.makeId spec;
               current = sshcInitial;
               archive = [];
               mutator = SorterShcSpec.makeMutator spec.mutatorSpec;
               evaluator = evaluator;
               annealer = SorterShcSpec.makeAnnealer spec.annealerSpec;
               logger = SorterShcSpec.makeLogger spec.loggerSpec;
               terminator = SorterShcSpec.makeTerminator spec.termSpec;
            }
        }



module SorterSHCset =

    let make (specs: seq<sorterShcSpec>) =
        SHCset.make (SorterShcSpec.makeId) (SHC.fromSorterShcSpec) specs


    let getResults (shcs:sHCset<sorterShcSpec, sorterShc, sorterShcArch>) = 
        let memberIds = shcs.memberMap |> Map.toArray |> Array.map(fst)
        let _rpt id = 
            let spec = shcs.specMap.[id]
            let aR = shcs.memberMap.[id]
            let rpt = 
                match aR with
                | Ok m -> ("OK", m.archive |> List.toArray)
                | Error m -> ("error", [||])
            {
                sorterShcResult.id = id;
                spec = spec;
                msg = (fst rpt)
                archives = (snd rpt)
            }

        {sorterShcResults.members =  memberIds |> Array.map(_rpt) }
