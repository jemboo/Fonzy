﻿namespace global
open System


module SorterShcSpec2 = 

    let makeEvaluator (spec:sorterShcSpec2) = 
        match spec.evalSpec with
        | sorterEvalSpec.PerfBin -> 
                SorterShc.sorterEvalPerfBin
                        spec.sorterStageWeightSpec
                        spec.srtblSetType
                  


module SorterShcSpecRndGen2 =

    let swapAnnealers  rndG 
                       (baseSpec:sorterShcSpec2)
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
                (shc:sorterShcSpec2)
                (count: ShcCount)
                (smc:sorterMutSpec) = 
        "Not impl" |> Error


    let swapRndGen rndG 
                   (shc:sorterShcSpec2)
                   (count: ShcCount) = 
        let randy = rndG |> Rando.fromRngGen
        seq {0 .. ((ShcCount.value count) - 1) }
        |> Seq.map( fun _ -> 
                { shc with 
                      rngGen = (randy |> Rando.nextRngGen)})


    let swapSorters (srSrepo: (SorterSetId->sorterSet) option) 
                    rndG 
                    (baseSpec:sorterShcSpec2) 
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
                       (shc:sorterShcSpec2)
                       (count: ShcCount)
                       (sws:sorterStageWeightSpec) = 
        "Not impl" |> Error



    let generate (sbSrepo: (SortableSetId->sorterSet) option) 
                 (srSrepo: (SorterSetId->sorterSet) option) 
                 (sssrg:sorterShcSpecRndGen2) = 
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



module SHC2 =

    let fromSorterShcSpec (spec:sorterShcSpec2) =

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
            lastSwitchUsed = 0 |> SwitchCount.fromInt
        }
        result {
            let! evaluator = SorterShcSpec2.makeEvaluator spec
            return  {
               sHC2.id = SorterShcSpec2.makeId spec;
               current = sshcInitial;
               mutator = SorterShcSpec.makeMutator spec.mutatorSpec;
               evaluator = evaluator;
               annealer = SorterShcSpec.makeAnnealer spec.annealerSpec;
               terminator = SorterShcSpec.makeTerminator spec.termSpec;
            }
        }



module SorterSHCset2 =

    let make (specs: seq<sorterShcSpec2>) =
        SHCset2.make (SorterShcSpec2.makeId) (SHC2.fromSorterShcSpec) specs


    let getResults (shcs:sHCset2<sorterShcSpec2, sorterShc>) = 
        let memberIds = shcs.memberMap |> Map.toArray |> Array.map(fst)
        let _rpt id = 
            let aR = shcs.memberMap.[id]
            let rpt = 
                match aR with
                | Ok m -> ("OK", m.current |> SorterShcArch.toFull)
                | Error m -> ("error", SorterShcArch.dfltPartial)
            {
                sorterShcResult2.id = id;
                msg = (fst rpt)
                archive = (snd rpt)
            }

        { sorterShcResults2.members = memberIds |> Array.map(_rpt) }


    
    let reportShcDetails (shouldRepPerf:StepNumber->bool) 
                         (shouldRepBins:StepNumber->bool) 
                         (reportPerf:SortingEval.sorterPerf option -> unit)
                         (reportBins:unit->unit)
                         (addEvalToBins:SortingEval.sorterPerf option -> unit)
                         (addAnnealBins:SortingEval.sorterPerf option -> unit) 
                         (state:sHCstate) 
                         (srtrShc:sorterShc) 
                         (shc:sHC2<sorterShc>) =

        match state with
        | PostMutate -> 
            ()
        | PostEvaluate -> 
            addEvalToBins srtrShc.perf
        | PostAnnealer -> 
            addAnnealBins srtrShc.perf
            if (shouldRepPerf srtrShc.step) then
                reportPerf srtrShc.perf
            if (shouldRepBins srtrShc.step) then
                reportBins ()
 

    let sorterShcLoggerMaker (rootOutDir:FileDir) (context:obj) = 
        let srtrShcSpec2, sorterShc = context :?> sorterShcSpec2*Result<sHC2<sorterShc>, string>
        //let logFldr = match sorterShc with
        //             | Result.Ok shc -> (ShcId.value shc.id) |> string |> FileFolder.fromString
        //             | Error msg -> "error" |> FileFolder.fromString
        // let shcFolder = rootOutDir |> FileDir.appendFolder logFldr |> Result.ExtractOrThrow

        let id = match sorterShc with
                     | Result.Ok shc -> (ShcId.value shc.id)
                     | Error msg -> Guid.Empty

        let sorterShcArchName = "sorterShcArch"
        
        let sorterShc2Fdtos = FileDtoStream.forSorterShc2Dto (id |> string) sorterShcArchName rootOutDir
                           |> Result.ExtractOrThrow
        let sorterShc2Fdtos = sorterShc2Fdtos |> FileDtoStream.makeFileHeader
                           |> Result.ExtractOrThrow

        let bk = FileDtoStream.addToCatalog sorterShc2Fdtos

        let totReptSteps = srtrShcSpec2.termSpec |> ShcTermSpec.getMaxSteps
        let ticsPerLog = 20.0
        let reportSteps = StepNumber.logReporting totReptSteps ticsPerLog |> Array.map(StepNumber.value)
        let mutable trialSorterPerfs = []
        let mutable acceptedSorterPerfs = []
        let mutable genSpan = 0

        let yab = 0
        result {
            return
                fun (context:obj) ->
                    let state, srtrShc, shc, spec = context :?> sHCstate*sorterShc*sHC2<sorterShc>*sorterShcSpec2

                    match state with
                    | PostMutate -> 
                        genSpan <- genSpan + 1
                    | PostEvaluate -> 
                        trialSorterPerfs <- (srtrShc.perf |> Option.get ) :: trialSorterPerfs
                        ()
                    | PostAnnealer -> 
                        acceptedSorterPerfs <- (srtrShc.perf |> Option.get ) :: acceptedSorterPerfs
                        if(reportSteps |> Array.contains ((StepNumber.value srtrShc.step))) then

                            let trialPerfBins = 
                                trialSorterPerfs 
                                        |> SortingEval.SorterPerfBin.fromSorterPerfs

                            let acceptedPerfBins = 
                                acceptedSorterPerfs 
                                        |> SortingEval.SorterPerfBin.fromSorterPerfs
                            let dto = SorterShc2Dto.toDto 
                                            srtrShc 
                                            shc.id 
                                            (spec |> SorterShcSpec2.sorterReport)
                                            (spec |> SorterShcSpec2.seedReport)
                                            (spec |> SorterShcSpec2.mutReport)
                                            (spec |> SorterShcSpec2.tempReport)
                                            genSpan 
                                            trialPerfBins 
                                            acceptedPerfBins
                            let res = sorterShc2Fdtos |> FileDtoStream.append (seq { dto })
                                      |> Result.ExtractOrThrow
                            trialSorterPerfs <- []
                            acceptedSorterPerfs <- []
                            genSpan <- 0
                        ()
        }