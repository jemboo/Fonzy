namespace global
open System


type sorterShcSpec = 
    {
       rngGen:RngGen; 
       sorter:sorter;
       mutatorSpec:sorterMutSpec;
       srtblSetType:sortableSetType;
       sorterStageWeightSpec:sorterStageWeightSpec;
       evalSpec: sorterEvalSpec;
       annealerSpec:annealerSpec;
       loggerSpec: shcSaveDetails;
       termSpec:shcTermSpec;
    }


module SorterShcSpec = 

    let makeId (s:sorterShcSpec) = 
        let gu = seq {
                       s.annealerSpec :> obj;
                       s.evalSpec :> obj;
                       s.mutatorSpec :> obj; 
                       s.rngGen :> obj;
                       s.sorterStageWeightSpec :> obj; 
                       s.sorter :> obj;
                       s.srtblSetType :> obj;
                       s.termSpec :> obj;
                       s.loggerSpec :> obj;} 
                  |> GuidUtils.guidFromObjs
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
                    
                    let newRng = shcCurrent.rngGen 
                                    |> Rando.fromRngGen
                                    |> Rando.nextRngGen

                    if pickNew then 
                       let ceV = curE |> Energy.value
                       let neV = newE |> Energy.value
                       let advInc = match (ceV > neV) with
                                    | true -> 1
                                    | false -> 0
                       let retInc = match (ceV < neV) with
                                    | true -> 1
                                    | false -> 0

                       return { shcNew with
                                   bestEnergy = Energy.betterEnergy shcNew.energy shcCurrent.bestEnergy
                                   energyDelta  = Energy.delta shcCurrent.energy shcNew.energy
                                   advanceCount = shcNew.advanceCount + advInc
                                   retreatCount = shcNew.retreatCount + retInc
                                   rngGen = newRng
                               }
                    else
                       return { shcCurrent with 
                                    sorterShc.step = shcCurrent.step |> StepNumber.increment;
                                    isNew = false;
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
                    sorterShc.step = shcCurrent.step |> StepNumber.increment;
                    isNew = true;
                    advanceCount = shcCurrent.advanceCount;
                    retreatCount = shcCurrent.retreatCount;
                    rngGen = randy |> Rando.nextRngGen; 
                    sorter = sorterMut;
                    switchUses = None;
                    perf = None;
                    energy = None;
                    energyDelta = None;
                    bestEnergy = None;
                }
            }

    let makeLogger (saveDetails:shcSaveDetails) =
        fun (arch:sorterShcArch list) 
            (newShc:sorterShc) ->
            
            let _threshB (thresh:Energy) =  
                Energy.isBetterThan 
                            (newShc.energy |> Option.get)
                            thresh
            let _addItOn (full:bool) =
                if full then
                    (newShc |> SorterShcArch.toFull) :: arch |> Ok
                else    
                    (newShc |> SorterShcArch.toPartial) :: arch |> Ok

            match saveDetails with
                | Always -> _addItOn true
                | IfBest ->  
                    _addItOn (newShc |> SorterShc.isCurrentBest)
                | BetterThanLast ->  
                    _addItOn (_threshB ((arch |> List.head) |> SorterShcArch.getEnergy))
                | EnergyThresh e -> 
                    _addItOn (_threshB e)
                | ForSteps stps -> 
                    if (newShc |> SorterShc.logPolicy0) then
                        //_addItOn true
                        arch |> Ok
                    else if  (stps |> Array.contains newShc.step) then
                        _addItOn false
                    else
                        arch |> Ok


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


    let makeBadUpdater (saveDetails:shcSaveDetails) =
        fun (arch:sorterShcArch list) 
            (newT:sorterShc) ->
            "bad updater" |> Error

    let sorterReport (s:sorterShcSpec) =
        s.sorter |> Sorter.makeId |> SorterId.value |> string

    let mutReport (s:sorterShcSpec) =
        s.mutatorSpec |> SorterMutSpec.colHdr

    let seedReport (s:sorterShcSpec) =
        s.rngGen.seed |> RandomSeed.value |> string

    let tempReport (s:sorterShcSpec) =
        s.annealerSpec |> AnnealerSpec.report



type sssrgType = 
    | Annealer of annealerSpec
    | Mutation of sorterMutSpec
    | RndGen
    | Sorters of sorterSetGen
    | StageWeight of sorterStageWeightSpec



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

        | StageWeight stw -> swapStageWeight 
                                    sssrg.rndGen 
                                    sssrg.baseSpec 
                                    sssrg.count 
                                    stw


type sorterShcResult =
    {
        id:ShcId;
        spec:sorterShcSpec;
        msg:string;
        archives: sorterShcArch[];
    }

type sorterShcResults =
    {
        members: array<sorterShcResult>
    }

