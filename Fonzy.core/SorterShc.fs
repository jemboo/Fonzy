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
    | ForSteps of StepNumber array


type shcTermSpec = 
    | FixedLength of StepNumber
    | EnergyBased of Energy*StepNumber*StepNumber

module ShcTermSpec =
    let getSteps (termSpec:shcTermSpec) = 
        match termSpec with
        | shcTermSpec.EnergyBased (e,sa,sb) -> sb
        | shcTermSpec.FixedLength st -> st

type sorterStageWeightSpec =
    | Constant of StageWeight


type sorterMutSpec =
    | Constant of sorterMutType


module SorterMutSpec =
    let colHdr (ms:sorterMutSpec) =
        match ms with
        | Constant smt -> smt |> SorterMutType.colHdr 


type sorterEvalSpec =
    | PerfBin


type sorterShc = 
    {
        step:StepNumber;
        isNew:bool;
        revision:RevNumber;
        rngGen:RngGen; 
        sorter:sorter;
        switchUses:switchUses option
        perf:SortingEval.sorterPerf option
        energy:Energy option;
        energyDelta:Energy option;
        bestEnergy:Energy option;
    }

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


type sorterShcArch = 
    {
        step:StepNumber;
        revision:RevNumber;
        rngGen:RngGen; 
        sorter:sorter;
        switchUses:switchUses
        perf:SortingEval.sorterPerf
        energy:Energy;
    }


module SorterShcArch = 
    
    let dflt = 
        {
            sorterShcArch.step = StepNumber.fromInt 0;
            sorterShcArch.revision = RevNumber.fromInt 0;
            sorterShcArch.rngGen = RngGen.createLcg (RandomSeed.fromInt 1);
            sorterShcArch.sorter = Sorter.fromSwitches (Degree.fromInt 2) [];
            sorterShcArch.switchUses = SwitchUses.createEmpty (SwitchCount.fromInt 0);
            sorterShcArch.perf = SortingEval.SorterPerf.dflt;
            sorterShcArch.energy = Energy.fromFloat Double.MaxValue;
        }

    let toFull (shc:sorterShc) =
        {
            sorterShcArch.step = shc.step;
            sorterShcArch.revision = shc.revision;
            sorterShcArch.rngGen = shc.rngGen;
            sorterShcArch.sorter = shc.sorter;
            sorterShcArch.switchUses = shc.switchUses |> Option.get;
            sorterShcArch.perf = shc.perf |> Option.get;
            sorterShcArch.energy= shc.energy |> Option.get;
        }

    //let toPartial (shc:sorterShc) =
    //    {
    //        sorterShcArch.step = shc.step;
    //        sorterShcArch.revision = shc.revision;
    //        sorterShcArch.rngGen = None;
    //        sorterShcArch.sorter = None;
    //        sorterShcArch.switchUses = None;
    //        sorterShcArch.perf = shc.perf |> Option.get;
    //        sorterShcArch.energy= shc.energy |> Option.get;
    //    }

    //let toSorterShcArch (doFull:bool) 
    //                    (shc:sorterShc) =
    //    if doFull then 
    //        toFull shc
    //        else
    //        toPartial shc


module ShcStageWeightSpec =
    let constantWeight wgt =
        fun (shc:sorterShc) ->
            wgt

    let getStageWeight (spec:sorterStageWeightSpec) =
        match spec with
        | sorterStageWeightSpec.Constant tw -> constantWeight tw


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

    
    let isCurrentBest (shc:sorterShc) =
        match shc.energy, shc.bestEnergy  with
        | Some e, Some be -> if Energy.value be >= Energy.value e then
                                true
                             else
                                false
                        // current energy is better than best (until now)
        | None, Some _ -> true
        | _, None -> failwith "sorterShc bestEnergy missing"

    let logPolicy0 (shc:sorterShc) =
        match shc.energyDelta with
        | Some (Energy e) -> (shc |> isCurrentBest) && (shc.isNew) && (e < 0)
        | None -> true

//let logPolicy0 (shc:sorterShc) =
//    if shc.step |> StepNumber.value = 0 then
//        true
//    else
//        match shc.energyDelta with
//        | Some (Energy e) -> (shc |> isCurrentBest) && (shc.isNew) && (e < 0)
//        | None -> true

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
            //if ((StepNumber.value shcCurrent.step) = 0) then
            //    shcNew |> Ok
            //else
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
                        return { shcNew with
                                   bestEnergy = Energy.betterEnergy shcNew.energy shcCurrent.bestEnergy
                                   energyDelta  = Energy.delta shcCurrent.energy shcNew.energy
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
                    revision = shcCurrent.revision |> RevNumber.increment;
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
                    arch |> Ok

            //if arch.Length = 0 then
            //    _addItOn true
            //else
            match saveDetails with
                | Always -> _addItOn true
                | IfBest ->  
                    _addItOn (newShc |> SorterShc.isCurrentBest)
                | BetterThanLast ->  
                    _addItOn (_threshB (arch |> List.head).energy)
                | EnergyThresh e -> 
                    _addItOn (_threshB e)
                | ForSteps stps -> 
                    if (newShc |> SorterShc.logPolicy0) then
                        _addItOn true
                    else
                        _addItOn (stps |> Array.contains newShc.step)


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

