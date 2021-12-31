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
    let getMaxSteps (termSpec:shcTermSpec) = 
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
        advanceCount:int;
        retreatCount:int;
        rngGen:RngGen; 
        sorter:sorter;
        switchUses:switchUses option
        perf:SortingEval.sorterPerf option
        energy:Energy option;
        energyDelta:Energy option;
        bestEnergy:Energy option;
        lastSwitchUsed:SwitchCount
    }


type sorterShcArchFull = 
    {
        step:StepNumber;
        advanceCount:int;
        retreatCount:int;
        rngGen:RngGen; 
        sorter:sorter;
        switchUses:switchUses
        perf:SortingEval.sorterPerf
        energy:Energy;
    }

type sorterShcArchPartial = 
    {
        step:StepNumber;
        advanceCount:int;
        retreatCount:int;
        perf:SortingEval.sorterPerf
        energy:Energy;
    }

type sorterShcArch = 
    | Full of sorterShcArchFull
    | Partial of sorterShcArchPartial


module SorterShcArch = 
    
    let dfltPartial = 
        {
            sorterShcArchPartial.step = StepNumber.fromInt 0;
            advanceCount = 0;
            retreatCount = 0;
            perf = SortingEval.SorterPerf.dflt;
            energy = Energy.fromFloat Double.MaxValue;
        } |> sorterShcArch.Partial

    let toFull (shc:sorterShc) =
        {
            sorterShcArchFull.step = shc.step;
            advanceCount = shc.advanceCount;
            retreatCount = shc.retreatCount;
            rngGen = shc.rngGen;
            sorter = shc.sorter;
            switchUses = shc.switchUses |> Option.get;
            perf = shc.perf |> Option.get;
            energy= shc.energy |> Option.get;
        } |> sorterShcArch.Full

    let toPartial (shc:sorterShc) =
        {
            sorterShcArchPartial.step = shc.step;
            advanceCount = shc.advanceCount;
            retreatCount = shc.retreatCount;
            perf = shc.perf |> Option.get;
            energy= shc.energy |> Option.get;
        }  |> sorterShcArch.Partial

    let getEnergy (arch:sorterShcArch) = 
        match arch with
        | Full fa -> fa.energy
        | Partial pa -> pa.energy

    let getStep (arch:sorterShcArch) = 
        match arch with
        | Full fa -> fa.step
        | Partial pa -> pa.step

    let getAdvanceCount (arch:sorterShcArch) = 
        match arch with
        | Full fa -> fa.advanceCount
        | Partial pa -> pa.advanceCount

    let getRetreatCount (arch:sorterShcArch) = 
        match arch with
        | Full fa -> fa.retreatCount
        | Partial pa -> pa.retreatCount

    let getPerf (arch:sorterShcArch) = 
        match arch with
        | Full fa -> fa.perf
        | Partial pa -> pa.perf




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

    let successFul (perf:SortingEval.sorterPerf) = 
        match perf.successful with
        | Some r -> r |> Ok
        | None -> Error "successful missing"

    let getEnergy (shc:sorterShc) = 
        result {
            let! perf = getPerf shc
            let! sFul = successFul perf
            if sFul then
                return!
                    match shc.energy with
                    | Some r -> r |> Ok
                    | None -> Error "Energy missing"
            else
                return Energy.failure
        }

    
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

