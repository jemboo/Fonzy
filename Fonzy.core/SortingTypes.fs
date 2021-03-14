namespace global
open System



// Sorter

type SortableCount = private SortableCount of int
type SortableSetId = private SortableSetId of Guid
type SorterCount = private SorterCount of int
type SorterSetId = private SorterSetId of Guid
type StageCount = private StageCount of int
type SwitchCount = private SwitchCount of int
type SwitchFrequency = private SwitchFrequency of float
type SwitchOrStage = | Switch | Stage
type SorterLength = | Switch of SwitchCount
                    | Stage of StageCount

module SortableCount =
    let value (SortableCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName SortableCount 0 100000000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let repStr v = match v with
                          | Some r -> sprintf "%d" (value r)
                          | None -> ""
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>int)
        }


module SorterSetId =
    let value (SorterSetId v) = v
    let create id = Ok (SorterSetId id)
    let fromGuid (id:Guid) = create id |> Result.ExtractOrThrow


module SorterCount =
    let value (SorterCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName SorterCount 1 100000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>int)
        }

module SortableSetId =
    let value (SortableSetId v) = v
    let create id = Ok (SortableSetId id)
    let fromGuid (id:Guid) = create id |> Result.ExtractOrThrow


module SwitchCount =
    let value (SwitchCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName SwitchCount 0 10000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>int)
        }

module SwitchFrequency =
    let value (SwitchFrequency v) = v
    let create fieldName v = 
        ConstrainedType.createFloat fieldName SwitchFrequency 0.0 1.0 v
    let fromFloat v = create "" v |> Result.ExtractOrThrow
    let max = fromFloat 1.0
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv :?> float)
        }

module StageCount =
    let value (StageCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName StageCount 0 1000 v
    let ToSwitchCount (degree:Degree) (stageCount:StageCount) =
        SwitchCount.create "" ((Degree.value degree) * (value stageCount) / 2)
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>int)
        }

module SorterLength =
    let makeSwitchCountR switchCount =
        result {
            let! wc = (SwitchCount.create "" switchCount)
            return SorterLength.Switch wc
        }

    let makeStageCountR stageCount =
        result {
            let! tc = (StageCount.create "" stageCount)
            return SorterLength.Stage  tc
        }

    let makeStageCount stageCount =
        SorterLength.Stage (StageCount.fromInt stageCount)

    let makeSwitchCount switchCount =
        SorterLength.Switch (SwitchCount.fromInt switchCount)

    let Add (lhs:SorterLength) (rhs:SorterLength) =
            match lhs with
                | SorterLength.Switch (SwitchCount wCtL) -> 
                        match rhs with
                        | SorterLength.Switch (SwitchCount wCtR) -> 
                                makeSwitchCount (wCtL + wCtR) |> Result.Ok
                        | SorterLength.Stage _ -> Error "cant add SwitchCount and StageCount"
                | SorterLength.Stage (StageCount tCtL) -> 
                        match rhs with
                        | SorterLength.Switch _ -> Error "cant add SwitchCount and StageCount"
                        | SorterLength.Stage (StageCount tCtR) -> 
                                makeStageCount (tCtL + tCtR) |> Result.Ok

    let Multiply (rhs:float) (lhs:SorterLength) =
            match lhs with
                | SorterLength.Switch (SwitchCount wCtL) -> 
                      ((wCtL |> float) * rhs) |> int |> SwitchCount.fromInt |> SorterLength.Switch
                | SorterLength.Stage (StageCount tCtL) -> 
                      ((tCtL |> float) * rhs) |> int |>  StageCount.fromInt |> SorterLength.Stage


    let degreeToRecordSwitchCount (degree:Degree) =
        let d = (Degree.value degree)
        let ct = match d with
                    | 4 -> 5    | 5 -> 9    | 6 -> 12
                    | 7 -> 16   | 8 -> 19   | 9 -> 25
                    | 10 -> 29  | 11 -> 35  | 12 -> 39
                    | 13 -> 45  | 14 -> 51  | 15 -> 56
                    | 16 -> 60  | 17 -> 71  | 18 -> 77
                    | 19 -> 85  | 20 -> 91  | 21 -> 100
                    | 22 -> 107 | 23 -> 115 | 24 -> 120
                    | 25 -> 132 | 26 -> 139 | 27 -> 150
                    | 28 -> 155 | 29 -> 165 | 30 -> 172
                    | 31 -> 180 | 32 -> 185 | _ -> 0
        let wc = SwitchCount.create "" ct |> Result.ExtractOrThrow
        SorterLength.Switch wc

    let degreeTo999SwitchCount (degree:Degree) =
        let d = (Degree.value degree)
        let ct = match d with
                    | 6  | 7 -> 600    | 8  | 9 -> 700
                    | 10 | 11 -> 800   | 12 | 13 -> 1000
                    | 14 | 15 -> 1200  | 16 | 17 -> 1600
                    | 18 | 19 -> 2000  | 20 | 21 -> 2200
                    | 22 | 23 -> 2600  | 24 | 25 -> 3000
                    | _ -> 0
        let wc = SwitchCount.create "" ct |> Result.ExtractOrThrow
        SorterLength.Switch wc

    let degreeToRecordStageCount (degree:Degree) =
        let d = (Degree.value degree)
        let ct = match d with
                    | 4 ->  3
                    | 5 | 6 ->  5
                    | 7 | 8 ->  6
                    | 9 | 10 -> 7
                    | 11 | 12 -> 8
                    | 13 | 14 | 15 | 16 -> 9
                    | 17 -> 10
                    | 18 | 19 | 20 -> 11
                    | 21 | 22 | 23 | 24 -> 12
                    | 25 | 26 -> 13
                    | 27 | 28 | 29 | 30 | 31 | 32 -> 14
                    | _ -> 0
        let tc = StageCount.create "" ct |> Result.ExtractOrThrow
        SorterLength.Stage tc

    let degreeTo999StageCount (degree:Degree) =
        let d = (Degree.value degree)
        let ct = match d with
                    | 8 | 9 -> 140
                    | 10 | 11 | 12 | 13 | 14 | 15 -> 160
                    | 16 | 17 | 18 | 19 | 20 | 21 -> 200
                    | 22 | 23 | 24 | 25 -> 220
                    | _ -> 0
        let tc = StageCount.create "" ct |> Result.ExtractOrThrow
        SorterLength.Stage tc

    let to999Sucessful (degree:Degree) (wOrT:SwitchOrStage) =
        match wOrT with
        | SwitchOrStage.Switch -> (degreeTo999SwitchCount degree)
        | SwitchOrStage.Stage -> (degreeTo999StageCount degree)

    let toRecordSorterLength (degree:Degree) (wOrT:SwitchOrStage) =
        match wOrT with
        | SwitchOrStage.Switch -> (degreeToRecordSwitchCount degree)
        | SwitchOrStage.Stage -> (degreeToRecordStageCount degree)

    let toRecordSorterLengthPlus(degree:Degree) (extraLength:SorterLength) =
        match extraLength with
        | SorterLength.Switch wCt -> (toRecordSorterLength degree SwitchOrStage.Switch) 
                                        |> Add extraLength |> Result.ExtractOrThrow
        | SorterLength.Stage wCt -> (toRecordSorterLength degree SwitchOrStage.Stage) 
                                        |> Add extraLength |> Result.ExtractOrThrow

    let toMediocreRandomPerfLength (wOrT:SwitchOrStage) (degree:Degree) =
        match wOrT with
        | SwitchOrStage.Switch -> (toRecordSorterLength degree SwitchOrStage.Switch) 
                                            |> Multiply 5.0 
        | SwitchOrStage.Stage -> (toRecordSorterLength degree SwitchOrStage.Stage) 
                                            |> Multiply 5.0 