namespace global
open System

// World
type WorldId = private WorldId of Guid
type WorldMergeId = private WorldMergeId of Guid
type CauseSpecId = private CauseSpecId of Guid

module WorldId =
    let value (WorldId v) = v
    let create id = Ok (WorldId id)
    let fromGuid (id:Guid) = create id |> Result.ExtractOrThrow

module WorldMergeId =
    let value (WorldMergeId v) = v
    let create id = Ok (WorldMergeId id)
    let fromGuid (id:Guid) = create id |> Result.ExtractOrThrow

module CauseSpecId =
    let value (CauseSpecId v) = v
    let create id = Ok (CauseSpecId id)
    let fromGuid (id:Guid) = create id |> Result.ExtractOrThrow
