namespace global
open System

// World
type WorldId = private WorldId of Guid

module WorldId =
    let value (WorldId v) = v
    let create id = Ok (WorldId id)
    let fromGuid (id:Guid) = create id |> Result.ExtractOrThrow
