﻿namespace global
open System

// Ga
type EnviroId = private EnviroId of Guid
type EnviroUpdateParamsId = private EnviroUpdateParamsId of Guid
type GenerationNumber = private GenerationNumber of int
type InitialConditionCount = private InitialConditionCount of int
type OrgId = private OrgId of Guid
type OrgsId = private OrgsId of Guid
type OrgUpdateParamsId = private OrgUpdateParamsId of Guid
type OrgAttributeName = private OrgAttributeName of string
type PoolFraction = private PoolFraction of float
type PoolCount = private PoolCount of int
type PoolGenCount = private PoolGenCount of int
type PoolMemberRank = private PoolMemberRank of int
type ReplicaCount = private ReplicaCount of int
type RunCount = private RunCount of int


module EnviroUpdateParamsId =
    let value (EnviroUpdateParamsId v) = v
    let create id = Ok (EnviroUpdateParamsId id)
    let fromGuid (id:Guid) = create id |> Result.ExtractOrThrow

module EnviroId =
    let value (EnviroId v) = v
    let create id = Ok (EnviroId id)
    let fromGuid (id:Guid) = create id |> Result.ExtractOrThrow




module InitialConditionCount =
    let value (InitialConditionCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName InitialConditionCount 1 100000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>int)
        }

module GenerationNumber =
    let value (GenerationNumber v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName GenerationNumber 0 100000000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let increment gen = fromInt ((value gen) + 1)
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>int)
        }

module OrgAttributeName =
    let value (OrgAttributeName str) = str
    let create fieldName str = 
        ConstrainedType.createString fieldName OrgAttributeName 20 str
    let createOption fieldName str = 
        ConstrainedType.createStringOption fieldName OrgAttributeName 20 str

module OrgUpdateParamsId =
    let value (OrgUpdateParamsId v) = v
    let create id = Ok (OrgUpdateParamsId id)
    let fromGuid (id:Guid) = create id |> Result.ExtractOrThrow

module OrgId =
    let value (OrgId v) = v
    let create id = Ok (OrgId id)
    let fromGuid (id:Guid) = create id |> Result.ExtractOrThrow

module OrgsId =
    let value (OrgsId v) = v
    let create id = Ok (OrgsId id)
    let fromGuid (id:Guid) = create id |> Result.ExtractOrThrow

module PoolFraction =
    let value (PoolFraction v) = v
    let create fieldName v = 
        ConstrainedType.createFloat fieldName PoolFraction 0.0 1.0 v
    let boundedMultiply pf rhs =
        Math.Max((int ((float rhs) * (value pf))), 1)
    let fromFloat v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>float)
        }

module PoolCount =
    let value (PoolCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName PoolCount 1 10000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>int)
        }

module PoolGenCount =
    let value (PoolGenCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName PoolGenCount 1 1000000000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>int)
        }

module PoolMemberRank =
    let value (PoolMemberRank v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName PoolMemberRank 1 100000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let repStr v = match v with
                    |Some r -> sprintf "%d" (value r)
                    |None -> ""
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>int)
        }

module ReplicaCount =
    let value (ReplicaCount count) = count
    let create fieldName (count:int) =
        ConstrainedType.createInt fieldName ReplicaCount 1 10000 count
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>int)
        }

module RunCount =
    let value (RunCount v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName RunCount 1 1000000 v
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>int)
        }



