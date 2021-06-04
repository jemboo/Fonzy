namespace global
open System
// Math
type Degree = private Degree of int
module Degree =
    let value (Degree v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName Degree 1 1000 v
    let within (b:Degree) v =
        (v >= 0) && (v < (value b))
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>int)
        }
    let binExp (v: Degree) =
        let fexp = (value v) |> float
        ( ** ) 2.0 fexp |> int


type MutationRate = private MutationRate of float
module MutationRate =
    let value (MutationRate v) = v
    let create fieldName v =
        ConstrainedType.createFloat fieldName MutationRate 0.0 1.0 v
    let fromFloat v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>float)
        }


// Common
type EntityId = private EntityId of Guid
type JsonString = private JsonString of string
type ReportingFrequency = private ReportingFrequency of int
type String50 = private String50 of string
type UseEagerProc = private UseEagerProc of bool
type UseParallel = private UseParallel of bool

module EntityId =
    let value (EntityId v) = v
    let create id = Ok (EntityId id)
    let fromGuid (id:Guid) = create id |> Result.ExtractOrThrow

module JsonString =
    let value (JsonString str) = str
    let create fieldName str = 
        ConstrainedType.createString fieldName JsonString 100000000 str
    let createOption fieldName str = 
        ConstrainedType.createStringOption fieldName JsonString 50 str

module ReportingFrequency =
    let value (ReportingFrequency freq) = freq
    let create fieldName (freq:int) =
        ConstrainedType.createInt fieldName ReportingFrequency 1 10000 freq
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>int)
        }

module String50 =
    let value (String50 str) = str
    let create fieldName str = 
        ConstrainedType.createString fieldName String50 50 str
    let createOption fieldName str = 
        ConstrainedType.createStringOption fieldName String50 50 str

module UseEagerProc = 
    let create (useEagerProc:bool) =
        UseEagerProc useEagerProc
    let value (UseEagerProc v) = v
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return create (gv:?>bool)
        }

module UseParallel = 
    let create (useParallel:bool) =
        UseParallel useParallel
    let value (UseParallel v) = v
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return create (gv:?>bool)
        }
