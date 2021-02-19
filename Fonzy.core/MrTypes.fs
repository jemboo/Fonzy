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


// Rando
type RandomSeed = private RandomSeed of int
type RngType = | Lcg | Net
type RngGen = {rngType:RngType; seed:RandomSeed}
type IRando =
    abstract member Count: int
    abstract member Seed : RandomSeed
    abstract member NextUInt : uint32
    abstract member NextPositiveInt: int32
    abstract member NextULong : uint64
    abstract member NextFloat : float
    abstract member RngType : RngType

module RandomSeed =
    let value (RandomSeed seed) = seed
    let create fieldName (seed:int) =
        let mSeed = Math.Abs(seed) % 2147483647
        ConstrainedType.createInt fieldName RandomSeed 1 2147483647 mSeed
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m 
            return! create "" (gv:?>int)
        }

module RngGen =
    let createLcg (seed:int) =
        let rnd = (RandomSeed.create "" seed) |> Result.ExtractOrThrow
        {rngType=RngType.Lcg; seed=rnd}

    let createNet (seed:int) =
        let rnd = (RandomSeed.create "" seed) |> Result.ExtractOrThrow
        {rngType=RngType.Net; seed=rnd}

module RngType =
    let toDto (rngt: RngType) =
        match rngt with
        | Lcg -> "Lcg"
        | Net -> "Net"
    let create str =
        match str with
        | "Lcg" -> RngType.Lcg |> Ok
        | "Net" -> RngType.Net |> Ok
        | _ -> Error (sprintf "no match for RngType: %s" str)

