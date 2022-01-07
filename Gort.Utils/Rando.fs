namespace global
open System

// Rando
type RandomSeed = private RandomSeed of int
type rngType = | Lcg | Net
type rngGen = {rngType:rngType; seed:RandomSeed}

type IRando =
    abstract member Count: int
    abstract member Seed : RandomSeed
    abstract member NextUInt : uint32
    abstract member NextPositiveInt: int32
    abstract member NextULong : uint64
    abstract member NextFloat : float
    abstract member rngType : rngType

module RandomSeed =
    let value (RandomSeed seed) = seed
    let create (seed:int) =
        (Math.Abs(seed) % 2147483647) |> RandomSeed
    let fromNow () = 
        DateTime.Now.Ticks |> int |> create


module RngGen =
    let createLcg (seed:RandomSeed) =
        {rngType=rngType.Lcg; seed=seed}
    let lcgFromNow() =
        RandomSeed.fromNow() |> createLcg
    let createNet (seed:RandomSeed) =
        {rngType=rngType.Net; seed=seed}


type RandomNet(seed:RandomSeed) =
    let mutable _count = 0
    let rnd = new System.Random(RandomSeed.value seed)

    interface IRando with
        member this.Seed = seed
        member this.Count = _count
        member this.NextUInt =
            _count <- _count + 2
            let vv = (uint32 (rnd.Next()))
            vv + (uint32 (rnd.Next()))
        member this.NextPositiveInt =
            rnd.Next()
        member this.NextULong =
            let r = this :> IRando
            let vv = (uint64 r.NextUInt)
            (vv <<< 32) + (uint64 r.NextUInt)

        member this.NextFloat = 
            _count <- _count + 1
            rnd.NextDouble()

        member this.rngType = rngType.Net


type RandomLcg(seed:RandomSeed) =
    let _a=6364136223846793005UL
    let _c=1442695040888963407UL
    let mutable _last = (_a * (uint64 (RandomSeed.value seed)) + _c)
    let mutable _count = 0
    member this.Seed = seed
    member this.Count = _count
    member this.NextUInt =
        _count <- _count + 1
        _last <- ((_a * _last) + _c)
        (uint32 (_last >>> 32))

    member this.NextULong =
        let mm = ((_a * _last) + _c)
        _last <- ((_a * mm) + _c)
        _count <- _count + 2
        _last + (mm >>> 32)

    member this.NextFloat = 
        (float this.NextUInt) / (float Microsoft.FSharp.Core.uint32.MaxValue)

    interface IRando with
        member this.Seed = this.Seed
        member this.Count = _count
        member this.NextUInt =
            this.NextUInt
        member this.NextPositiveInt =
            int (this.NextUInt >>> 1)
        member this.NextULong =
            this.NextULong
        member this.NextFloat = 
            this.NextFloat
        member this.rngType = rngType.Lcg

     
module Rando =
    
    let create rngtype seed =
        match rngtype with
        | rngType.Lcg -> new RandomLcg(seed) :> IRando
        | rngType.Net -> new RandomNet(seed) :> IRando


    let fromRngGen (rg:rngGen) =
        create rg.rngType rg.seed


    let nextRngGen (randy:IRando) =
        create randy.rngType (RandomSeed.create randy.NextPositiveInt)


    let multiDraw (rnd:IRando) (freq:float) (numDraws:int)  =
        let draw (randy:IRando) =
            if randy.NextFloat < freq then 1 else 0
        let mutable i=0
        let mutable successCount = 0
        while (i < numDraws) do
                successCount <- successCount + draw rnd
                i <- i + 1
        successCount


    let normalDistRandomPair meanX stdDevX meanY stdDevY (rnd:IRando) = 
        let rec getRands () =
            let u = (2.0 * rnd.NextFloat) - 1.0
            let v = (2.0 * rnd.NextFloat) - 1.0
            let w = u * u + v * v
            if w >= 1.0 then
                getRands()
            else
                u, v, w
        let u, v, w = getRands()
                
        let scale = System.Math.Sqrt(-2.0 * System.Math.Log(w) / w)
        let x = scale * u
        let y = scale * v
        (meanX + x * stdDevX, meanY + y * stdDevY)
 
    let normalDistInt2d meanX stdDevX meanY stdDevY (rnd:IRando) = 
        let fpr = normalDistRandomPair meanX stdDevX meanY stdDevY rnd
        (int (fst fpr), int (snd fpr))

    let normalDistInt3d meanX stdDevX meanY stdDevY stdDevZ (rnd:IRando) = 
        let fpr = normalDistRandomPair meanX stdDevX meanY stdDevY rnd
        let spr = normalDistRandomPair meanX stdDevZ meanY 0.0 rnd
        (int (fst fpr), int (snd fpr), int (fst spr))

    let normalDistRandomSeq mean stdDev (rnd:IRando) = 
        let rec polarBoxMullerDist () = seq {
                let rec getRands () =
                    let u = (2.0 * rnd.NextFloat) - 1.0
                    let v = (2.0 * rnd.NextFloat) - 1.0
                    let w = u * u + v * v
                    if w >= 1.0 then
                        getRands()
                    else
                        u, v, w
                let u, v, w = getRands()
                    
                let scale = System.Math.Sqrt(-2.0 * System.Math.Log(w) / w)
                let x = scale * u
                let y = scale * v
                yield mean + (x * stdDev); yield mean + (y * stdDev); yield! polarBoxMullerDist()
            }
        polarBoxMullerDist ()

    let choose (rando:IRando) (items:'T[]) =
        if(items.Length > 0) then
            Some items.[rando.NextPositiveInt % items.Length]
        else
            None

