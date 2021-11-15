namespace global
open System


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

        member this.RngType = RngType.Net


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
        member this.RngType = RngType.Lcg

     
module Rando =
    
    let NetFromSeed seed =
        new RandomNet(seed) :> IRando


    let LcgFromSeed seed =
        new RandomLcg(seed) :> IRando


    let private _NextGuid (curr:IRando) =
            GuidUtils.makeGuid (curr.NextULong) (curr.NextULong) 
                                (curr.NextULong) (curr.NextULong)


    let private _NextGuid2 (curr1:IRando) 
                           (curr2:IRando) =
            GuidUtils.makeGuid (curr1.NextULong) (curr1.NextULong) 
                               (curr2.NextULong) (curr2.NextULong)


    let NextGuid (curr1:IRando) 
                 (curr2:IRando option) =
        match curr2 with
        | Some rando -> _NextGuid2 curr1 rando
        | None -> _NextGuid curr1

    let fromSeed rngtype seed =
        match rngtype with
        | RngType.Lcg -> LcgFromSeed seed
        | RngType.Net -> NetFromSeed seed


    let fromGuid rngType (gu:Guid) =
        fromSeed rngType (gu.GetHashCode() |> RandomSeed.fromInt)


    let getSeed = 
        (RandomSeed.create "" (int System.DateTime.Now.Ticks))


    let fromRngGen (rngGen:RngGen) =
        match rngGen.rngType with
        | RngType.Lcg -> LcgFromSeed rngGen.seed
        | RngType.Net -> NetFromSeed rngGen.seed


    let nextRngGen (randy:IRando) =
        match (randy.RngType) with
        | RngType.Lcg -> { RngGen.rngType = RngType.Lcg; 
                           seed = (RandomSeed.fromInt randy.NextPositiveInt) }
        | RngType.Net -> { RngGen.rngType = RngType.Net; 
                           seed = (RandomSeed.fromInt randy.NextPositiveInt) }


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
