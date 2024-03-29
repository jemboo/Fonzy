﻿namespace Fonzy.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open System
open System.Collections.Generic

[<TestClass>]
type UtilsFixture () =

    // SeqUtils
    [<TestMethod>]
    member this.join() =
        let seqFirst = seq {1;2;3}
        let seqSecond = seq {4;5;6}
        let expectedJoin = [1;2;3;4;5;6]
        let theJoin = seqFirst 
                          |> SeqUtils.join seqSecond 
                          |> Seq.toList
        Assert.AreEqual(expectedJoin, theJoin)

    // ByteUtils
    [<TestMethod>]
    member this.structHash() =
        let gA = ByteUtils.structHash(1 :> obj) |> Array.toList
        let gAd = ByteUtils.structHash(1 :> obj) |> Array.toList
        let gB = ByteUtils.structHash(2 :> obj) |> Array.toList
        Assert.AreEqual(gA, gAd)
        Assert.AreNotEqual(gA, gB)


    [<TestMethod>]
    member this.trueBitCount64() =
        let gA = BitSet.create [|1;0;1;1;0;1;0;1;1;0;1;0;1;1;0;1;0;1;1;0;1;0;1;1;0;1;0;1;1;0;1;0;1;1;0;1;0;1;1;0;1|]
        let gB = BitSet.toUint64 gA
        let tc = ByteUtils.trueBitCount64 gB
        Assert.AreEqual(tc, 25)


    [<TestMethod>]
    member this.StripeReadAndWrite() =
        let srcUla = [|128822345678987UL; 0UL; 475555577799876091UL|]
        let destUla = [|0UL; 0UL; 0UL|]
        let ivals = [|0..63|] |> Array.map (fun v ->  ByteUtils.stripeRead srcUla v)
        [0..63] |> List.map (fun v ->  ByteUtils.stripeWrite destUla ivals.[v] v)
            |> ignore
        for i = 0 to 2 do
            Assert.AreEqual(srcUla.[i], destUla.[i])




    // GuidUtils

    [<TestMethod>]
    member this.sandbox() =
        let g1 = true |> string
        let db = g1 |> bool.Parse
        let g2 = false |> string
        let dd = g2 |> bool.Parse
        Assert.IsTrue(true)

    [<TestMethod>]
    member this.addGuid() =
        let g1 = Guid.NewGuid()
        let g2 = Guid.NewGuid()
        let g3 = g1 |> GuidUtils.addGuids g2
        let hc = g3.GetHashCode()
        Assert.IsTrue(true)
        

    [<TestMethod>]
    member this.guidFromObjs() =
        let objsA = seq { 1:>obj; 2:> obj;}
        let objsB = seq { 1:>obj; 2:> obj;}
        let gA = objsA |> GuidUtils.guidFromObjs
        let gB = objsB |> GuidUtils.guidFromObjs
        Assert.AreEqual(gA, gB)



// CollectionUtils

    [<TestMethod>]
    member this.maxWindowed() =
        let ts = [1; 2; 3; 4; 5; 6]
        let maxWinSpan = 3
        let carty = ts |> CollectionUtils.maxWindowed maxWinSpan  |> Seq.toArray
        Assert.IsTrue(carty.Length > 0)

    [<TestMethod>]
    member this.listToTransitionTuples() =
        let ts = [1; 2; 1; 4;]
        let expected = [(1,2);(2,1);(1,4)]
        let res = CollectionUtils.listToTransitionTuples ts |> Seq.toList
        Assert.AreEqual (expected, res)


    [<TestMethod>]
    member this.iterateCircular() =
        let ts = [|1; 2; 3|]
        let reppy = CollectionUtils.arrayLoop 10 ts  |> Seq.toArray
        Assert.IsTrue(reppy.Length = 10)


    [<TestMethod>]
    member this.addDictionary() =
        let tBase = seq {("k1","v1"); ("k2","v2")} 
                        |> CollectionUtils.tuplesToDict
        let tAdd = seq {("k2","v2"); ("k3","v3"); ("k4","v4")}
                        |> CollectionUtils.tuplesToDict
        let newItems = CollectionUtils.addDictionary tBase tAdd

        Assert.IsTrue(tBase.Count = 4)
        Assert.IsTrue(newItems.Length = 2)


    [<TestMethod>]
    member this.chunkAndSum() =
        let wab = [1;2;3;4;5;6;7;8;9]
        let res = CollectionUtils.wrapAndSumCols 3 wab
                  |> Array.toList
        Assert.AreEqual(res, [12; 15; 18])


    [<TestMethod>]
    member this.merge2DArray() =
        let sorterLen = 10
        let genCount = 5
        let mergeCount = 3
        let gen0 = [|1 .. sorterLen|] 
        let genN (gnN:int) = 
            gen0 |> Array.map(fun v -> gnN*10 + v)
        let shc0 = Array.init genCount (fun dex -> genN dex)
        let mergeF = fun fm (im:int[]) -> fm |> Array.mapi(fun dex v -> v + (float im.[dex]))
        let merged = CollectionUtils.merge2DArray<int,float> 0.0 mergeF shc0
        Assert.AreEqual (merged.Length, sorterLen)
        



    [<TestMethod>]
    member this.merge3DintsToFloats() =
        let sorterLen = 10
        let genCount = 5
        let mergeCount = 3
        let gen0 = [|1 .. sorterLen|] 
        let genN (gnN:int) = 
            gen0 |> Array.map(fun v -> gnN*10 + v)
        let shc0 = Array.init genCount (fun dex -> genN dex)
        let repN (rpN:int) = 
            shc0 |> Array.map(fun v -> v |> Array.map((+) (100*rpN)))
        let shcS = Array.init mergeCount (fun dex -> repN dex)
        let merged = CollectionUtils.merge3DintsToFloats shcS
        //let yab = CollectionUtils.merge2DArray<int,float> 0.0 (fun a b -> (float a) + b) shc0
        Assert.AreEqual (3, 3)




    [<TestMethod>]
    member this.breakIntoSegments() =
        let testArray = [|1; 2; 3; 4; 5; 6; 7; 8; 9|] 
        let testBreaks = [|0; 2; 5; 9|] 
        let yak = CollectionUtils.breakArrayIntoSegments testArray testBreaks
        Assert.AreEqual (yak.Length, 3)
        


    // ResultMap

    [<TestMethod>]
    member this.addNewKey() =
        let kvps = seq { ("a", 1); ("b", 2); ("c", 3); ("d", 5) } |> Seq.toArray
        let ma = ResultMap.fromTuples kvps |> Result.ExtractOrThrow
        let mp = ma |> ResultMap.add "bb" 5
        let result = match mp with
                     | Ok m -> "success"
                     | Error m -> m
        Assert.IsTrue((result = "success"))

    [<TestMethod>]
    member this.addExistingKey() =
        let kvps = seq { ("a", 1); ("b", 2); ("c", 3); ("d", 5) } |> Seq.toArray
        let ma = ResultMap.fromTuples kvps |> Result.ExtractOrThrow
        let mp = ma |> ResultMap.add "b" 5
        let result = match mp with
                     | Ok m -> "success"
                     | Error m -> m
        Assert.IsTrue((result <> "success"))

    [<TestMethod>]
    member this.fromTuplesWithKeyDupes() =
        let kvps = seq { ("a", 1); ("b", 2); ("c", 3); ("b", 5) } |> Seq.toArray
        let ma = ResultMap.fromTuples kvps
        let result = match ma with
                     | Ok m -> "success"
                     | Error m -> m
        Assert.IsTrue((result <> "success"))


    [<TestMethod>]
    member this.fromTuplesWithoutKeyDupes() =
        let kvps = seq { ("a", 1); ("b", 2); ("c", 3); } |> Seq.toArray
        let ma = ResultMap.fromTuples kvps
        let result = match ma with
                        | Ok m -> "success"
                        | Error m -> m
        Assert.IsTrue((result = "success"))



    [<TestMethod>]
    member this.fromTransitionFormat() =
        let kvps = seq { (1, "a"); (6, "b"); (3, "c"); }
        let expected = ["q"; "a"; "a"; "c"; "c"; "c"; "b"; "b"; ]
        let dv = "q"
        let maxV = 8
        let res = SizeOpt.fromTransitionFormat kvps dv maxV
                  |> Array.toList
        Assert.AreEqual(expected, res)



// ReportUtils
        
    [<TestMethod>]
    member this.ReportUtils_padSeries() =
        let deOpt (yab:(string list)*(int*string option)[]) =
            let ddOpt (k:int*string option) = 
                match (snd k) with
                | Some s -> (fst k, s)
                | None -> (fst k, "")

            (fst yab, (snd yab) |> Array.map(ddOpt))

        let startingA = ("a", [|(1, "a_1"); (8, "a_8");|])
        let startingB = ("b", [|(0, "b_0"); (7, "b_7");|])
        let startingC = ("c", [|(0, "c_0"); (3, "c_3"); (5, "c_5");|])
        let startingD = ("d", [|(0, "d_0");|])
        
        let expectedA = ("a", [|(0, ""); (1, "a_1"); (2, "a_1"); (3, "a_1"); (4, "a_1"); (5, "a_1"); (6, "a_1"); (7, "a_1"); (8, "a_8");|])
        let expectedB = ("b", [|(0, "b_0"); (1, "b_0"); (2, "b_0"); (3, "b_0"); (4, "b_0"); (5, "b_0"); (6, "b_0"); (7, "b_7"); (8, "b_7");|])
        let expectedC = ("c", [|(0, "c_0"); (1, "c_0"); (2, "c_0"); (3, "c_3"); (4, "c_3"); (5, "c_5"); (6, "c_5"); (7, "c_5"); (8, "c_5");|])
        let expectedD = ("d", [|(0, "d_0"); (1, "d_0"); (2, "d_0"); (3, "d_0"); (4, "d_0"); (5, "d_0"); (6, "d_0"); (7, "d_0"); (8, "d_0");|])

        let hdTup = seq { startingA; startingB; startingC; startingD; }
        let hdExpected = [ expectedA; expectedB; expectedC; expectedD; ]
                         |> List.sortBy(fst)

        let dDexer = fun d -> d |> fst
        let dData = fun d -> (snd d) |> Some
        let specRep = fun d -> [d]
        let res = ReportUtils.completelyPadSeries<string, int*string> hdTup dDexer specRep
        let hdResult = res.Keys |> Seq.sort |> Seq.map(fun k -> res.[k]) |> Seq.toList


       // let hdResult = res.Keys |> Seq.sort |> Seq.map(fun k -> (k, res.[k])) |> Seq.toList
        //               |> List.map(deOpt)

        //let res0 = res |> ReportUtils.paddedSeriesToFatTable id
        
        //Assert.AreEqual(hdExpected.Length, hdResult.Length)
        Assert.AreEqual(1, 1)



    [<TestMethod>]
    member this.reportValueAt() =
        let max = 16.0
        let ticCt = 40.0
        let tics = Array.init (ticCt |> int) (fun dex -> (Math.Pow(2.0, ((dex |> float) * max) / ticCt)) |> int)
        let data = seq {0; 1; 3; 4; 10; 1222} |> Seq.toArray
        let res = ReportUtils.reportValuesAt 0 tics data (fun d t -> t >= d) |> Seq.toArray
        Assert.AreEqual(1, 1)