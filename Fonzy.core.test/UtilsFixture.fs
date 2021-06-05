namespace Fonzy.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open System
open System.Collections.Generic

[<TestClass>]
type UtilsFixture () =

    // ByteUtils
    
    [<TestMethod>]
    member this.structHash() =
        let gA = ByteUtils.structHash(1 :> obj) |> Array.toList
        let gAd = ByteUtils.structHash(1 :> obj) |> Array.toList
        let gB = ByteUtils.structHash(2 :> obj) |> Array.toList
        Assert.AreEqual(gA, gAd)
        Assert.AreNotEqual(gA, gB)


    [<TestMethod>]
    member this.trueBitCount32() =
        let gA = IntBits.create [|1;0;0;0;0;1;1;0;0;0;0;1;1;0;1;0;0;0;0;1|]
        let gB = IntBits.toUint32 gA
        let tc = ByteUtils.trueBitCount32 gB
        Assert.AreEqual(tc, 7)


    [<TestMethod>]
    member this.trueBitCount64() =
        let gA = IntBits.create [|1;0;1;1;0;1;0;1;1;0;1;0;1;1;0;1;0;1;1;0;1;0;1;1;0;1;0;1;1;0;1;0;1;1;0;1;0;1;1;0;1|]
        let gB = IntBits.toUint64 gA
        let tc = ByteUtils.trueBitCount64 gB
        Assert.AreEqual(tc, 25)


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
        let ts = [1; 2; 1; 4; 5; 6]
        let carty = CollectionUtils.listToTransitionTuples ts  |> Seq.toArray
        Assert.IsTrue(true)

    [<TestMethod>]
    member this.iterateCircular() =
        let ts = [|1; 2; 3|]
        let reppy = CollectionUtils.iterateCircular 10 ts  |> Seq.toArray
        Assert.IsTrue(reppy.Length = 10)


    [<TestMethod>]
    member this.addDictionary() =
        let tBase = seq {("k1","v1"); ("k2","v2")} 
                        |> CollectionUtils.dictFromSeqOfTuples
        let tAdd = seq {("k2","v2"); ("k3","v3"); ("k4","v4")}
                        |> CollectionUtils.dictFromSeqOfTuples
        let newItems = CollectionUtils.addDictionary tBase tAdd

        Assert.IsTrue(tBase.Count = 4)
        Assert.IsTrue(newItems.Length = 2)


    [<TestMethod>]
    member this.cumulate() =
        let cumer = new Dictionary<int, Dictionary<string, string>>()
        let l1 = CollectionUtils.cumulate cumer 1 "group1" "group1_1"
        let l2 = CollectionUtils.cumulate cumer 2 "group1" "group1_2"
        let l4 = CollectionUtils.cumulate cumer 1 "group2" "group2_1"
        let l5 = CollectionUtils.cumulate cumer 3 "group2" "group2_3"
        let l6 = CollectionUtils.cumulate cumer 1 "group3" "group3_4"
        let l7 = CollectionUtils.cumulate cumer 4 "group3" "group3_4"
        let ud = CollectionUtils.cumerBackFill cumer
        Assert.IsTrue(true)

    [<TestMethod>]
    member this.cumerBackFill() =
        let wab = [1;2;3;4;5]
        let res = CollectionUtils.listToTransitionTuples wab
        Assert.IsTrue(true)


    [<TestMethod>]
    member this.chunkAndSum() =
        let wab = [1;2;3;4;5;6;7;8;9]
        let res = CollectionUtils.chunkAndSum 3 wab
                  |> Array.toList
        Assert.AreEqual(res, [12; 15; 18])


        
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