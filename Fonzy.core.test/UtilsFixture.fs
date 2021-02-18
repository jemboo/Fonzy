﻿namespace Fonzy.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open System
open System.Collections.Generic

[<TestClass>]
type UtilsFixture () =

    [<TestMethod>]
    member this.iterateCircular() =
        let ts = [|1; 2; 3|]
        let reppy = CollectionUtils.IterateCircular 10 ts  |> Seq.toArray
        Assert.IsTrue(reppy.Length = 10)


    [<TestMethod>]
    member this.addGuid() =
        let g1 = Guid.NewGuid()
        let g2 = Guid.NewGuid()
        let g3 = g1 |> GuidUtils.addGuids g2
        let hc = g3.GetHashCode()
        Assert.IsTrue(true)


    [<TestMethod>]
    member this.guidFromObjs() =
        let objs = seq { Guid.NewGuid():>obj; Guid.NewGuid() :> obj;}
        let g2 = objs |> GuidUtils.guidFromObjs
        Assert.IsTrue(true)


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
        let res = CollectionUtils.listToTuples wab

        Assert.IsTrue(true)

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