namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type WorldMergesFixture () =

    [<TestMethod>]
    member this.WorldGrandChild () =
        let worldMerges = WorldMerges.mergeWorlds 
                            TestData.WorldMerge.mergedWorldId 
                            TestData.WorldMerge.sourceWorldsNameMap 
                            TestData.WorldMerge.mergeMapItems
                            TestData.WorldMerge.enviroM
                                    |> Result.ExtractOrThrow

        let map = worldMerges.enviro |> Enviro.toMap |> Result.ExtractOrThrow

        Assert.AreEqual(worldMerges.id, TestData.WorldMerge.mergedWorldId);
        Assert.AreEqual(map.Count, 3)

    member this.addkvp (m:Map<string,string>) (kvp:string*string) =
            m.Add kvp

    member this.addkvp2 (mr:Result<Map<string,string>, string>) (kvp:string*string) =
            match mr with
            | Ok m -> m.Add kvp |> Ok
            | Error s -> s |> Error



    [<TestMethod>]
    member this.WorldGrandChild2 () =
        let kvps = [("ak", "av"); ("bk", "bv"); ("ck", "cv"); ("dk", "dv")]
        let mapq = [("0k", "0v"); ] |> Map.ofList
        let mapR = [("0k", "0v"); ] |> Map.ofList |> Ok
        //let mr = kvps |> List.fold(fun (s:Map<string,string>) (t:string*string) -> s.Add t) mapq
        //let mr = kvps |> List.fold(fun (s:Map<string,string>) (t:string*string) 
        //                                     -> this.addkvp s t) mapq

        let mr = kvps |> List.fold(fun (s:Result<Map<string,string>, string>) (t:string*string) 
                                            -> this.addkvp2 s t) mapR

        Assert.AreEqual(5, 5);
