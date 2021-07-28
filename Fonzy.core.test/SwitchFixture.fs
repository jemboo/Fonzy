namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SwitchFixture () =
    [<TestMethod>]
    member this.testSwitchMap() =
        let yak = Switch.switchMap.[1]
        let yak2 = Switch.switchMap.[3]
        Assert.IsFalse(false)

    //[<TestMethod>]
    //member this.Stage_windowBuddies() =
    //    let degree = Degree.fromInt 16
    //    let randy = RngGen.createLcg 1234 |> Rando.fromRngGen
    //    let stageCount = StageCount.fromInt 10
    //    let windowSize = 4

    //    let startingStages = 
    //        Stage.makeRandomReflSymmetricStages
    //                    degree
    //                    randy
    //            |> Seq.take (StageCount.value stageCount)
    //            |> Seq.toArray

    //    let stageWindows = 
    //        startingStages 
    //            |> Stage.windowBuddies windowSize
    //            |> Seq.toArray

    //    Assert.IsTrue(stageWindows.Length > 0);


    //[<TestMethod>]
    //member this.Stage_buddyStages() =
    //    let degree = Degree.fromInt 16
    //    let randy = RngGen.createLcg 1234 |> Rando.fromRngGen
    //    let stageWindowSize = StageCount.fromInt 10
    //    let windowSize = 4

    //    let buddyStages = Stage.makeBuddyStages 
    //                        stageWindowSize
    //                        SwitchFrequency.max
    //                        degree
    //                        randy
    //                        List.Empty 
    //                     |> Seq.take 100
    //                     |> Seq.toArray

    //    let buddySwitches = 
    //        buddyStages 
    //            |> Stage.windowBuddies windowSize
    //            |> Seq.toArray
    //    let count = buddySwitches |> Array.sumBy(List.length)

    //    Assert.AreEqual(count, 0);


    //[<TestMethod>]
    //member this.Stage_buddyStages2() =
    //    let degree = Degree.fromInt 10
    //    let randy = RngGen.createLcg 1234 |> Rando.fromRngGen
    //    let stageWindowSize = StageCount.fromInt 4
    //    let maxStageTry = (StageCount.fromInt 10000)
    //    let sampleCount = 1000

    //    let (occCum, totCum) = 
    //                    Stage.makeReflBuddyStats
    //                        stageWindowSize
    //                        degree
    //                        randy
    //                        maxStageTry
    //                        sampleCount
                                 

    //    let occCumTegral = occCum |> Seq.scan (fun c v -> c + (v |> float) / (sampleCount |> float) ) 0.0
    //                              |> Seq.toArray

    //    totCum |> Array.iteri(fun i v -> Console.WriteLine (
    //                                            sprintf "%d\t%d\t%d\t%d"
    //                                                (Degree.value degree) 
    //                                                (StageCount.value stageWindowSize)
    //                                                i
    //                                                v))
    //    Assert.AreEqual(1, 1);