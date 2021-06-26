namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SorterPartsFixture () =

    [<TestMethod>]
    member this.Stage_switchIntersection() =
        let degree = Degree.fromInt 16
        let randy = RngGen.createLcg 1234 |> Rando.fromRngGen
        let stageCount = StageCount.fromInt 2

        let startingStages() = 
            Stage.makeRandomReflSymmetricStages
                        degree
                        randy
                |> Seq.take (StageCount.value stageCount)
                |> Stage.switchIntersection
                |> List.length

        for i=0 to 100 do
            Console.WriteLine (sprintf "%d" (startingStages()))

        Assert.IsTrue(1 > 0);


    [<TestMethod>]
    member this.Stage_switchPairwiseIntersections() =
        let degree = Degree.fromInt 16
        let randy = RngGen.createLcg 1234 |> Rando.fromRngGen
        let stageCount = StageCount.fromInt 4

        let startingStages() = 
            Stage.makeRandomReflSymmetricStages
                        degree
                        randy
                |> Seq.take (StageCount.value stageCount)
                |> Stage.switchPairwiseIntersections
                |> Seq.length

        let res = Seq.init 1000 (fun _ -> startingStages())

        let hist = CollectionUtils.histogram (id) res
        Assert.IsTrue(hist.Count > 0);


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
        

    [<TestMethod>]
    member this.Stage_buddyStages3() =
        let degree = Degree.fromInt 10
        let randy = RngGen.createLcg 7234 |> Rando.fromRngGen
        let stageWindowSize = StageCount.fromInt 4
        let maxStageTry = (StageCount.fromInt 1200)
        let stageCount = (StageCount.fromInt 100)
        let buddyStages() = 
            Stage.makeSymmetricBuddyStages
                            stageWindowSize
                            SwitchFrequency.max
                            degree
                            randy
                            List.Empty 
                            maxStageTry
                            stageCount
        let aa = Array.init 100 (fun _ -> buddyStages())
        Assert.AreEqual(aa.Length, (StageCount.value stageCount));