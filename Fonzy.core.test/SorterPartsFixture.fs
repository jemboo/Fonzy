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
            Stage.makeRandomFullStages
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
            Stage.makeRandomFullStages
                        degree
                        randy
                |> Seq.take (StageCount.value stageCount)
                |> Stage.switchPairwiseIntersections
                |> Seq.length

        let res = Seq.init 1000 (fun _ -> startingStages())

        let hist = CollectionUtils.histogram (id) res
        Assert.IsTrue(hist.Count > 0);


    [<TestMethod>]
    member this.Stage_windowBuddies() =
        let degree = Degree.fromInt 16
        let randy = RngGen.createLcg 1234 |> Rando.fromRngGen
        let stageCount = StageCount.fromInt 10
        let windowSize = 4

        let startingStages = 
            Stage.makeRandomFullStages
                        degree
                        randy
                |> Seq.take (StageCount.value stageCount)
                |> Seq.toArray

        let stageWindows = 
            startingStages 
                |> Stage.windowBuddies windowSize
                |> Seq.toArray

        Assert.IsTrue(stageWindows.Length > 0);


    [<TestMethod>]
    member this.Stage_buddyStages() =
        let degree = Degree.fromInt 16
        let randy = RngGen.createLcg 1234 |> Rando.fromRngGen
        let stageWindowSize = StageCount.fromInt 10
        let windowSize = 4

        let buddyStages = Stage.buddyStages 
                            List.Empty 
                            stageWindowSize
                            degree
                            randy
                         |> Seq.take 100
                         |> Seq.toArray

        let buddySwitches = 
            buddyStages 
                |> Stage.windowBuddies windowSize
                |> Seq.toArray
        let count = buddySwitches |> Array.sumBy(List.length)

        Assert.AreEqual(count, 0);