namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type StageFixture () =

    [<TestMethod>]
    member this.Stage_switchIntersection() =
        let degree = Degree.fromInt 16
        let randy = RngGen.createLcg (RandomSeed.fromInt 1234) 
                        |> Rando.fromRngGen
        let stageCount = StageCount.fromInt 2

        let startingStages() = 
            Stage.rndSymmetric
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
        let randy = RngGen.createLcg (RandomSeed.fromInt 1234) |> Rando.fromRngGen
        let stageCount = StageCount.fromInt 4

        let startingStages() = 
            Stage.rndSymmetric
                        degree
                        randy
                |> Seq.take (StageCount.value stageCount)
                |> Stage.switchPairwiseIntersections
                |> Seq.length

        let res = Seq.init 1000 (fun _ -> startingStages())

        let hist = CollectionUtils.histogram (id) res
        Assert.IsTrue(hist.Count > 0);
        

    [<TestMethod>]
    member this.Stage_buddyStages3() =
        let degree = Degree.fromInt 10
        let randy = RngGen.createLcg (RandomSeed.fromInt 7234) |> Rando.fromRngGen
        let stageWindowSize = StageCount.fromInt 4
        let maxStageTry = (StageCount.fromInt 1200)
        let stageCount = (StageCount.fromInt 100)
        let buddyStages() = 
            Stage.rndSymmetricBuddyStages
                            stageWindowSize
                            SwitchFrequency.max
                            degree
                            randy
                            List.Empty 
                            maxStageTry
                            stageCount
        let aa = Array.init 100 (fun _ -> buddyStages())
        Assert.AreEqual(aa.Length, (StageCount.value stageCount));



    [<TestMethod>]
    member this.BuddyTrack_makeQualifier() =
        let degree = Degree.fromInt 16
        let buffSz = StageCount.fromInt 5
        let testDepth = StageCount.fromInt 3
        let bt = BuddyTrack.make degree buffSz
        bt |> BuddyTrack.prepNextStage |> ignore
        let quali = BuddyTrack.makeQualifier testDepth
        let res = bt.traces.[1] |> quali
        Assert.IsTrue(res)
        bt |> BuddyTrack.updateCb 1 true false |> ignore
        let res2 = bt.traces.[1] |> quali
        Assert.IsFalse(res2)


    [<TestMethod>]
    member this.BuddyTrack_makeNextStage() =
        let randy = RngGen.createLcg (RandomSeed.fromInt 234) 
                        |> Rando.fromRngGen
        let degree = Degree.fromInt 24
        let buffSz = StageCount.fromInt 4
        let testDepth = StageCount.fromInt 4
        let bt = BuddyTrack.make degree buffSz

        let newStage() =
            {
                Stage.degree = degree;
                switches = BuddyTrack.makeNextStage bt testDepth randy
                            |> Seq.toList
            }

        let stages = seq {0 .. 300} |> Seq.map(fun _ -> newStage())
                        |> Seq.toArray
        stages |> Seq.iter(fun t -> Console.WriteLine t.switches.Length)
        //let pairs = Stage.switchPairwiseIntersections stages
        //            |> Seq.toArray
        Assert.IsFalse(false)


