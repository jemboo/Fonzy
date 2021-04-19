namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SorterPartsFixture () =

    [<TestMethod>]
    member this.SwitchMapTracker_recordSwitches () =
        let degree = Degree.fromInt 16
        let maxSwitchDex = int (Switch.switchCountForDegree degree) - 1
        let switchIndexes = [0; 3; 2; maxSwitchDex; 3; 22;]
        let switches = switchIndexes |> List.map(fun d -> Switch.switchMap.[d])
        let switchMapTracker = SwitchMapTracker.create degree
        let trackWeight = 3
        let res = SwitchMapTracker.recordSwitches 
                            trackWeight 
                            switchMapTracker 
                            switches
        let mapIndexes = res |> SwitchMapTracker.usedIndexes
        let canSwDxes = switchIndexes |> CollectionUtils.sortedUnique
                                      |> Seq.toList
        Assert.AreEqual(mapIndexes, canSwDxes);


    [<TestMethod>]
    member this.SwitchMapTracker_recordAndFilter () =
        let degree = Degree.fromInt 16
        let maxSwitchDex = int (Switch.switchCountForDegree degree) - 1
        let randy = RngGen.createLcg 1234 |> Rando.fromRngGen
        let switchGen = Switch.randomSwitchesOfDegree degree randy
        let switchMapTracker = SwitchMapTracker.create degree

        let batchA = switchGen
                     |> Seq.take 80
                     |> SwitchMapTracker.recordAndFilter 3
                                                         switchMapTracker
                     |> Seq.toArray
                                
        let batchB = switchGen
                     |> Seq.take 80
                     |> SwitchMapTracker.recordAndFilter 3
                                                         switchMapTracker
                     |> Seq.toArray

        let batchC = switchGen
                     |> Seq.take 50
                     |> SwitchMapTracker.recordAndFilter 3
                                                         switchMapTracker
                     |> Seq.toArray

        let batchD = switchGen
                     |> Seq.take 30
                     |> SwitchMapTracker.recordAndFilter 3
                                                         switchMapTracker
                     |> Seq.toArray

        Assert.AreEqual(1, 1);

    [<TestMethod>]
    member this.Stage_gusStagesOfDegree() =
        let degree = Degree.fromInt 16
        let randy = RngGen.createLcg 1234 |> Rando.fromRngGen
        let startingStageCount = StageCount.fromInt 4
        let stageCompSpan = StageCount.fromInt 2
        let startingStages = Stage.makeRandomFullStages
                                        degree
                                        randy
                             |> Seq.take (StageCount.value startingStageCount)
                             |> Seq.toList

        let pStages = Stage.gusStagesOfDegree
                                startingStages

                                        //startingStageCount
                                        //stageCompSpan
        Assert.IsTrue(true);