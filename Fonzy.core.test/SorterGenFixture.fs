namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SorterGenFixture () =


    [<TestMethod>]
    member this.randomSwitches() =
        let switchCount = SwitchCount.fromInt 20
        let degree = TestData.degree
        let randy = TestData.iRando
        let randSorter = SorterGen.randomSwitches
                                        degree
                                        switchCount
                                        randy
        Assert.AreEqual(randSorter.switches.Length, 
                        switchCount |> SwitchCount.value)



    [<TestMethod>]
    member this.randomStages() =
        let stageCount = StageCount.fromInt 20
        let degree = TestData.degree
        let randy = TestData.iRando
        let randSorter = SorterGen.randomStages
                                        degree
                                        stageCount
                                        SwitchFrequency.max
                                        randy
        let sorterStages = randSorter.switches 
                            |> Stage.fromSwitches degree
                            |> Seq.toArray

        Assert.AreEqual(sorterStages.Length, 
                        stageCount |> StageCount.value)



    [<TestMethod>]
    member this.mutateByStage() =
        let skipPfx = ((TestData.degree |> Degree.value) / 2)
                        |> SwitchCount.fromInt 
        let mutationRate = MutationRate.fromFloat 0.999
        let sorter = TestData.SorterParts.goodRefSorter
        let randy = TestData.iRando
        let mutantSorter = sorter |> SorterGen.mutateByStage
                                        mutationRate
                                        skipPfx
                                        randy
        Assert.AreEqual(sorter.switches.Length, mutantSorter.switches.Length)


    [<TestMethod>]
    member this.mutateBySwitch() =
        let skipPfx = ((TestData.degree |> Degree.value) / 2)
                        |> SwitchCount.fromInt 
        let mutationRate = MutationRate.fromFloat 0.999
        let sorter = TestData.SorterParts.goodRefSorter
        let randy = TestData.iRando
        let mutantSorter = sorter |> SorterGen.mutateBySwitch
                                        mutationRate
                                        skipPfx
                                        randy
        Assert.AreEqual(sorter.switches.Length, mutantSorter.switches.Length)