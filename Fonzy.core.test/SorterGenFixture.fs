namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SorterGenFixture () =

    [<TestMethod>]
    member this.oddeven_merge_switches_try_oddballs() =
        seq {8 .. 64 } 
        |> Seq.iter (fun v -> 
            let ws = SorterGen.oddeven_merge_switches v
            let ts = ws |> Stage.fromSwitches (Degree.fromInt v)
                        |> Seq.toList
            Console.WriteLine (sprintf "%d\t%d\t%d" v ts.Length ws.Length))
        Assert.AreEqual(1, 1)

    [<TestMethod>]
    member this.oddeven_merge_switches() =
        let v = 128
        let ws = SorterGen.oddeven_merge_switches v
        let ts = ws |> Stage.fromSwitches (Degree.fromInt v)
                    |> Seq.toList
        Console.WriteLine (sprintf "%d\t%d\t%d" v ts.Length ws.Length)
        Assert.AreEqual(1, 1)


    [<TestMethod>]
    member this.oddeven_merge_stages() =
        seq {8 .. 64 } 
        |> Seq.iter (fun v -> 
            let ts = SorterGen.oddeven_merge_stages v
            let ws = ts |> List.concat
            Console.WriteLine (sprintf "%d %d" ts.Length  ws.Length))
        Assert.AreEqual(1, 1)


    [<TestMethod>]
    member this.oddeven_merge_stages_try_oddballs() =
        seq {8 .. 64 } 
        |> Seq.iter (fun v -> 
            let ts = SorterGen.oddeven_merge_stages v
            let ws = ts |> List.concat
            Console.WriteLine (sprintf "%d %d" ts.Length  ws.Length))
        Assert.AreEqual(1, 1)


    [<TestMethod>]
    member this.randomSwitches() =
        let switchCount = SwitchCount.fromInt 20
        let degree = TestData.degree
        let randy = TestData.iRando
        let randSorter = SorterRndGen.randomSwitches
                                        degree
                                        []
                                        switchCount
                                        randy
        Assert.AreEqual(randSorter.switches.Length, 
                        switchCount |> SwitchCount.value)



    [<TestMethod>]
    member this.randomStages() =
        let stageCount = StageCount.fromInt 20
        let degree = TestData.degree
        let randy = TestData.iRando
        let randSorter = SorterRndGen.randomStages
                                        degree
                                        []
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
        let mutantSorter = sorter |> SorterMutate.mutateByStage
                                        mutationRate
                                        skipPfx
                                        randy
        Assert.AreEqual(sorter.degree, mutantSorter.degree)


    [<TestMethod>]
    member this.mutateBySwitch() =
        let skipPfx = ((TestData.degree |> Degree.value) / 2)
                        |> SwitchCount.fromInt 
        let mutationRate = MutationRate.fromFloat 0.999
        let sorter = TestData.SorterParts.goodRefSorter
        let randy = TestData.iRando
        let mutantSorter = sorter |> SorterMutate.mutateBySwitch
                                        mutationRate
                                        skipPfx
                                        randy
        Assert.AreEqual(sorter.switches.Length, mutantSorter.switches.Length)