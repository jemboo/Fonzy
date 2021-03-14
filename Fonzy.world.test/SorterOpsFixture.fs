namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SorterOpsFixture () =

    //[<TestMethod>]
    //member this.SAGbySwitch() =
    //    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    //    let sorter16 = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
    //    let sortableSet = SortableSetRollout.allBinary degree |> Result.ExtractOrThrow    

    //    let resR = SortingOps.EvalSorterOnSortableSetSAGbySwitch 
    //                    sorter16 sortableSet SortingEval.SwitchUsePlan.All
    //    //let wR = snd resR    
    //    //let histo = wR |> SortableSetRollout.toSortableIntArrays
    //    //               |> Seq.countBy id
    //    //               |> Seq.toArray
    //    //Assert.IsTrue(histo.Length > 1)
    //    Assert.IsTrue(true)

    //[<TestMethod>]
    //member this.SAGbySortable() =
    //    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    //    let sorter16 = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
    //    let sortableSet = SortableSetRollout.allBinary degree |> Result.ExtractOrThrow    

    //    let resR = SortingOps.EvalSorterOnSortableSetSAGbySortable 
    //                    sorter16 sortableSet SortingEval.SwitchUsePlan.All
    //    //let wR = snd resR  
    //    //let su = fst resR
    //    //let sus  = su.weights |> Array.sortDescending
    //    //let histo = wR |> SortableSetRollout.toSortableIntArrays
    //    //               |> Seq.countBy id
    //    //               |> Seq.toArray
    //    //Assert.IsTrue(sus.Length > 1)
    //    Assert.IsTrue(true)

    //[<TestMethod>]
    //member this.NoSAG() =
    //    let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
    //    let sorter16 = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
    //    let sortableSet = SortableSetRollout.allBinary degree |> Result.ExtractOrThrow    

    //    let resR = SortingOps.EvalSorterOnSortableSetWithNoSAG 
    //                        sorter16 sortableSet SortingEval.SwitchUsePlan.All
    //    //let Rollout = snd resR
    //    //let histo = Rollout |> SortableSetRollout.toSortableIntArrays
    //    //               |> Seq.countBy id
    //    //               |> Seq.toArray

    //    //let useTrack = fst resR

    //    let rollout = SwitchEventRollout.create 
    //                            sorter16.switchCount
    //                            sortableSet.sortableCount
    //    Assert.IsTrue(true)


    [<TestMethod>]
    member this.evalRecordCorrectSorter() =
        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let sorter16 = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
        let sortableSetEx = SortableSet.Generated (SortableSetGenerated.allIntBits degree)
                                |> SortableSet.getSortableSetExplicit
                                |> Result.ExtractOrThrow 

        let ssR = SortingOps.evalSorter 
                            sorter16 sortableSetEx SortingEval.SwitchUsePlan.All
                            SortingEval.SwitchEventGrouping.BySwitch
        let switchCount =
            match ssR with
            | SortingEval.SwitchEventRecords.GroupbySwitch s -> s.switchUses.switchCount
            | _ -> failwith "yoe"
        Assert.IsTrue((SwitchCount.value switchCount) > 0)