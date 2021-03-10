namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SorterOpsFixture () =

    [<TestMethod>]
    member this.switchUseRollout() =
        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let sorter16 = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
        let sortableSet = SortableSetRollout.allBinary degree |> Result.ExtractOrThrow    

        let resR = SortingOps.makeSwitchUses 
                        sorter16 sortableSet SwitchusePlan.All
        let wR = snd resR    
        let histo = wR |> SortableSetRollout.toSortableIntArrays
                       |> Seq.countBy id
                       |> Seq.toArray
        Assert.IsTrue(histo.Length > 1)

    [<TestMethod>]
    member this.Rollout() =
        let degree = (Degree.create "" 16 ) |> Result.ExtractOrThrow
        let sorter16 = RefSorter.CreateRefSorter RefSorter.Green16 |> Result.ExtractOrThrow
        let sortableSet = SortableSetRollout.allBinary degree |> Result.ExtractOrThrow    

        let resR = SortingOps.makeSwitchUsesRollout 
                            sorter16 sortableSet SwitchusePlan.All
        let Rollout = snd resR
        let histo = Rollout |> SortableSetRollout.toSortableIntArrays
                       |> Seq.countBy id
                       |> Seq.toArray

        let useTrack = fst resR

        let rollout = SwitchUseRollout.create 
                                sorter16.switchCount
                                sortableSet.sortableCount

        Assert.IsTrue(true)