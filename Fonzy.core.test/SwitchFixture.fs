namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open System.Diagnostics

[<TestClass>]
type SwitchFixture () =

    [<TestMethod>]
    member this.testSwitchMap() =
        let yak = Switch.switchMap.[1]
        let yak2 = Switch.switchMap.[3]
        Assert.IsFalse(false)


    [<TestMethod>]
    member this.allMasks() =
        let degSrc = Degree.fromInt 16
        let degDest = Degree.fromInt 12
        let srtGreen = RefSorter.createRefSorter RefSorter.End16
                       |> Result.ExtractOrThrow
        let subSorters = Switch.allMasks degSrc degDest srtGreen.switches
                         |> Seq.toArray

        let hist = subSorters |> CollectionUtils.histogram (fun a -> a.Length)
        hist |> Map.toSeq |> Seq.iter(fun (k, v) -> Debug.WriteLine (sprintf "%d\t%d" k v) )
        Assert.IsTrue(subSorters.Length > 0)
