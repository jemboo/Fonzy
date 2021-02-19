namespace Fonzy.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting



[<TestClass>]
type CommonTypesFixture() =

    [<TestMethod>]
    member this.PrintArray() =
        let st = SwitchUses.createEmpty (SwitchCount.fromInt 1000)
        let wts = SwitchUses.getWeights st
        let ss = sprintf "%A" wts |> (fun s->s.Replace("\n ", ""))
        Assert.AreEqual(1, 1)


