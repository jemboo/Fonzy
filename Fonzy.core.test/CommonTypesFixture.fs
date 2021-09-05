namespace Fonzy.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Newtonsoft.Json


[<TestClass>]
type CommonTypesFixture() =

    [<TestMethod>]
    member this.Degree_maxSwitchesPerStage() =
        let degree = Degree.fromInt 7
        let msw = 3
        let cc = Degree.maxSwitchesPerStage degree
        Assert.AreEqual(msw, cc)



