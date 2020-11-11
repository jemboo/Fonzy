namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type GpPartsFixture () =

    [<TestMethod>]
    member this.TestMethodPassing () =
        Assert.IsTrue(true);
