namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type GpOpsFixture () =

    [<TestMethod>]
    member this.TestMethodPassing () =
        Assert.IsTrue(true);
