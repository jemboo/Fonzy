namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type EnvirosFixture () =

    [<TestMethod>]
    member this.TestMethodPassing () =
        Assert.IsTrue(true);
