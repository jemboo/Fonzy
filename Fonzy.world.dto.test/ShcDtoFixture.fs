namespace Fonzy.world.dto.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type ShcDtoFixture () =

    [<TestMethod>]
    member this.TestMethodPassing () =
        let yab = nameof sortableSetSpec.Explicit
        Assert.IsTrue(true);
