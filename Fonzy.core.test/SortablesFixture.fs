namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SortablesFixture () =

    [<TestMethod>]
    member this.TestIdentityPermutation() =
      Assert.AreEqual(1, 1)
