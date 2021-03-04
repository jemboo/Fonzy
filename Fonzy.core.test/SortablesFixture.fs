namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SortablesFixture () =

    [<TestMethod>]
    member this.TestIdentityPermutation() =
      let arA = [1;2;3]
      let arB = [1;2;3]
      Assert.AreEqual(arA, arB)
      let arAf = [1.0; 2.0; 3.0]
      let arBf = [1.0; 2.0; 3.0]
      Assert.AreEqual(arAf, arBf)