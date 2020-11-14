namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type WorldFixture () =

    [<TestMethod>]
    member this.WorldGrandChild () =
        let rootGuid = Guid.NewGuid()
        let childGuid = Guid.NewGuid()
        let childCauseType = CauseType.Test (TestCauseType.CreateInt 2)
        let grandChildCauseType = CauseType.Test (TestCauseType.AddInt 3)
        let worldRoot = World.create rootGuid None Cause.noOp Enviroment.Empty
        let worldChild = World.createFromParent childGuid worldRoot (Cause.fromCauseType childCauseType)
                             |> Result.ExtractOrThrow

        let worldGrandChild = World.createFromParent childGuid worldChild (Cause.fromCauseType grandChildCauseType)
                                |> Result.ExtractOrThrow
        let grandChildEnv = Enviroment.A 5
        Assert.AreEqual(worldGrandChild.enviroment, grandChildEnv);


    [<TestMethod>]
    member this.TestMethodPassing () =
        Assert.IsTrue(true);
