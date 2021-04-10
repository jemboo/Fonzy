namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type WorldsFixture () =

    [<TestMethod>]
    member this.WorldGrandChild () =
        //let rootGuid = Guid.NewGuid()
        //let childGuid = Guid.NewGuid()
        //let childCauseType = CauseType.Test (TestCauseType.CreateInt 2)
        //let grandChildCauseType = CauseType.Test (TestCauseType.AddInt 3)
        //let worldRoot = World.create rootGuid None Cause.noOp Enviro.Empty
        //let worldChild = World.createFromParent childGuid worldRoot (Cause.fromCauseType childCauseType)
        //                     |> Result.ExtractOrThrow

        //let worldGrandChild = World.createFromParent childGuid worldChild (Cause.fromCauseType grandChildCauseType)
        //                        |> Result.ExtractOrThrow
        //let grandChildEnv = Enviro.A 5
        //Assert.AreEqual(worldGrandChild.enviroment, grandChildEnv);
        Assert.IsTrue(true);


    [<TestMethod>]
    member this.worldActionGenIntDist() =
        let worldAction = TestData.WorldAction.IntDist.randomUniform
        let newWorld = WorldAction.createWorld worldAction 
                    |> Result.ExtractOrThrow
        Assert.AreEqual(WorldId.value newWorld.id, 
                        CauseSpecId.value TestData.CauseSpec.IntDist.rndUniform.id);


    [<TestMethod>]
    member this.worldActionGenSorterDist() =
        let worldAction = TestData.WorldAction.SorterGen.randWorldAction
        let newWorld = WorldAction.createWorld worldAction 
                    |> Result.ExtractOrThrow
        let genSorterSetDto, meta = 
            Enviro.getDtoAndMetaFromEnviro<SorterSetDto>
                                 newWorld.enviro
                                 TestData.CauseSpec.SorterSet.rndSorterSetName
            |> Result.ExtractOrThrow
        let sorterSet = genSorterSetDto |> SorterSetDto.fromDto
                                        |> Result.ExtractOrThrow
        Assert.AreEqual(sorterSet.sorterCount, TestData.CauseSpec.SorterSet.sorterCount)
        Assert.AreEqual(WorldId.value newWorld.id, 
                        CauseSpecId.value TestData.CauseSpec.SorterSet.rand1.id);