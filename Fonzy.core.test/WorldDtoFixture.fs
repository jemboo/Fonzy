namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type WorldDtoFixture () =

    [<TestMethod>]
    member this.WorldDto () =
        let worldId = Guid.NewGuid()
        let world = World.create worldId None Cause.noOp Enviro.Empty
        let worldDto = WorldDto.toDto world
        let jsonWorld = Json.serialize worldDto
        let worldDtoBack = Json.deserialize<WorldDto> jsonWorld
                            |> Result.ExtractOrThrow
        let worldBack = WorldDto.fromDto worldDtoBack
                            |> Result.ExtractOrThrow
        Assert.AreEqual(world.id, worldBack.id);
        Assert.AreEqual(world.enviroment, worldBack.enviroment);
        Assert.AreEqual(world.parentId, worldBack.parentId);
        Assert.AreEqual(world.cause, worldBack.cause);


    [<TestMethod>]
    member this.WorldDtoChild () =
        let rootGuid = Guid.NewGuid()
        let childGuid = Guid.NewGuid()
        let childCauseType = CauseType.Test (TestCauseType.CreateInt 1)
        let worldRoot = World.create rootGuid None Cause.noOp Enviro.Empty
        let worldChild = World.createFromParent childGuid worldRoot (Cause.fromCauseType childCauseType)
                             |> Result.ExtractOrThrow
        let worldDtoChild = WorldDto.toDto worldChild
        let jsonWorldChild = Json.serialize worldDtoChild
        let worldChildDtoBack = Json.deserialize<WorldDto> jsonWorldChild
                                    |> Result.ExtractOrThrow
        let worldChildBack = WorldDto.fromDto worldChildDtoBack
                                |> Result.ExtractOrThrow
        Assert.AreEqual(worldChild.id, worldChildBack.id);
        Assert.AreEqual(worldChild.enviroment, worldChildBack.enviroment);
        Assert.AreEqual(worldChild.parentId, worldChildBack.parentId);
        Assert.AreEqual(worldChild.cause.causeType, worldChildBack.cause.causeType);


    [<TestMethod>]
    member this.WorldDtoSorterPool () =
        let rootGuid = Guid.NewGuid()
        let world = World.create rootGuid None Cause.noOp (Enviro.S (SorterPoolEnviro.Bag 7))
        let worldDto = WorldDto.toDto world
        let jsonWorld = Json.serialize worldDto
        let worldDtoBack = Json.deserialize<WorldDto> jsonWorld
                            |> Result.ExtractOrThrow
        let worldBack = WorldDto.fromDto worldDtoBack
                            |> Result.ExtractOrThrow
        Assert.AreEqual(world.id, worldBack.id);
        Assert.AreEqual(world.enviroment, worldBack.enviroment);
        Assert.AreEqual(world.parentId, worldBack.parentId);
        Assert.AreEqual(world.cause, worldBack.cause);

