namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type WorldDtoFixture () =

    [<TestMethod>]
    member this.EnvironmentDto () =
        let env = Enviroment.A 1
        let envDto = EnvironmentDto.toDto env
        let envJson = Json.serialize envDto
        let envDtoBack = Json.deserialize<EnvironmentDto> envJson 
                         |> Result.ExtractOrThrow
        let envBack = EnvironmentDto.fromDto envDtoBack
                         |> Result.ExtractOrThrow

        Assert.AreEqual(env, envBack);


    [<TestMethod>]
    member this.CauseTypeDto () =
        let ct = CauseType.CreateInt 1
        let ctDto = CauseTypeDto.toDto ct
        let ctJson = Json.serialize ctDto
        let ctDtoBack = Json.deserialize<CauseTypeDto> ctJson 
                         |> Result.ExtractOrThrow
        let ctBack = CauseTypeDto.fromDto ctDtoBack
                         |> Result.ExtractOrThrow
        Assert.AreEqual(ct, ctBack);

        let ctF = CauseType.CreateFloat 1.0
        let ctFDto = CauseTypeDto.toDto ctF
        let ctFJson = Json.serialize ctFDto
        let ctFDtoBack = Json.deserialize<CauseTypeDto> ctFJson 
                         |> Result.ExtractOrThrow
        let ctFBack = CauseTypeDto.fromDto ctFDtoBack
                         |> Result.ExtractOrThrow
        Assert.AreEqual(ctF, ctFBack);

    
    [<TestMethod>]
    member this.WorldDto () =
        let rootGuid = Guid.NewGuid()
        let world = World.create rootGuid None Cause.noOp Enviroment.Empty
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
        let childValue = 2
        let worldRoot = World.create rootGuid None Cause.noOp Enviroment.Empty
        let worldChild = World.createFromParent childGuid worldRoot (Cause.createInt childValue)
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
