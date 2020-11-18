namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type EnviroDtoFixture () =

    [<TestMethod>]
    member this.EnviroDto () =
        let env = Enviro.A 1
        let envDto = EnviroDto.toDto env
        let envJson = Json.serialize envDto
        let envDtoBack = Json.deserialize<EnviroDto> envJson 
                         |> Result.ExtractOrThrow
        let envBack = EnviroDto.fromDto envDtoBack
                         |> Result.ExtractOrThrow

        Assert.AreEqual(env, envBack);
        

    [<TestMethod>]
    member this.EnviromentDto () =
        let spe = SorterPoolEnviro.Bag 5
        let env = Enviro.S spe
        let envDto = env |> EnviroDto.toDto
        let envJson = Json.serialize envDto
        let envDtoBack = Json.deserialize<EnviroDto> envJson |> Result.ExtractOrThrow
        let envBack = envDtoBack |> EnviroDto.fromDto |> Result.ExtractOrThrow
        Assert.AreEqual(env, envBack);


    [<TestMethod>]
    member this.SorterPoolEnviroDto () =
        let spe = SorterPoolEnviro.Bag 5
        let speDto = spe |> SorterPoolEnviroDto.toDto
        let speJson = Json.serialize speDto
        let speDtoBack = Json.deserialize<SorterPoolEnviroDto> speJson |> Result.ExtractOrThrow
        let speBack = speDtoBack |> SorterPoolEnviroDto.fromDto |> Result.ExtractOrThrow
        Assert.AreEqual(spe,speBack);
