namespace Fonzy.world.dto.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type EnvirosDtoFixture () =

    [<TestMethod>]
    member this.TestEnviroDto () =
        let map = [("one", "one"); ("two", "two")] 
                  |> Map.ofList
        let objEnviro = Enviro.ObjectMap map
        let objEnviroDto = objEnviro |> EnviroDto.toDto
        let objEnviroBack = objEnviroDto 
                                |> EnviroDto.fromDto 
                                |> Result.ExtractOrThrow
        Assert.AreEqual(objEnviro, objEnviroBack);
