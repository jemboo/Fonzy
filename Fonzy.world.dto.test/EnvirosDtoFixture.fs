namespace Fonzy.world.dto.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type EnvirosDtoFixture () =

    [<TestMethod>]
    member this.TestEnviroDto () =
        let map = [("one", "one"); ("two", "two")] 
                  |> Map.ofList
        let objEnviro = enviro.ObjectMap map
        let objEnviroDto = objEnviro |> EnviroDto.toDto
        let objEnviroBack = objEnviroDto 
                                |> EnviroDto.fromDto 
                                |> Result.ExtractOrThrow
        Assert.AreEqual(objEnviro, objEnviroBack);


    [<TestMethod>]
    member this.Seri () =
        let supA = Sorting.switchUsePlan.All;
        let supAc = Json.serialize supA
        let supAb = Json.deserialize<Sorting.switchUsePlan> supAc
                    |> Result.ExtractOrThrow
        Assert.AreEqual(supA, supAb)
        let supR = Sorting.switchUsePlan.Range (3,11);
        let supRc = Json.serialize supR
        let supRb = Json.deserialize<Sorting.switchUsePlan> supRc
                    |> Result.ExtractOrThrow
        Assert.AreEqual(supR, supRb)
        Assert.IsTrue(true)