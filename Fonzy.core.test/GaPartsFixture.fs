namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type GaPartsFixture () =

    [<TestMethod>]
    member this.TestPoolOfGridLocations () =
        let poolId = Guid.NewGuid()
        let poolCount = PoolCount.fromInt 1000
        let locIds = seq {0 .. (PoolCount.value poolCount)}
                     |> Seq.map(fun _ -> Guid.NewGuid())
                     |> Seq.toArray

        let pogl = PoolOfGridLocations.makeOrigin poolId locIds

        let diffPoolId = Guid.NewGuid()
        let xRadius = Some 10
        let yRadius = Some 10
        let stDev = 4.0
        let randy = Rando.LcgFromSeed 123
        let pogldiff = PoolOfGridLocations.gaussianDiffuse 
                        xRadius yRadius stDev randy pogl diffPoolId

        let xDisp = pogldiff |> PoolOfGridLocations.getGridLocations
                             |> Seq.map(fun gl -> Math.Abs (float gl.x))
                             |> Seq.average

        Assert.IsTrue(xDisp > 2.0);


    [<TestMethod>]
    member this.TestPoolOfGridLocationsDto () =
        let poolId = Guid.NewGuid()
        let poolCount = PoolCount.fromInt 1000
        let locIds = seq {0 .. (PoolCount.value poolCount)}
                     |> Seq.map(fun _ -> Guid.NewGuid())
                     |> Seq.toArray

        let pogl = PoolOfGridLocations.makeOrigin poolId locIds
        let diffPoolId = Guid.NewGuid()
        let xRadius = Some 10
        let yRadius = Some 10
        let stDev = 4.0
        let randy = Rando.LcgFromSeed 123
        let pogldiff = PoolOfGridLocations.gaussianDiffuse 
                        xRadius yRadius stDev randy pogl diffPoolId


        let pogldDto = PoolOfGridLocationsDto.toDto pogldiff
        let jsn = Json.serialize pogldDto
        let pogldDtoBack = Json.deserialize jsn |> Result.ExtractOrThrow
        let pogldiffBack = PoolOfGridLocationsDto.fromDto pogldDtoBack |> Result.ExtractOrThrow
        Assert.AreEqual(pogldiff, pogldiffBack)
