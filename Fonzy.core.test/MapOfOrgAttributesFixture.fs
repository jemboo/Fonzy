namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type GuidMapOfAttributesFixture () =
    //[<TestMethod>]
    //member this.TestMakeMapOfIntAttributes() =
    //    let poolId = Guid.NewGuid()
    //    let poolSz = 8
    //    let mutable curVal = -1
    //    let getMemVal g =
    //        curVal <- curVal + 1
    //        curVal
    //    let orgids = 
    //            (seq {0 .. poolSz} 
    //            |> Seq.map(fun _ -> Guid.NewGuid()))
    //            |> Seq.map(fun g -> OrgId.create g)
    //            |> Seq.toList
    //            |> Result.sequence
    //            |> Result.ExtractOrThrow
                        

    //    let qua = MapOfOrgAttributes.makeMapOfAttributes
    //                   poolId 
    //                   orgids
    //                   getMemVal

    //    let quaDto = MapOfOrgAttributesDto.toDto<int> (AttributesDto.fromIntToDto) qua

    //    let quaJson = Json.serialize quaDto
    //    let quaDtoBack = Json.deserialize<MapOfOrgAttributesDto> quaJson |> Result.ExtractOrThrow
    //    let quaBack = MapOfOrgAttributesDto.fromDto (AttributesDto.fromDtoToInt)  quaDtoBack |> Result.ExtractOrThrow

    //    Assert.AreEqual (qua , quaBack);


    //[<TestMethod>]
    //member this.TestMakeMapOfLatticeLoc2dAttributes() =
    //    let poolId = Guid.NewGuid()
    //    let poolSz = 8
    //    let mutable curVal = -1
    //    let getMemVal g =
    //        curVal <- curVal + 1
    //        {LatticeLoc2d.x = curVal; y=curVal}
    //    let orgids = 
    //            (seq {0 .. poolSz} 
    //            |> Seq.map(fun _ -> Guid.NewGuid()))
    //            |> Seq.map(fun g -> OrgId.create g)
    //            |> Seq.toList
    //            |> Result.sequence
    //            |> Result.ExtractOrThrow
                        

    //    let qua = MapOfOrgAttributes.makeMapOfAttributes
    //                   poolId 
    //                   orgids
    //                   getMemVal

    //    let quaDto = MapOfOrgAttributesDto.toDto<LatticeLoc2d> 
    //                   (AttributesDto.fromLatticeLoc2dToDto) qua

    //    let quaJson = Json.serialize quaDto
    //    let quaDtoBack = Json.deserialize<MapOfOrgAttributesDto> quaJson |> Result.ExtractOrThrow
    //    let quaBack = MapOfOrgAttributesDto.fromDto (AttributesDto.fromDtoToLatticeLoc2d)  quaDtoBack |> Result.ExtractOrThrow

    //    Assert.AreEqual (qua , quaBack);


    [<TestMethod>]
    member this.TestCreateUniform() =
        let poolId = Guid.NewGuid()
        let poolSz = 8
        let mutable curVal = -1
        let getMemVal() =
            {LatticeLoc2d.x=1;y=2}
        let orgids = 
                (seq {0 .. poolSz} 
                |> Seq.map(fun _ -> Guid.NewGuid()))
                |> Seq.map(fun g -> OrgId.create g)
                |> Seq.toList
                |> Result.sequence
                |> Result.ExtractOrThrow
                    

        let qua = MapOfOrgAttributes.createUniform
                       poolId 
                       orgids
                       getMemVal

        let items = qua.attrMap |> Map.toArray
        let aVal = qua.attrMap.[fst items.[0]]
        Assert.AreEqual (aVal , getMemVal());




    //[<TestMethod>]
    //member this.TestPoolOfGridLocations() =
    //    let poolId = Guid.NewGuid()
    //    let poolCount = PoolCount.fromInt 1000
    //    let locIds = seq {0 .. (PoolCount.value poolCount)}
    //                 |> Seq.map(fun _ -> Guid.NewGuid())
    //                 |> Seq.toArray

    //    let pogl = PoolOfGridLocations.makeOrigin poolId locIds

    //    let diffPoolId = Guid.NewGuid()
    //    let xRadius = Some 10
    //    let yRadius = Some 10
    //    let stDevX = 4.0
    //    let stDevY = 40.0
    //    let randy = Rando.LcgFromSeed 123
    //    let pogldiff = PoolOfGridLocations.gaussianDiffuse 
    //                    xRadius yRadius stDevX stDevY randy pogl diffPoolId

    //    let xDisp = pogldiff |> PoolOfGridLocations.getGridLocations
    //                         |> Seq.map(fun gl -> Math.Abs (float gl.x))
    //                         |> Seq.average

    //    Assert.IsTrue(xDisp > 2.0);



    //[<TestMethod>]
    //member this.TestPoolOfGridLocationsDto() =
    //    let poolId = Guid.NewGuid()
    //    let poolCount = PoolCount.fromInt 1000
    //    let locIds = seq {0 .. (PoolCount.value poolCount)}
    //                 |> Seq.map(fun _ -> Guid.NewGuid())
    //                 |> Seq.toArray

    //    let pogl = PoolOfGridLocations.makeOrigin poolId locIds
    //    let diffPoolId = Guid.NewGuid()
    //    let xRadius = Some 10
    //    let yRadius = Some 10
    //    let stDev = 4.0
    //    let randy = Rando.LcgFromSeed 123
    //    let pogldiff = PoolOfGridLocations.gaussianDiffuse 
    //                    xRadius yRadius stDev randy pogl diffPoolId


    //    let pogldDto = PoolOfGridLocationsDto.toDto pogldiff
    //    let jsn = Json.serialize pogldDto
    //    let pogldDtoBack = Json.deserialize jsn |> Result.ExtractOrThrow
    //    let pogldiffBack = PoolOfGridLocationsDto.fromDto pogldDtoBack 
    //                        |> Result.ExtractOrThrow
    //    Assert.AreEqual(pogldiff, pogldiffBack)



    //[<TestMethod>]
    //member this.TestAttributesDto() =
    //    let iIn = 3
    //    let iInDto = AttributesDto.fromIntToDto iIn
    //    let iOut = AttributesDto.fromDtoToInt iInDto |> Result.ExtractOrThrow
    //    Assert.AreEqual(iIn, iOut)

    //    let gl2dIn = {LatticeLoc2d.x=1; y=2;}
    //    let gl2InDto = AttributesDto.fromLatticeLoc2dToDto gl2dIn
    //    let gl2dOut = AttributesDto.fromDtoToLatticeLoc2d gl2InDto |> Result.ExtractOrThrow
    //    Assert.AreEqual(gl2dIn, gl2dOut)

    //    let gl3dIn = {LatticeLoc3d.x=1; y=2; z=3}
    //    let gl3InDto = AttributesDto.fromLatticeLoc3dToDto gl3dIn
    //    let gl3dOut = AttributesDto.fromDtoToLatticeLoc3d gl3InDto |> Result.ExtractOrThrow
    //    Assert.AreEqual(gl3dIn, gl3dOut)