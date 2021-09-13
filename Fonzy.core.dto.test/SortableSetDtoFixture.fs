namespace Fonzy.core.dto.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SortableSetDtoFixture () =

    [<TestMethod>]
    member this.sortableSetTypeDto_AllForDegree() =
        let degree = Degree.fromInt 5
        let ssr = sortableSetRep.Bp64 degree
        let sst = sortableSetType.AllForDegree (ssr)

        let dto = sst |> SortableSetTypeDto.toDto
        let sstBack = dto |> SortableSetTypeDto.fromDto
                          |> Result.ExtractOrThrow
        Assert.AreEqual(sst, sstBack);

    [<TestMethod>]
    member this.sortableSetTypeDto_Explicit() =
        let ssID = SortableSetId.fromGuid (Guid.NewGuid())
        let sst = sortableSetType.Explicit ssID
        let dto = sst |> SortableSetTypeDto.toDto
        let sstBack = dto |> SortableSetTypeDto.fromDto
                          |> Result.ExtractOrThrow
        Assert.AreEqual(sst, sstBack);


         
    [<TestMethod>]
    member this.sortableSetTypeDto_Random() =
        let degree = Degree.fromInt 5
        let ssr = sortableSetRep.Bp64 degree
        let rng = RngGen.createLcg (RandomSeed.fromInt 123)
        let sc = SortableCount.fromInt 22
        let sst = sortableSetType.Random (rng, sc, ssr)
        let dto = sst |> SortableSetTypeDto.toDto
        let sstBack = dto |> SortableSetTypeDto.fromDto
                          |> Result.ExtractOrThrow
        Assert.AreEqual(sst, sstBack);


    [<TestMethod>]
    member this.sortableSetTypeDto_SwitchReduced() =
        let degree = Degree.fromInt 5
        let ssr = sortableSetRep.Bp64 degree
        let rng = RngGen.createLcg (RandomSeed.fromInt 123)
        let sc = SortableCount.fromInt 22
        let sst = sortableSetType.Random (rng, sc, ssr)
        let swLst = [0;1;2;] |> List.map( SwitchDto.fromDto)
                             |> Result.sequence
                             |> Result.ExtractOrThrow

        let sstR = sortableSetType.SwitchReduced (sst, swLst)
        let dto = sstR |> SortableSetTypeDto.toDto
        let sstBack = dto |> SortableSetTypeDto.fromDto
                          |> Result.ExtractOrThrow
        Assert.AreEqual(sstR, sstBack);


    [<TestMethod>]
    member this.sortableSetImplDto() =
        let degree = Degree.fromInt 5
        let intSets = [|{intSet.values =[|0;4;3;2;1|]}; {intSet.values =[|1;4;3;2;0|]}|]
        let ssImpl = sortableSetImpl.Integer (intSets, degree)
        let ssDto = ssImpl |> SortableSetImplDto.toDto
        let ssImplBack = ssDto |> SortableSetImplDto.fromDto
                               |> Result.ExtractOrThrow
        Assert.AreEqual(ssImpl, ssImplBack);