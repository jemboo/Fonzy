namespace Fonzy.core.dto.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SorterGaDtoFixture () =

    [<TestMethod>]
    member this.SorterGenomeDto_Permutation() =
        let tsSorterGenome = SorterGenome.Permutaions TestData.SorterGa.twoCycleList
        let swSgDto = SorterGenomeDto.toDto tsSorterGenome
        let tsSorterGenomeBack = SorterGenomeDto.fromDto swSgDto
                                  |> Result.ExtractOrThrow
        Assert.AreEqual(tsSorterGenome, tsSorterGenomeBack);


    [<TestMethod>]
    member this.SorterGenomeDto_Switch() =
        let swSorterGenome = SorterGenome.Switches TestData.SorterParts.switchList
        let swSgDto = SorterGenomeDto.toDto swSorterGenome
        let swSorterGenomeBack = SorterGenomeDto.fromDto swSgDto
                                  |> Result.ExtractOrThrow
        Assert.AreEqual(swSorterGenome, swSorterGenomeBack);


    [<TestMethod>]
    member this.SorterPhenotypeDto_Single() =
        let phenoSingle = SorterPhenotype.Singleton 
                            (TestData.SorterParts.makeRandomSorter())
        let dto = SorterPhenotypeDto.toDto phenoSingle
        let cereal = Json.serialize dto
        let dtoBack = Json.deserialize<SorterPhenotypeDto> cereal 
                        |> Result.ExtractOrThrow
        Assert.AreEqual(dto, dtoBack);
        let phenoSingleBack = SorterPhenotypeDto.fromDto dtoBack
                                |> Result.ExtractOrThrow
        Assert.AreEqual(phenoSingle, phenoSingleBack);


    [<TestMethod>]
    member this.SorterPhenotypeDto_Multiple() =
        let phenoMulti = SorterPhenotype.Multiple TestData.SorterGa.sorterList
        let dto = SorterPhenotypeDto.toDto phenoMulti
        let cereal = Json.serialize dto
        let dtoBack = Json.deserialize<SorterPhenotypeDto> cereal 
                        |> Result.ExtractOrThrow
        Assert.AreEqual(dto, dtoBack);
        let phenoMultiBack = SorterPhenotypeDto.fromDto dtoBack
                                |> Result.ExtractOrThrow
        Assert.AreEqual(phenoMulti, phenoMultiBack);


    [<TestMethod>]
    member this.SorterTestResultsDto_Single() =
        let switchUses = SwitchUses.init TestData.SorterParts.switchUseArray
        let testRestultSingle = SorterTestResults.Singleton switchUses
        let dto = SorterTestResultsDto.toDto testRestultSingle
        let cereal = Json.serialize dto
        let dtoBack = Json.deserialize<SorterTestResultsDto> cereal 
                        |> Result.ExtractOrThrow
        Assert.AreEqual(dto, dtoBack);
        let testResultSingleBack = SorterTestResultsDto.fromDto dtoBack
                                |> Result.ExtractOrThrow
        Assert.AreEqual(testRestultSingle, testResultSingleBack);


    [<TestMethod>]
    member this.SorterTestResultsDto_Multiple() =
        let testRestultMultiple = SorterTestResults.Multiple 
                                    TestData.SorterGa.listOfSwitchUses
        let dto = SorterTestResultsDto.toDto testRestultMultiple
        let cereal = Json.serialize dto
        let dtoBack = Json.deserialize<SorterTestResultsDto> cereal 
                        |> Result.ExtractOrThrow
        Assert.AreEqual(dto, dtoBack);
        let testRestultMultipleBack = SorterTestResultsDto.fromDto dtoBack
                                        |> Result.ExtractOrThrow
        Assert.AreEqual(testRestultMultiple, testRestultMultipleBack);




    [<TestMethod>]
    member this.SorterPhenotypeEvalDto_Single() =
        let phenotypeEvalSingle = SorterPhenotypeEval.Singleton 42.0
        let dto = SorterPhenotypeEvalDto.toDto phenotypeEvalSingle
        let cereal = Json.serialize dto
        let dtoBack = Json.deserialize<SorterPhenotypeEvalDto> cereal 
                        |> Result.ExtractOrThrow
        Assert.AreEqual(dto, dtoBack);
        let testResultSingleBack = SorterPhenotypeEvalDto.fromDto dtoBack
                                |> Result.ExtractOrThrow
        Assert.AreEqual(phenotypeEvalSingle, testResultSingleBack);


    [<TestMethod>]
    member this.SorterPhenotypeEvalDto_Multiple() =
        let phenotypeEvalMultiple = SorterPhenotypeEval.Multiple [1.0; 2.0; 3.0;]
        let dto = SorterPhenotypeEvalDto.toDto phenotypeEvalMultiple
        let cereal = Json.serialize dto
        let dtoBack = Json.deserialize<SorterPhenotypeEvalDto> cereal 
                        |> Result.ExtractOrThrow
        Assert.AreEqual(dto, dtoBack);
        let testRestultMultipleBack = SorterPhenotypeEvalDto.fromDto dtoBack
                                        |> Result.ExtractOrThrow
        Assert.AreEqual(phenotypeEvalMultiple, testRestultMultipleBack);



