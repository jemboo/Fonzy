namespace Fonzy.core.dto.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type SorterGenomeDtoFixture () =

    [<TestMethod>]
    member this.SorterGenomeDto_Permutation() =
        let randy = Rando.fromRngGen (RngGen.createLcg 4321)
        let degree = Degree.fromInt 8
        let permCount = 10
        
        let tsps = List.init permCount (fun _ -> 
                TwoCyclePerm.makeRandomTwoCycle degree randy 0.5)
        let tsSorterGenome = SorterGenome.Permutaions tsps
        let swSgDto = SorterGenomeDto.toDto tsSorterGenome
        let tsSorterGenomeBack = SorterGenomeDto.fromDto swSgDto
                                  |> Result.ExtractOrThrow
        Assert.AreEqual(tsSorterGenome, tsSorterGenomeBack);


    [<TestMethod>]
    member this.SorterGenomeDto_Switch() =
        let randy = Rando.fromRngGen (RngGen.createLcg 4321)
        let degree = Degree.fromInt 8
        let switchCount = 10
        let switches = Switch.randomSwitchesOfDegree degree randy
                    |> Seq.take switchCount
                    |> Seq.toList

        let swSorterGenome = SorterGenome.Switches switches
        let swSgDto = SorterGenomeDto.toDto swSorterGenome
        let swSorterGenomeBack = SorterGenomeDto.fromDto swSgDto
                                  |> Result.ExtractOrThrow
        Assert.AreEqual(swSorterGenome, swSorterGenomeBack);

    [<TestMethod>]
    member this.SorterPhenotypeDto_Single() =
        let randy = Rando.fromRngGen (RngGen.createLcg 4321)
        let degree = Degree.fromInt 8
        let sorterLength = SorterLength.degreeToRecordStageCount degree
        let sorter = Sorter.createRandom degree sorterLength None randy
        let phenoSingle = SorterPhenotype.Singleton sorter
        let dto = SorterPhenotypeDto.toDto phenoSingle
        let cereal = Json.serialize dto
        let dtoBack = Json.deserialize<SorterPhenotypeDto> cereal 
                        |> Result.ExtractOrThrow
        Assert.AreEqual(dto, dtoBack);
        let phenoSingleBack = SorterPhenotypeDto.fromDto dtoBack
        Assert.AreEqual(phenoSingle, phenoSingleBack);


    [<TestMethod>]
    member this.SorterPhenotypeDto_Multiple() =
        let randy = Rando.fromRngGen (RngGen.createLcg 4321)
        let degree = Degree.fromInt 8
        let permCount = 10
    
        let tsps = List.init permCount (fun _ -> 
                TwoCyclePerm.makeRandomTwoCycle degree randy 0.5)
        let tsSorterGenome = SorterGenome.Permutaions tsps
        let swSgDto = SorterGenomeDto.toDto tsSorterGenome
        let tsSorterGenomeBack = SorterGenomeDto.fromDto swSgDto
                                  |> Result.ExtractOrThrow
        Assert.AreEqual(tsSorterGenome, tsSorterGenomeBack);
