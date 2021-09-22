namespace Fonzy.core.dto.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type OrgsDtoFixture () =

    [<TestMethod>]
    member this.AncestryDto() =
        let gu = Guid.NewGuid()
        let orgId = OrgId.fromGuid gu
        let ancestry = Ancestry.SingleParent orgId
        let ancDto = ancestry |> AncestryDto.toDto
        let ancestryBack = ancDto |> AncestryDto.fromDto
                                  |> Result.ExtractOrThrow
        Assert.AreEqual(ancestry, ancestryBack);
        let gn = GenerationNumber.fromInt 5
        let ancestryDp = Ancestry.SingleDistantParent (orgId, gn)
        let ancDtoDp = ancestryDp |> AncestryDto.toDto
        let ancestryBackDp = ancDtoDp |> AncestryDto.fromDto
                                      |> Result.ExtractOrThrow
        Assert.AreEqual(ancestryDp, ancestryBackDp);

    [<TestMethod>]
    member this.OrgGenomeDto() =
        let swSorterGenome = sorterGenome.Switches TestData.SorterParts.switchList
        let genome = Genome.Sorter swSorterGenome
        let genomeDto = GenomeDto.toDto genome
        let genomeBack = GenomeDto.fromDto genomeDto
                                  |> Result.ExtractOrThrow
        Assert.AreEqual(genome, genomeBack);


    [<TestMethod>]
    member this.OrgPhenotypeDto() =
        let phenoMulti = sorterPhenotype.Multiple TestData.SorterGa.sorterList
        let phenotype = Phenotype.Sorter phenoMulti
        let dto = PhenotypeDto.toDto phenotype
        let cereal = Json.serialize dto
        let dtoBack = Json.deserialize<PhenotypeDto> cereal 
                        |> Result.ExtractOrThrow
        Assert.AreEqual(dto, dtoBack);
        let phenotypeBack = PhenotypeDto.fromDto dtoBack
                                |> Result.ExtractOrThrow
        Assert.AreEqual(phenotype, phenotypeBack);


    [<TestMethod>]
    member this.OrgTestResultsDto() =
        let testRestultMultiple = sorterTestResults.Multiple 
                                    TestData.SorterGa.listOfSwitchUses
        let orgPerformance = OrgPerformance.Sorter testRestultMultiple
        let dto = OrgPerformanceDto.toDto orgPerformance
        let cereal = Json.serialize dto
        let dtoBack = Json.deserialize<OrgPerformanceDto> cereal 
                        |> Result.ExtractOrThrow
        Assert.AreEqual(dto, dtoBack);
        let orgPerformanceBack = OrgPerformanceDto.fromDto dtoBack
                                        |> Result.ExtractOrThrow
        Assert.AreEqual(orgPerformance, orgPerformanceBack);


    [<TestMethod>]
    member this.OrgPhenotypeEvalDto() =
        let phenotypeEvalMultiple = sorterPhenotypeEval.Multiple [1.0; 2.0; 3.0;]
        let phenotypeEval = PhenotypeEval.Sorter phenotypeEvalMultiple
        let dto = PhenotypeEvalDto.toDto phenotypeEval
        let cereal = Json.serialize dto
        let dtoBack = Json.deserialize<PhenotypeEvalDto> cereal 
                        |> Result.ExtractOrThrow
        Assert.AreEqual(dto, dtoBack);
        let phenotypeEvalBack = PhenotypeEvalDto.fromDto dtoBack
                                        |> Result.ExtractOrThrow
        Assert.AreEqual(phenotypeEval, phenotypeEvalBack);


    [<TestMethod>]
    member this.OrgDto_Degenerate() =
        let orgId = OrgId.fromGuid (Guid.NewGuid())
        let ancestry = Ancestry.NoAncestry
        let genome = Genome.NoGenome
        let generation = GenerationNumber.fromInt 0
        let phenotype = Phenotype.NoPhenotype
        let orgPerformance = OrgPerformance.NoPerformance
        let phenotypeEval = PhenotypeEval.NoPhenotypeEval
        let org = Org.create orgId ancestry genome generation 
                             phenotype orgPerformance phenotypeEval
        let orgDto = org |> OrgDto.toDto
        let cereal = orgDto |> Json.serialize
        let orgDtoBack = cereal |> Json.deserialize<OrgDto> |> Result.ExtractOrThrow
        Assert.AreEqual(orgDto, orgDtoBack);
        let orgBack = orgDto |> OrgDto.fromDto |> Result.ExtractOrThrow
        Assert.AreEqual(org, orgBack);



    [<TestMethod>]
    member this.OrgDto_Sorter() =
        let orgId = OrgId.fromGuid (Guid.NewGuid())
        let ancestry = Ancestry.SingleParent orgId
        let genome = Genome.Sorter (sorterGenome.Switches TestData.SorterParts.switchList)
        let generation = GenerationNumber.fromInt 0
        let phenotype = Phenotype.Sorter (sorterPhenotype.Multiple TestData.SorterGa.sorterList)
        let orgPerformance = OrgPerformance.Sorter (sorterTestResults.Multiple 
                                                        TestData.SorterGa.listOfSwitchUses)
        let phenotypeEval = PhenotypeEval.Sorter (sorterPhenotypeEval.Multiple [1.0; 2.0; 3.0;])
        let org = Org.create orgId ancestry genome generation 
                             phenotype orgPerformance phenotypeEval
        let orgDto = org |> OrgDto.toDto
        let cereal = orgDto |> Json.serialize
        let orgDtoBack = cereal |> Json.deserialize<OrgDto> |> Result.ExtractOrThrow
        Assert.AreEqual(orgDto, orgDtoBack);
        let orgBack = orgDto |> OrgDto.fromDto |> Result.ExtractOrThrow
        Assert.AreEqual(org, orgBack);