namespace global
open System


type AncestryDto = {cat:string; value:string}
module AncestryDto =
    let toDto (ancestry:Ancestry) =
         match ancestry with
         | NoAncestry -> {cat="NoAncestry"; value = ""}
         | SingleParent id -> {cat="SingleParent"; value = (string (OrgId.value id))}
         | SingleDistantParent (id, gen) -> 
                    let genDto = (GenerationNumber.value gen).ToString()
                    {
                        cat="SingleDistantParent"; 
                        value = Json.serialize [(string (OrgId.value id)); genDto]
                    }

    let fromDto (eDto:AncestryDto) =
        if eDto.cat = "NoAncestry" then
            result {
                return Ancestry.NoAncestry
            }
        else if eDto.cat = "SingleParent" then
            result {
                let! gu = eDto.value |> GuidUtils.guidFromStringR
                let! orgId = gu |> OrgId.create
                return Ancestry.SingleParent orgId
            }
        else if eDto.cat = "SingleDistantParent" then
            result {
                let! vals = eDto.value |> Json.deserialize<string[]>
                let! gu = vals.[0] |> GuidUtils.guidFromStringR
                let! orgId = gu |> OrgId.create
                let! gen = (vals.[1]) |> int |> GenerationNumber.create ""
                return Ancestry.SingleDistantParent (orgId, gen)
            }
        else sprintf "cat: %s for AncestryDto not found"
                      eDto.cat |> Error



type GenomeDto = {cat:string; value:string}
module GenomeDto =
    let toDto (genome:Genome) =
         match genome with
         | NoGenome -> {cat="NoGenome"; value = ""}
         | Genome.Sorter sorterGenome -> 
                {
                    GenomeDto.cat = "Sorter"; 
                    value = sorterGenome |> SorterGenomeDto.toDto |> Json.serialize 
                }

    let fromDto (eDto:GenomeDto) =
        if eDto.cat = "NoGenome" then
            result {
                return Genome.NoGenome
            }
        else if eDto.cat = "Sorter" then
            result {
                let! sgDto = eDto.value |> Json.deserialize<sorterGenomeDto>
                let! sg = sgDto |> SorterGenomeDto.fromDto
                return Genome.Sorter sg
            }
        else sprintf "cat: %s for GenomeDto not found"
                      eDto.cat |> Error



type PhenotypeDto = {cat:string; value:string}
module PhenotypeDto =

    let toDto (phenotype:Phenotype) =
         match phenotype with
         | NoPhenotype -> {cat="NoPhenotype"; value = ""}
         | Phenotype.Sorter sorterPhenotype -> 
             {
                 PhenotypeDto.cat="Sorter"; 
                 value = sorterPhenotype |> SorterPhenotypeDto.toDto |> Json.serialize
             }

    let fromDto (eDto:PhenotypeDto) =
        if eDto.cat = "NoPhenotype" then
            result {
                return Phenotype.NoPhenotype
            }
        else if eDto.cat = "Sorter" then
            result {
                let! sp = eDto.value |> SorterPhenotypeDto.fromJson
                return Phenotype.Sorter sp
            }
        else sprintf "cat: %s for PhenotypeDto not found"
                      eDto.cat |> Error



type OrgPerformanceDto = {cat:string; value:string}
module OrgPerformanceDto =
    let toDto (orgPerformance:OrgPerformance) =
         match orgPerformance with
         | NoPerformance -> {cat="NoPerformance"; value = ""}
         | OrgPerformance.Sorter sorterTestResults -> 
             {
                 OrgPerformanceDto.cat = "Sorter";
                 value = sorterTestResults |> SorterTestResultsDto.toDto |> Json.serialize
             }

    let fromDto (eDto:OrgPerformanceDto) =
        if eDto.cat = "NoPerformance" then
                OrgPerformance.NoPerformance |> Ok
        else if eDto.cat = "Sorter" then
            result {
                let! sorterTestResults = SorterTestResultsDto.fromJson eDto.value
                return OrgPerformance.Sorter sorterTestResults
            }
        else sprintf "cat: %s for PhenotypeEvalDto not found"
                      eDto.cat |> Error



type PhenotypeEvalDto = {cat:string; value:string}
module PhenotypeEvalDto =
    let toDto (phenotypeEval:PhenotypeEval) =
         match phenotypeEval with
         | NoPhenotypeEval -> {cat="NoPhenotype"; value = ""}
         | Sorter sorterPhenotypeEval -> 
             {
                 cat="Sorter"; 
                 value = sorterPhenotypeEval |> SorterPhenotypeEvalDto.toDto |> Json.serialize
             }

    let fromDto (eDto:PhenotypeEvalDto) =
        if eDto.cat = "NoPhenotype" then
            result {
                return PhenotypeEval.NoPhenotypeEval
            }
        else if eDto.cat = "Sorter" then
            result {
                let! spe = eDto.value |> SorterPhenotypeEvalDto.fromJson
                return PhenotypeEval.Sorter spe
            }
        else sprintf "cat: %s for PhenotypeEvalDto not found"
                      eDto.cat |> Error



type OrgDto = 
    {
        orgId:Guid
        ancestryDto:AncestryDto
        genomeDto:GenomeDto
        generation:int
        phenotypeDto:PhenotypeDto
        orgPerformanceDto:OrgPerformanceDto
        phenotypeEvalDto:PhenotypeEvalDto
    }

module OrgDto =

    let fromDto (dto:OrgDto) =
        result 
            {
                let! orgId = OrgId.create dto.orgId
                let! ancestry = dto.ancestryDto |> AncestryDto.fromDto
                let! genome = dto.genomeDto |> GenomeDto.fromDto
                let! generation = GenerationNumber.create "" dto.generation
                let! phenotype = dto.phenotypeDto |> PhenotypeDto.fromDto
                let! orgPerformance = dto.orgPerformanceDto |> OrgPerformanceDto.fromDto
                let! phenotypeEval = dto.phenotypeEvalDto |> PhenotypeEvalDto.fromDto

                return {
                    Org.orgId = orgId
                    ancestry = ancestry
                    genome = genome
                    generation = generation
                    phenotype = phenotype
                    orgPerformance = orgPerformance
                    phenotypeEval = phenotypeEval
                }
            }


    let toDto (org:Org) =
        {
            OrgDto.orgId = OrgId.value org.orgId
            ancestryDto = org.ancestry  |> AncestryDto.toDto
            genomeDto = org.genome  |> GenomeDto.toDto
            generation = GenerationNumber.value org.generation
            phenotypeDto = org.phenotype  |> PhenotypeDto.toDto
            orgPerformanceDto = org.orgPerformance |> OrgPerformanceDto.toDto
            phenotypeEvalDto = org.phenotypeEval |> PhenotypeEvalDto.toDto
        }


type OrgsDto = 
    {
        id:Guid
        orgs:OrgDto[]
    }
    
module OrgsDto =
    
    let fromDto (dto:OrgsDto) =
        result 
            {
                let! orgList = dto.orgs
                                |> Array.map(OrgDto.fromDto)
                                |> Array.toList |> Result.sequence
    
                let orgMap = orgList |> List.map(fun gl-> (gl.orgId, gl))
                                     |> Map.ofList
    
                return {
                    Orgs.id = OrgsId.fromGuid dto.id
                    orgMap = orgMap
                }
            }
    
    let toDto (orgs:Orgs) =
        {
            OrgsDto.id = OrgsId.value orgs.id
            OrgsDto.orgs = orgs.orgMap
                            |> Map.toArray
                            |> Array.map(fun tup-> snd tup)
                            |> Array.map(OrgDto.toDto)
        }
            