namespace global
open System


type AncestryDto = {cat:string; value:string}
module AncestryDto =
    let toDto (ancestry:Ancestry) =
         match ancestry with
         | NoAncestry -> {cat="NoAncestry"; value = ""}
         | SingleParent id -> {cat="SingleParent"; value = Json.serialize id}
         | SingleDistantParent (id, gen) -> 
                    let genDto = (GenerationNumber.value gen).ToString()
                    {
                        cat="SingleDistantParent"; 
                        value = Json.serialize [id.ToString(), genDto]
                    }

    let fromDto (eDto:AncestryDto) =
        if eDto.cat = "NoAncestry" then
            result {
                return Ancestry.NoAncestry
            }
        else if eDto.cat = "SingleParent" then
            result {
                let! gu = new Guid(eDto.value) |> OrgId.create
                return Ancestry.SingleParent gu
            }
        else if eDto.cat = "SingleDistantParent" then
            result {
                let! vals = eDto.value |> Json.deserialize<string[]>
                let! gu = new Guid(vals.[0]) |> OrgId.create
                let! gen = (vals.[1]) |> int |> GenerationNumber.create ""
                return Ancestry.SingleDistantParent (gu,gen)
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
                    value = "" // sorterGenome |> SorterGenomeDto.toDto |> Json.serialize 
                }

    let fromDto (eDto:GenomeDto) =
        if eDto.cat = "NoGenome" then
            result {
                return Genome.NoGenome
            }
        else if eDto.cat = "Sorter" then
            result {
                return Genome.Sorter SorterGenome.Empty
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
                 cat="Sorter"; 
                 value = "" // sorterPhenotype |> SorterPhenotypeDto.toDto |> Json.serialize
             }

    let fromDto (eDto:PhenotypeDto) =
        if eDto.cat = "NoPhenotype" then
            result {
                return Phenotype.NoPhenotype
            }
        else if eDto.cat = "Sorter" then
            result {
                return Phenotype.Sorter SorterPhenotype.Empty
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
                 value = ""
             }

    let fromDto (eDto:OrgPerformanceDto) =
        if eDto.cat = "NoPerformance" then
                OrgPerformance.NoPerformance |> Ok
        else if eDto.cat = "Sorter" then
            result {
                return OrgPerformance.Sorter SorterTestResults.Empty
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
                 value = "" //sorterPhenotypeEval |> SorterPhenotypeEvalDto.
             }

    let fromDto (eDto:PhenotypeEvalDto) =
        if eDto.cat = "NoPhenotype" then
            result {
                return PhenotypeEval.NoPhenotypeEval
            }
        else if eDto.cat = "Sorter" then
            result {
                let floaterVal = (eDto.value) |> float
                return PhenotypeEval.Sorter SorterPhenotypeEval.Empty
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
        mapOfOrgAttributeMaps:Map<string, MapOfOrgAttributes<string>> option
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
                    mapOfOrgAttributeMaps = dto.mapOfOrgAttributeMaps
                }
            }
    
    let toDto (orgs:Orgs) =
        {
            OrgsDto.id = OrgsId.value orgs.id
            OrgsDto.orgs = orgs.orgMap
                            |> Map.toArray
                            |> Array.map(fun tup-> snd tup)
                            |> Array.map(OrgDto.toDto)

            OrgsDto.mapOfOrgAttributeMaps = None
        }
            
         
//type OrgsEnviromentTypeDto = 
//    {
//        category:string;
//        details:string
//    }


//module OrgsEnviromentTypeDto =
//    let toDto (orgsEnviromentType:OrgsEnvirotType) =
//        match orgsEnviromentType with
//        | GenerationSnapshot str -> 
//                {
//                    OrgsEnviromentTypeDto.category = "GenerationSnapshot";
//                    details = str
//                }

//    let fromDto (orgsEnviromentTypeDto:OrgsEnviromentTypeDto) =
//        match orgsEnviromentTypeDto.category with
//        | "GenerationSnapshot" -> OrgsEnvirotType.GenerationSnapshot 
//                                    orgsEnviromentTypeDto.details |> Ok
//        | cat -> sprintf "category %s not handled" cat |> Error



//type OrgsEnviromentDto = 
//    {
//        id:Guid
//        orgsDto:OrgsDto
//        mapOfOrgAttributeMaps:Map<string, MapOfOrgAttributes<string>> option
//    }






















//module OrgsEnviromentDto =

//    let fromDto (dto:OrgsWithGridLocsDto) =
//        result 
//            {
//                let! orgList = dto.orgs
//                                |> Array.map(OrgDto.fromDto)
//                                |> Array.toList |> Result.sequence

//                let orgMap = orgList |> List.map(fun gl-> (gl.orgId, gl))
//                                     |> Map.ofList
//                let! poolOfGridLocations =  dto.poolOfGridLocationsDto |> PoolOfGridLocationsDto.fromDto

//                return {
//                    OrgsWithGridLocs.id = dto.id
//                    OrgsWithGridLocs.orgMap = orgMap
//                    OrgsWithGridLocs.poolOfGridLocations = poolOfGridLocations
//                }
//            }

//    let toDto (orgs:OrgsWithGridLocs) =
//        {
//            OrgsWithGridLocsDto.id = orgs.id
//            OrgsWithGridLocsDto.orgs = orgs.orgMap
//                                        |> Map.toArray
//                                        |> Array.map(fun tup-> snd tup)
//                                        |> Array.map(OrgDto.toDto)
//            OrgsWithGridLocsDto.poolOfGridLocationsDto = orgs.poolOfGridLocations 
//                                        |> PoolOfGridLocationsDto.toDto
//        }
        

    //type OrgsDto = 
    //    {
    //        id:Guid
    //        orgs:OrgDto[]
    //    }
        
    //module OrgsDto =
        
    //    let fromDto (dto:OrgsDto) =
    //        result 
    //            {
    //                let! orgList = dto.orgs
    //                                |> Array.map(OrgDto.fromDto)
    //                                |> Array.toList |> Result.sequence
        
    //                let orgMap = orgList |> List.map(fun gl-> (gl.orgId, gl))
    //                                        |> Map.ofList
        
    //                return {
    //                    Orgs.id = dto.id
    //                    Orgs.orgMap = orgMap
    //                }
    //            }
        
    //    let toDto (orgs:Orgs) =
    //        {
    //            OrgsDto.id = orgs.id
    //            OrgsDto.orgs = orgs.orgMap
    //                            |> Map.toArray
    //                            |> Array.map(fun tup-> snd tup)
    //                            |> Array.map(OrgDto.toDto)
    //        }
    



//type OrgPoolEnviroDto = {cat:string; value:string}
//module OrgPoolEnviroDto =
//    let toDto (env:OrgPoolEnviro) =
//         match env with
//         | OrgPoolEnviro.Bag ob -> {cat="Bag"; value = ob |> OrgsDto.toDto  |> Json.serialize}
//         | OrgPoolEnviro.Grid og -> {cat="Grid"; value = og |> OrgsWithGridLocsDto.toDto  |> Json.serialize}


//    let fromDto (eDto:OrgPoolEnviroDto) =
//        if eDto.cat = "Bag" then
//            result {
//                let! dto = Json.deserialize<OrgsDto> eDto.value
//                let! ob = dto |> OrgsDto.fromDto
//                return OrgPoolEnviro.Bag ob
//            }
//        else if eDto.cat = "Grid" then
//            result {
//                let! dto = Json.deserialize<OrgsWithGridLocsDto> eDto.value
//                let! ogl = dto |> OrgsWithGridLocsDto.fromDto
//                return OrgPoolEnviro.Grid ogl
//            }
//        else sprintf "cat: %s for OrgPoolEnviroDto not found"
//                      eDto.cat |> Error
