namespace global
open System

type SorterGenomeDto = {cat:string; value:string}
module SorterGenomeDto =
    let fromDto dto =
        if dto.cat = "Permutaions" then
            result {
                let! b = Json.deserialize<string[]> dto.value
                let! bs = b |> Array.map(TwoCyclePermDto.fromJson)
                            |> Array.toList
                            |> Result.sequence

                return SorterGenome.Permutaions bs
            }
        else if dto.cat = "Switches" then
            result {
                let! b = Json.deserialize<int[]> dto.value
                let! bs = b |> Array.map(SwitchDto.fromDto)
                            |> Array.toList
                            |> Result.sequence

                return SorterGenome.Switches bs
            }
        else sprintf "cat: %s for SorterGenomeDto not found"
                        dto.cat |> Error

    let toDto (sorterGenome:SorterGenome) =
        match sorterGenome with
        | Permutaions ps ->
            let dsf = ps |> List.toArray 
                         |> Array.map(fun p-> p |> TwoCyclePermDto.toDto
                                                |> Json.serialize)
            {
                SorterGenomeDto.cat = "Permutaions"; 
                value = dsf |> Json.serialize       
            }
        | Switches sw ->
            {
                SorterGenomeDto.cat = "Switches"; 
                value = sw |> List.toArray 
                           |> Array.map(SwitchDto.toDto)
                           |> Json.serialize
            }

type SorterPhenotypeDto = {cat:string; value:string}
module SorterPhenotypeDto =
    let fromDto dto =
        if dto.cat = "Singleton" then
            result {
                let! sorter =  SorterDto.fromJson dto.value
                return SorterPhenotype.Singleton sorter
            }
        else if dto.cat = "Multiple" then
            result {
                let! b = Json.deserialize<string[]> dto.value
                let! bs = b |> Array.map(SorterDto.fromJson)
                            |> Array.toList
                            |> Result.sequence
                return SorterPhenotype.Multiple bs
            }
        else sprintf "cat: %s for SorterPhenotypeDto not found"
                        dto.cat |> Error
    let fromJson (cereal:string) =
            result {
                let! dto = Json.deserialize<SorterPhenotypeDto> cereal
                return! fromDto dto
            }

    let toDto (sorterPhenotype:SorterPhenotype) =
        match sorterPhenotype with
        | SorterPhenotype.Singleton s ->
            {
                SorterPhenotypeDto.cat = "Singleton"; 
                value = s |> SorterDto.toDto |> Json.serialize
            }
        | SorterPhenotype.Multiple sList ->
            {
                SorterPhenotypeDto.cat = ""; 
                value = sList |> List.toArray 
                              |> Array.map(SorterDto.toJson)
                              |> Json.serialize
            }


type SorterTestResultsDto = {cat:string; value:string}
module SorterTestResultsDto =
    let fromDto (dto:SorterTestResultsDto) =
        if dto.cat = "Singleton" then
            result {
                let! switchUses =  SwitchUsesDto.fromJson dto.value
                return SorterTestResults.Singleton switchUses
            }
        else if dto.cat = "Multiple" then
            result {
                let! b = Json.deserialize<string[]> dto.value
                let! bs = b |> Array.map(SwitchUsesDto.fromJson)
                            |> Array.toList
                            |> Result.sequence
                return SorterTestResults.Multiple bs
            }
        else sprintf "cat: %s for SorterTestResultsDto not found"
                        dto.cat |> Error
    let fromJson (cereal:string) =
            result {
                let! dto = Json.deserialize<SorterTestResultsDto> cereal
                return! fromDto dto
            }

    let toDto (sorterPhenotype:SorterTestResults) =
        match sorterPhenotype with
        | SorterTestResults.Singleton s ->
            {
                SorterPhenotypeDto.cat = "Singleton"; 
                value = s |> SwitchUsesDto.toDto |> Json.serialize
            }
        | SorterTestResults.Multiple sList ->
            {
                SorterPhenotypeDto.cat = ""; 
                value = sList |> List.toArray 
                              |> Array.map(SwitchUsesDto.toJson)
                              |> Json.serialize
            }
