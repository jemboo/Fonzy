namespace global
open System

type sorterGenomeDto = {cat:string; value:string}
module SorterGenomeDto =
    let fromDto dto =
        if dto.cat = "Permutaions" then
            result {
                let! b = Json.deserialize<string[]> dto.value
                let! bs = b |> Array.map(TwoCyclePermDto.fromJson)
                            |> Array.toList
                            |> Result.sequence

                return sorterGenome.Permutaions bs
            }
        else if dto.cat = "Switches" then
            result {
                let! b = Json.deserialize<int[]> dto.value
                let! bs = b |> Array.map(SwitchDto.fromDto)
                            |> Array.toList
                            |> Result.sequence

                return sorterGenome.Switches bs
            }
        else sprintf "cat: %s for SorterGenomeDto not found"
                        dto.cat |> Error

    let toDto (sorterGenome:sorterGenome) =
        match sorterGenome with
        | sorterGenome.Permutaions ps ->
            let dsf = ps |> List.toArray 
                         |> Array.map(fun p-> p |> TwoCyclePermDto.toDto
                                                |> Json.serialize)
            {
                sorterGenomeDto.cat = "Permutaions"; 
                value = dsf |> Json.serialize       
            }
        | sorterGenome.Switches sw ->
            {
                sorterGenomeDto.cat = "Switches"; 
                value = sw |> List.toArray 
                           |> Array.map(SwitchDto.toDto)
                           |> Json.serialize
            }

type sorterPhenotypeDto = {cat:string; value:string}
module SorterPhenotypeDto =
    let fromDto dto =
        if dto.cat = "Singleton" then
            result {
                let! sorter =  SorterDto.fromJson dto.value
                return sorterPhenotype.Singleton sorter
            }
        else if dto.cat = "Multiple" then
            result {
                let! b = Json.deserialize<string[]> dto.value
                let! bs = b |> Array.map(SorterDto.fromJson)
                            |> Array.toList
                            |> Result.sequence
                return sorterPhenotype.Multiple bs
            }
        else sprintf "cat: %s for SorterPhenotypeDto not found"
                        dto.cat |> Error
    let fromJson (cereal:string) =
            result {
                let! dto = Json.deserialize<sorterPhenotypeDto> cereal
                return! fromDto dto
            }

    let toDto (sorterPhenotype:sorterPhenotype) =
        match sorterPhenotype with
        | sorterPhenotype.Singleton s ->
            {
                sorterPhenotypeDto.cat = "Singleton"; 
                value = s |> SorterDto.toDto |> Json.serialize
            }
        | sorterPhenotype.Multiple sList ->
            {
                sorterPhenotypeDto.cat = "Multiple"; 
                value = sList |> List.toArray 
                              |> Array.map(SorterDto.toJson)
                              |> Json.serialize
            }


type sorterTestResultsDto = {cat:string; value:string}
module SorterTestResultsDto =
    let fromDto (dto:sorterTestResultsDto) =
        if dto.cat = "Singleton" then
            result {
                let! switchUses =  SwitchUsesDto.fromJson dto.value
                return sorterTestResults.Singleton switchUses
            }
        else if dto.cat = "Multiple" then
            result {
                let! b = Json.deserialize<string[]> dto.value
                let! bs = b |> Array.map(SwitchUsesDto.fromJson)
                            |> Array.toList
                            |> Result.sequence
                return sorterTestResults.Multiple bs
            }
        else sprintf "cat: %s for SorterTestResultsDto not found"
                        dto.cat |> Error
    let fromJson (cereal:string) =
            result {
                let! dto = Json.deserialize<sorterTestResultsDto> cereal
                return! fromDto dto
            }

    let toDto (sorterPhenotype:sorterTestResults) =
        match sorterPhenotype with
        | sorterTestResults.Singleton s ->
            {
                sorterTestResultsDto.cat = "Singleton";
                value = s |> SwitchUsesDto.toDto |> Json.serialize
            }
        | sorterTestResults.Multiple sList ->
            {
                sorterTestResultsDto.cat = "Multiple";
                value = sList |> List.toArray 
                              |> Array.map(SwitchUsesDto.toJson)
                              |> Json.serialize
            }


type sorterPhenotypeEvalDto = {cat:string; value:string}
module SorterPhenotypeEvalDto =
    let fromDto (dto:sorterPhenotypeEvalDto) =
        if dto.cat = "Singleton" then
            result {
                let eval = float dto.value
                return sorterPhenotypeEval.Singleton eval
            }
        else if dto.cat = "Multiple" then
            result {
                let! b = Json.deserialize<float[]> dto.value
                return sorterPhenotypeEval.Multiple (b |> Array.toList)
            }
        else sprintf "cat: %s for SorterTestResultsDto not found"
                        dto.cat |> Error
    let fromJson (cereal:string) =
            result {
                let! dto = Json.deserialize<sorterPhenotypeEvalDto> cereal
                return! fromDto dto
            }

    let toDto (sorterPhenotype:sorterPhenotypeEval) =
        match sorterPhenotype with
        | sorterPhenotypeEval.Singleton s ->
            {
                sorterPhenotypeEvalDto.cat = "Singleton";
                value = s.ToString()
            }
        | sorterPhenotypeEval.Multiple sList ->
            {
                sorterPhenotypeEvalDto.cat = "Multiple";
                value = sList |> List.toArray 
                              |> Json.serialize
            }
