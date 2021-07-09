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
        | SorterGenome.Permutaions ps ->
            let dsf = ps |> List.toArray 
                         |> Array.map(fun p-> p |> TwoCyclePermDto.toDto
                                                |> Json.serialize)
            {
                sorterGenomeDto.cat = "Permutaions"; 
                value = dsf |> Json.serialize       
            }
        | SorterGenome.Switches sw ->
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
                let! dto = Json.deserialize<sorterPhenotypeDto> cereal
                return! fromDto dto
            }

    let toDto (sorterPhenotype:SorterPhenotype) =
        match sorterPhenotype with
        | SorterPhenotype.Singleton s ->
            {
                sorterPhenotypeDto.cat = "Singleton"; 
                value = s |> SorterDto.toDto |> Json.serialize
            }
        | SorterPhenotype.Multiple sList ->
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
                let! dto = Json.deserialize<sorterTestResultsDto> cereal
                return! fromDto dto
            }

    let toDto (sorterPhenotype:SorterTestResults) =
        match sorterPhenotype with
        | SorterTestResults.Singleton s ->
            {
                sorterTestResultsDto.cat = "Singleton";
                value = s |> SwitchUsesDto.toDto |> Json.serialize
            }
        | SorterTestResults.Multiple sList ->
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
                return SorterPhenotypeEval.Singleton eval
            }
        else if dto.cat = "Multiple" then
            result {
                let! b = Json.deserialize<float[]> dto.value
                return SorterPhenotypeEval.Multiple (b |> Array.toList)
            }
        else sprintf "cat: %s for SorterTestResultsDto not found"
                        dto.cat |> Error
    let fromJson (cereal:string) =
            result {
                let! dto = Json.deserialize<sorterPhenotypeEvalDto> cereal
                return! fromDto dto
            }

    let toDto (sorterPhenotype:SorterPhenotypeEval) =
        match sorterPhenotype with
        | SorterPhenotypeEval.Singleton s ->
            {
                sorterPhenotypeEvalDto.cat = "Singleton";
                value = s.ToString()
            }
        | SorterPhenotypeEval.Multiple sList ->
            {
                sorterPhenotypeEvalDto.cat = "Multiple";
                value = sList |> List.toArray 
                              |> Json.serialize
            }
