namespace global
open System

type NumberOrgDto = 
    {
        numberOrgId:Guid
        numberOrgType:NumberOrgType
        parentId:Guid option
        generation:int
        floatPhenotype:float
        phenotypeEval:float
    }

module NumberOrgDto =

    let fromDto (dto:NumberOrgDto) =
        result 
            {
                let! numberOrgId = NumberOrgId.create dto.numberOrgId
                let numberOrgType = dto.numberOrgType
                let! parentId = match dto.parentId with
                                | Some guey -> NumberOrgId.create guey
                                               |> Result.map Some
                                | None -> None |> Ok
                let! generation = GenerationNumber.create "" dto.generation

                return {
                    NumberOrg.numberOrgId = numberOrgId
                    numberOrgType = numberOrgType
                    parentId = parentId
                    generation = generation
                    floatPhenotype = dto.floatPhenotype
                    phenotypeEval = dto.phenotypeEval
                }
            }


    let toDto (numberOrg:NumberOrg) =
        {
            NumberOrgDto.numberOrgId = NumberOrgId.value numberOrg.numberOrgId
            numberOrgType = numberOrg.numberOrgType
            parentId = match numberOrg.parentId with
                        | Some id -> Some (NumberOrgId.value id)
                        | None -> None
            generation = numberOrg.generation |> GenerationNumber.value
            floatPhenotype = numberOrg.floatPhenotype
            phenotypeEval = numberOrg.phenotypeEval
        }


