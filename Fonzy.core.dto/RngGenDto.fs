namespace global

type rngGenDto = {rngType:string; seed:int}
module RngGenDto =
    let fromDto (dto:rngGenDto) =
        result {
            let! typ = RngType.create dto.rngType
            let! rs = RandomSeed.create "" dto.seed
            return {RngGen.rngType=typ; seed=rs}
        }

    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<rngGenDto> jstr
            return! fromDto dto
        }

    let toDto (rngGen:RngGen) =
        {rngType=(RngType.toDto rngGen.rngType); 
         seed=RandomSeed.value rngGen.seed}

    let toJson (rngGen:RngGen) =
        rngGen |> toDto |> Json.serialize