namespace global

type RngGenDto = {rngType:string; seed:int}
module RngGenDto =
    let fromDto (dto:RngGenDto) =
        result {
            let! typ = RngType.create dto.rngType
            let! rs = RandomSeed.create "" dto.seed
            return {RngGen.rngType=typ; seed=rs}
        }
    let toDto (rngGen:RngGen) =
        {rngType=(RngType.toDto rngGen.rngType); 
         seed=RandomSeed.value rngGen.seed}

    let toJson (rngGen:RngGen) =
        rngGen |> toDto |> Json.serialize

