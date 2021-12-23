namespace global
open System


type sorterSetGenDto = {cat:string; value:string}

module SorterSetGenDto =
    let fromDto dto =
        if dto.cat = nameof sorterSetGen.Mutate then
            result {
                let! aa = Json.deserialize<string[]> dto.value
                let! smt = aa.[0] |> SorterMutTypeDto.fromJson
                let! srtr = aa.[1] |> SorterDto.fromJson
                let! rng = aa.[2] |> RngGenDto.fromJson
                let! scv = aa.[3] |> Json.deserialize<int>
                let! sc = scv |> SorterCount.create ""
                return sorterSetGen.Mutate (smt,srtr,rng,sc)
            }
        else if dto.cat = nameof sorterSetGen.Rnd  then
            result {
                let! aa = Json.deserialize<string[]> dto.value
                let! srg = aa.[0] |> SorterRndGenDto.fromJson
                let! rng = aa.[1] |> RngGenDto.fromJson
                let! scv = aa.[2] |> Json.deserialize<int>
                let! sc = scv |> SorterCount.create ""
                return sorterSetGen.Rnd (srg,rng,sc)
            }
        else if dto.cat = nameof sorterSetGen.Repo  then
            result {
                let! guID = dto.value |> Json.deserialize
                let! ssID = guID |> SorterSetId.create
                return sorterSetGen.Repo ssID
            }

        else sprintf "cat: %s for SorterSetGenDto not found"
                        dto.cat |> Error

    let fromJson (jstr:string) =
        result {
            let! dto = Json.deserialize<sorterSetGenDto> jstr
            return! fromDto dto
        }



    let toDto (ssg:sorterSetGen) =
        match ssg with
        | sorterSetGen.Mutate (smt, srtr, rng, sc) ->
            let cereal = [| smt |> SorterMutTypeDto.toJson;
                            srtr |> SorterDto.toJson;
                            rng |> RngGenDto.toJson;
                            sc |> SorterCount.value |> string|]
            {
                sorterSetGenDto.cat = nameof sorterSetGen.Mutate; 
                value = cereal |> Json.serialize       
            }
        | sorterSetGen.Rnd (srg, rng, sc) ->
            let cereal = [| srg |> SorterRndGenDto.toJson;
                            rng |> RngGenDto.toJson;
                            sc |> SorterCount.value |> string|]
            {
                sorterSetGenDto.cat = nameof sorterSetGen.Rnd;
                value = cereal |> Json.serialize 
            }
        | sorterSetGen.Repo ssid ->
            {
                sorterSetGenDto.cat = nameof sorterSetGen.Repo; 
                value = ssid |> SorterSetId.value |> Json.serialize 
            }

    let toJson (idt:sorterSetGen) =
        idt |> toDto |> Json.serialize