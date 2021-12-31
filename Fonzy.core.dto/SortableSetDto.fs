namespace global
open System

type sortableSetRepDto = {cat:string; value:string}
module SortableSetRepDto =
    let toDto (ssr:sortableSetRep) =
        match ssr with
        | sortableSetRep.Binary d -> 
            {sortableSetRepDto.cat = nameof sortableSetRep.Binary;
             value = (Degree.value d) |> string}
        | sortableSetRep.Integer d ->
            {sortableSetRepDto.cat = nameof sortableSetRep.Integer;
             value = (Degree.value d) |> string}
        | sortableSetRep.Bp64 d ->
            {sortableSetRepDto.cat = nameof sortableSetRep.Bp64;
             value = (Degree.value d) |> string}

    let toJson (ssr:sortableSetRep) =
        ssr |> toDto |> Json.serialize

    let fromDto (dto:sortableSetRepDto) =
        match dto.cat with
        | nameof sortableSetRep.Binary -> 
            result {
                let! dv = dto.value |> Json.deserialize<int>
                let! d = dv |> Degree.create ""
                return sortableSetRep.Binary d
            }
        | nameof sortableSetRep.Integer ->
            result {
                let! dv = dto.value |> Json.deserialize<int>
                let! d = dv |> Degree.create ""
                return sortableSetRep.Integer d
            }
        | nameof sortableSetRep.Bp64 ->
            result {
                let! dv = dto.value |> Json.deserialize<int>
                let! d = dv |> Degree.create ""
                return sortableSetRep.Bp64 d
            }
        | cc -> sprintf "cat: %s not found" cc |> Error

    let fromJson (cereal:string) =
        result {
            let! dto = cereal |> Json.deserialize<sortableSetRepDto>
            return! dto |> fromDto
        }


type sortableSetTypeDto = {cat:string; value:string}
module SortableSetTypeDto =
    
    let rec toDto (sst:sortableSetType) =
         match sst with
         | sortableSetType.BinaryMerge (degs, ssr) ->
            let cereal = [| degs |> Seq.map(Degree.value) |> Seq.toArray |> Json.serialize;
                            ssr |> SortableSetRepDto.toJson;|]
            { cat = nameof sortableSetType.BinaryMerge; 
              value = cereal |> Json.serialize }
         | sortableSetType.AllForDegree ssr ->
            { cat = nameof sortableSetType.AllForDegree; 
              value = ssr |> SortableSetRepDto.toJson }
         | sortableSetType.Explicit ssid ->
             { cat = nameof sortableSetType.Explicit; 
             value = ssid |> SortableSetId.value |> Json.serialize }
         | sortableSetType.Random (rng, sc, ssr) -> 
            let cereal = [| rng |> RngGenDto.toJson;
                            sc |> SortableCount.value |> string;
                            ssr |> SortableSetRepDto.toJson;|]
            { cat = nameof sortableSetType.Random; 
                value = cereal |> Json.serialize }
         | sortableSetType.SwitchReduced (sst, ws) -> 
            let cereal = [| sst |> toDto |> Json.serialize; 
                            ws |> List.map(SwitchDto.toDto) |> Json.serialize|]
            {   cat = nameof sortableSetType.SwitchReduced;  
                value = cereal |> Json.serialize }


    let rec fromDto (eDto:sortableSetTypeDto) =
        if eDto.cat = nameof sortableSetType.BinaryMerge then
            result {
                let! pcs = eDto.value |> Json.deserialize<string[]>
                let! degVs = pcs.[0] |> Json.deserialize<int[]>
                let degs = degVs |> Array.map(Degree.fromInt)
                                 |> Array.toList
                let! ssr = pcs.[1] |> SortableSetRepDto.fromJson
                return (degs, ssr) |> sortableSetType.BinaryMerge
            }
        else if eDto.cat = nameof sortableSetType.AllForDegree then
            result {
                let! ssr = eDto.value |> SortableSetRepDto.fromJson
                return ssr |> sortableSetType.AllForDegree
            }
        else if eDto.cat = nameof sortableSetType.Explicit then
            result {
                let! guID = eDto.value |> Json.deserialize
                let! ssID = guID |> SortableSetId.create
                return ssID |> sortableSetType.Explicit
            }
        else if eDto.cat = nameof sortableSetType.Random then
            result {
                let! pcs = eDto.value |> Json.deserialize<string[]>
                let! rng = pcs.[0] |> RngGenDto.fromJson
                let! sc = pcs.[1] |> int |> SortableCount.create ""
                let! ssr = pcs.[2] |> SortableSetRepDto.fromJson
                return (rng, sc, ssr) |> sortableSetType.Random
            }
        else if eDto.cat = nameof sortableSetType.SwitchReduced then
            result {
            let! pcs = eDto.value |> Json.deserialize<string[]>
            let! sstDto = pcs.[0] |> Json.deserialize<sortableSetTypeDto> 
            let! sst = sstDto |> fromDto
            let! swDtos = pcs.[1] |> Json.deserialize<int[]>
            let! switches = swDtos |> Array.map(fun sw -> SwitchDto.fromDto sw)
                                   |> Array.toList
                                   |> Result.sequence
            return (sst, switches) |> sortableSetType.SwitchReduced
            }
        else sprintf "cat: %s for %s not found"
                      eDto.cat (nameof eDto) |> Error


    let toJson (sortableSet:sortableSetType) =
        sortableSet |> toDto |> Json.serialize


    let fromJson (cereal:string) =
        result {
            let! dto = cereal |> Json.deserialize<sortableSetTypeDto>
            return! dto |> fromDto
        }



type sortableSetImplDto = {cat:string; value:string; degree:int}
module SortableSetImplDto =
    let toDto (ssr:sortableSetImpl) =
        match ssr with
        | sortableSetImpl.Binary (bs, d) -> 
            {sortableSetImplDto.cat = nameof sortableSetRep.Binary;
             value = bs |> Json.serialize;
             degree = (Degree.value d) }
        | sortableSetImpl.Integer (ts, d) ->
            {sortableSetImplDto.cat = nameof sortableSetRep.Integer;
             value = ts |> Json.serialize;
             degree = (Degree.value d) }
        | sortableSetImpl.Bp64 (bps, d) ->
            {sortableSetImplDto.cat = nameof sortableSetRep.Bp64;
             value = bps |> Json.serialize;
             degree = (Degree.value d) }

    let toJson (ssr:sortableSetImpl) =
        ssr |> toDto |> Json.serialize

    let fromDto (dto:sortableSetImplDto) =
        match dto.cat with
        | nameof sortableSetImpl.Binary -> 
            result {
                let! av = dto.value |> Json.deserialize<bitSet[]>
                let! d = dto.degree |> Degree.create ""
                return sortableSetImpl.Binary (av, d)
            }
        | nameof sortableSetImpl.Integer ->
            result {
                let! av = dto.value |> Json.deserialize<intSet[]>
                let! d = dto.degree |> Degree.create ""
                return sortableSetImpl.Integer (av, d)
            }
        | nameof sortableSetImpl.Bp64 ->
            result {
                let! av = dto.value |> Json.deserialize<bitsP64[]>
                let! d = dto.degree |> Degree.create ""
                return sortableSetImpl.Bp64 (av, d)
            }
        | cc -> sprintf "cat: %s not found" cc |> Error

    let fromJson (cereal:string) =
        result {
            let! dto = cereal |> Json.deserialize<sortableSetImplDto>
            return! dto |> fromDto
        }
