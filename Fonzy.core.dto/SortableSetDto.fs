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
        if eDto.cat = nameof sortableSetType.AllForDegree then
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


//type sortableSetIntsDto = {id:Guid; cat:string; degree:int; sortableIntArrays:int[][]}
//module SortableSetIntsDto =
//    let yab = None
    
//    let toDtoBinary (ssBinary:sortableSetBinary) =
//        {
//            sortableSetIntsDto.id = 
//                           (SortableSetId.value ssBinary.id)
//            sortableSetIntsDto.cat = "binary"
//            sortableSetIntsDto.degree = 
//                           (Degree.value ssBinary.degree)
//            sortableSetIntsDto.sortableIntArrays = 
//                            ssBinary.sortables
//                            |> Array.map(fun avs -> avs.values)
//        }

//    let toDtoInteger (ssInteger:sortableSetInteger) =
//        {
//            sortableSetIntsDto.id = 
//                           (SortableSetId.value ssInteger.id)
//            sortableSetIntsDto.cat = "integer"
//            sortableSetIntsDto.degree = 
//                           (Degree.value ssInteger.degree)
//            sortableSetIntsDto.sortableIntArrays = 
//                            ssInteger.sortables
//                            |> Array.map(fun s-> s.values)
//        }

//    let toJsonBinary (ssBinary:sortableSetBinary) =
//        ssBinary |> toDtoBinary |> Json.serialize

//    let toJsonInteger (ssInteger:sortableSetInteger) =
//        ssInteger |> toDtoInteger |> Json.serialize

//    let fromDto (dto:sortableSetIntsDto) =
//        if dto.cat = "binary" then
//            result {
//                let! id = SortableSetId.create dto.id
//                let! degree = Degree.create "" dto.degree
//                let sias = dto.sortableIntArrays 
//                           |> Array.map(fun avs -> {bitSet.values = avs})
//                return  {
//                            sortableSetBinary.id = id
//                            degree = degree
//                            sortables =sias
//                        }
//                        |> sortableSetO.Binary
//             }
//        else
//            result {
//                let! id = SortableSetId.create dto.id
//                let! degree = Degree.create "" dto.degree
//                return  {
//                            sortableSetInteger.id = id
//                            degree = degree
//                            sortables = dto.sortableIntArrays
//                                        |> Array.map(IntSet.create)
//                        }
//                        |> sortableSetO.Integer
//             }

//    let fromJson (cereal:string) =
//        result {
//            let! dto = cereal |> Json.deserialize<sortableSetIntsDto>
//            return! dto |> fromDto
//        }


//type sortableSetBpDto = {id:Guid; degree:int; sortableUintArrays:uint64[][]}
//module SortableSetBpDto =

//    let toDto (ssBp64:sortableSetBp64) =
//        {
//            sortableSetBpDto.id = 
//                           (SortableSetId.value ssBp64.id)
//            degree = (Degree.value ssBp64.degree)
//            sortableUintArrays = 
//                            ssBp64.sortables
//                            |> Array.map(fun avs -> avs.values)
//        }

//    let toJson (ssBp64:sortableSetBp64) =
//        ssBp64 |> toDto |> Json.serialize

//    let fromDto (dto:sortableSetBpDto) =
//        result {
//            let! id = SortableSetId.create dto.id
//            let! degree = Degree.create "" dto.degree
//            let sias = dto.sortableUintArrays 
//                       |> Array.map(fun avs -> {bitsP64.values = avs})
//            return  {
//                        sortableSetBp64.id = id
//                        degree = degree
//                        sortables = sias
//                    }
//                    |> sortableSetO.Bp64
//        }

//    let fromJson (cereal:string) =
//        result {
//            let! dto = cereal |> Json.deserialize<sortableSetBpDto>
//            return! dto |> fromDto
//        }


//type sortableSetDto = {cat:string; value:string}
//module SortableSetDto =
    
//    let toDto (sortableSet:sortableSetO) =
//         match sortableSet with
//         | Binary ssb -> { cat="Binary"; value = ssb |> SortableSetIntsDto.toJsonBinary }
//         | Integer ssi -> { cat="Integer"; value = ssi |> SortableSetIntsDto.toJsonInteger }
//         | Bp64 ssbp -> { cat="Bp64"; value = ssbp |> SortableSetBpDto.toJson }

//    let fromDto (eDto:sortableSetDto) =
//        if eDto.cat = "Binary" then
//            result {
//                return! eDto.value |> SortableSetIntsDto.fromJson
//            }
//        else if eDto.cat = "Integer" then
//            result {
//                return! eDto.value |> SortableSetIntsDto.fromJson
//            }
//        else if eDto.cat = "Bp64" then
//            result {
//                return! eDto.value |> SortableSetBpDto.fromJson
//            }
//        else sprintf "cat: %s for SortableSetDto not found"
//                      eDto.cat |> Error


//    let toJson (sortableSet:sortableSetO) =
//        sortableSet |> toDto |> Json.serialize


//    let fromJson (cereal:string) =
//        result {
//            let! dto = cereal |> Json.deserialize<sortableSetDto>
//            return! dto |> fromDto
//        }


//type sortableSetGenDto = {id:Guid; cat:string; prams:Map<string, string>}
//module SortableSetGenDto =
    
//    let toDto (ssGen:sortableSetGen) =
//        {
//            sortableSetGenDto.id = 
//                           (SortableSetId.value ssGen.id)
//            sortableSetGenDto.cat =  ssGen.cat
//            sortableSetGenDto.prams = ssGen.prams
//        }

//    let toJson (ssGen:sortableSetGen) =
//        ssGen |> toDto |> Json.serialize

//    let fromDto (dto:sortableSetGenDto) =
//        result {
//            let! id = SortableSetId.create dto.id
//            return  {
//                        sortableSetGen.id = id
//                        sortableSetGen.cat = dto.cat
//                        sortableSetGen.prams = dto.prams
//                    }
//        }

//    let fromJson (cereal:string) =
//        result {
//            let! dto = cereal |> Json.deserialize<sortableSetGenDto>
//            return! dto |> fromDto
//        }


//type sortableSetSpecDto0 = {cat:string; value:string;}
//module SortableSetSpecDto0 =

//    let toDto (spec:sortableSetSpec) =
//        match spec with
//        | sortableSetSpec.Explicit ss -> 
//                    {cat = nameof sortableSetSpec.Explicit; 
//                     value = ss |> SortableSetDto.toJson }
//        | sortableSetSpec.Generated g -> 
//                    {cat = nameof sortableSetSpec.Generated; 
//                     value = g |> SortableSetGenDto.toJson }


//    let toJson (sortableSetSpec:sortableSetSpec) =
//        sortableSetSpec |> toDto |> Json.serialize


//    let fromDto (dto:sortableSetSpecDto0) =
//            match dto.cat with
//            | nameof sortableSetSpec.Explicit -> 
//                    result {
//                            let! exp = dto.value |> SortableSetDto.fromJson
//                            return sortableSetSpec.Explicit exp
//                           }

//            | nameof sortableSetSpec.Generated -> 
//                    result {
//                            let! gen = dto.value |> SortableSetGenDto.fromJson
//                            return sortableSetSpec.Generated gen
//                           }
//            | _ -> Error (sprintf "no match for SortableSetDto.cat: %s" dto.cat)


//    let fromJson (cereal:string) =
//        result {
//            let! dto = cereal |> Json.deserialize<sortableSetSpecDto0>
//            return! dto |> fromDto
//        }


//type sortableSetSpecReducedDto = 
//        {ssSpec:sortableSetSpecDto0; swPfx:int[];}
//module SortableSetSpecReducedDto =

//    let toDto (spec:sortableSetSpecReduced) =
//        let sss, pfx = spec
//        {
//            sortableSetSpecReducedDto.ssSpec = sss |> SortableSetSpecDto0.toDto;
//            swPfx = pfx  |> Array.map(SwitchDto.toDto)
//        }

//    let toJson (spec:sortableSetSpecReduced) =
//        spec |> toDto |> Json.serialize

//    let fromDto (dto:sortableSetSpecReducedDto) =
//        result {
//            let! sss = dto.ssSpec |> SortableSetSpecDto0.fromDto
//            let! pfx = dto.swPfx |> Array.map(fun sw -> SwitchDto.fromDto sw)
//                                 |> Array.toList
//                                 |> Result.sequence
//            return (sss, pfx |> List.toArray) |> sortableSetSpecReduced
//        }

//    let fromJson (cereal:string) =
//        result {
//            let! dto = cereal |> Json.deserialize<sortableSetSpecReducedDto>
//            return! dto |> fromDto
//        }