namespace global
open System


type sortableSetIntsDto = {id:Guid; cat:string; degree:int; sortableIntArrays:int[][]}

module SortableSetIntsDto =
    let yab = None
    
    let toDtoBinary (ssBinary:sortableSetBinary) =
        {
            sortableSetIntsDto.id = 
                           (SortableSetId.value ssBinary.id)
            sortableSetIntsDto.cat = "binary"
            sortableSetIntsDto.degree = 
                           (Degree.value ssBinary.degree)
            sortableSetIntsDto.sortableIntArrays = 
                            ssBinary.sortables
                            |> Array.map(fun avs -> avs.values)
        }

    let toDtoInteger (ssInteger:sortableSetInteger) =
        {
            sortableSetIntsDto.id = 
                           (SortableSetId.value ssInteger.id)
            sortableSetIntsDto.cat = "integer"
            sortableSetIntsDto.degree = 
                           (Degree.value ssInteger.degree)
            sortableSetIntsDto.sortableIntArrays = 
                            ssInteger.sortables
        }

    let toJsonBinary (ssBinary:sortableSetBinary) =
        ssBinary |> toDtoBinary |> Json.serialize

    let toJsonInteger (ssInteger:sortableSetInteger) =
        ssInteger |> toDtoInteger |> Json.serialize

    let fromDto (dto:sortableSetIntsDto) =
        if dto.cat = "binary" then
            result {
                let! id = SortableSetId.create dto.id
                let! degree = Degree.create "" dto.degree
                let sias = dto.sortableIntArrays 
                           |> Array.map(fun avs -> {IntBits.values = avs})
                return  {
                            sortableSetBinary.id = id
                            degree = degree
                            sortables =sias
                        }
                        |> sortableSet.Binary
             }
        else
            result {
                let! id = SortableSetId.create dto.id
                let! degree = Degree.create "" dto.degree
                return  {
                            sortableSetInteger.id = id
                            degree = degree
                            sortables = dto.sortableIntArrays
                        }
                        |> sortableSet.Integer
             }

    let fromJson (cereal:string) =
        result {
            let! dto = cereal |> Json.deserialize<sortableSetIntsDto>
            return! dto |> fromDto
        }


type sortableSetBpDto = {id:Guid; degree:int; sortableUintArrays:uint64[][]}

module SortableSetBpDto =

    let toDto (ssBp64:sortableSetBp64) =
        {
            sortableSetBpDto.id = 
                           (SortableSetId.value ssBp64.id)
            degree = (Degree.value ssBp64.degree)
            sortableUintArrays = 
                            ssBp64.sortables
                            |> Array.map(fun avs -> avs.values)
        }

    let toJson (ssBp64:sortableSetBp64) =
        ssBp64 |> toDto |> Json.serialize

    let fromDto (dto:sortableSetBpDto) =
        result {
            let! id = SortableSetId.create dto.id
            let! degree = Degree.create "" dto.degree
            let sias = dto.sortableUintArrays 
                       |> Array.map(fun avs -> {bitsP64.values = avs})
            return  {
                        sortableSetBp64.id = id
                        degree = degree
                        sortables = sias
                    }
                    |> sortableSet.Bp64
        }

    let fromJson (cereal:string) =
        result {
            let! dto = cereal |> Json.deserialize<sortableSetBpDto>
            return! dto |> fromDto
        }


type sortableSetDto = {cat:string; value:string}

module SortableSetDto =
    
    let toDto (sortableSet:sortableSet) =
         match sortableSet with
         | Binary ssb -> { cat="Binary"; value = ssb |> SortableSetIntsDto.toJsonBinary }
         | Integer ssi -> { cat="Integer"; value = ssi |> SortableSetIntsDto.toJsonInteger }
         | Bp64 ssbp -> { cat="Bp64"; value = ssbp |> SortableSetBpDto.toJson }

    let fromDto (eDto:sortableSetDto) =
        if eDto.cat = "Binary" then
            result {
                return! eDto.value |> SortableSetIntsDto.fromJson
            }
        else if eDto.cat = "Integer" then
            result {
                return! eDto.value |> SortableSetIntsDto.fromJson
            }
        else if eDto.cat = "Bp64" then
            result {
                return! eDto.value |> SortableSetBpDto.fromJson
            }
        else sprintf "cat: %s for SortableSetDto not found"
                      eDto.cat |> Error


    let toJson (sortableSet:sortableSet) =
        sortableSet |> toDto |> Json.serialize


    let fromJson (cereal:string) =
        result {
            let! dto = cereal |> Json.deserialize<sortableSetDto>
            return! dto |> fromDto
        }



type sortableSetGenDto = {id:Guid; cat:string; prams:Map<string, string>}

module SortableSetGeneratedDto =
    
    let toDto (ssGen:sortableSetGen) =
        {
            sortableSetGenDto.id = 
                           (SortableSetId.value ssGen.id)
            sortableSetGenDto.cat =  ssGen.cat
            sortableSetGenDto.prams = ssGen.prams
        }

    let toJson (ssGen:sortableSetGen) =
        ssGen |> toDto |> Json.serialize

    let fromDto (dto:sortableSetGenDto) =
        result {
            let! id = SortableSetId.create dto.id
            return  {
                        sortableSetGen.id = id
                        sortableSetGen.cat = dto.cat
                        sortableSetGen.prams = dto.prams
                    }
        }

    let fromJson (cereal:string) =
        result {
            let! dto = cereal |> Json.deserialize<sortableSetGenDto>
            return! dto |> fromDto
        }

type sortableSetSpecDto = {cat:string; value:string;}

module SortableSetSpecDto =

    let toDto (sortableSetSpec:sortableSetSpec) =
        match sortableSetSpec with
        | sortableSetSpec.Explicit ss -> 
                    {cat="Explicit"; 
                     value = ss |> SortableSetDto.toJson }
        | sortableSetSpec.Generated g -> 
                    {cat="Generated"; 
                     value = g |> SortableSetGeneratedDto.toJson }


    let toJson (sortableSetSpec:sortableSetSpec) =
        sortableSetSpec |> toDto |> Json.serialize


    let fromDto (dto:sortableSetSpecDto) =
            match dto.cat with
            | "Explicit" -> 
                    result {
                            let! exp = dto.value |> SortableSetDto.fromJson
                            return sortableSetSpec.Explicit exp
                           }

            | "Generated" -> 
                    result {
                            let! gen = dto.value |> SortableSetGeneratedDto.fromJson
                            return sortableSetSpec.Generated gen
                           }
            | _ -> Error (sprintf "no match for SortableSetDto.cat: %s" dto.cat)


    let fromJson (cereal:string) =
        result {
            let! dto = cereal |> Json.deserialize<sortableSetSpecDto>
            return! dto |> fromDto
        }