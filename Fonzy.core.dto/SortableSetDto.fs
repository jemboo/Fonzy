namespace global
open System


type SortableSetIntsDto = {id:Guid; cat:string; degree:int; sortableIntArrays:int[][]}

module SortableSetIntsDto =
    let yab = None
    
    let toDtoBinary (sortableSetBinary:SortableSetBinary) =
        {
            SortableSetIntsDto.id = 
                           (SortableSetId.value sortableSetBinary.id)
            SortableSetIntsDto.cat = "binary"
            SortableSetIntsDto.degree = 
                           (Degree.value sortableSetBinary.degree)
            SortableSetIntsDto.sortableIntArrays = 
                            sortableSetBinary.sortables
                            |> Array.map(fun avs -> avs.values)
        }

    let toDtoInteger (sortableSetBinary:SortableSetInteger) =
        {
            SortableSetIntsDto.id = 
                           (SortableSetId.value sortableSetBinary.id)
            SortableSetIntsDto.cat = "integer"
            SortableSetIntsDto.degree = 
                           (Degree.value sortableSetBinary.degree)
            SortableSetIntsDto.sortableIntArrays = 
                            sortableSetBinary.sortables
        }

    let toJsonBinary (sortableSetBinary:SortableSetBinary) =
        sortableSetBinary |> toDtoBinary |> Json.serialize

    let toJsonInteger (sortableSetInteger:SortableSetInteger) =
        sortableSetInteger |> toDtoInteger |> Json.serialize

    let fromDto (dto:SortableSetIntsDto) =
        if dto.cat = "binary" then
            result {
                let! id = SortableSetId.create dto.id
                let! degree = Degree.create "" dto.degree
                let sias = dto.sortableIntArrays 
                           |> Array.map(fun avs -> {IntBits.values = avs})
                return  {
                            SortableSetBinary.id = id
                            SortableSetBinary.degree = degree
                            SortableSetBinary.sortables =sias
                        }
                        |> sortableSet.Binary
             }
        else
            result {
                let! id = SortableSetId.create dto.id
                let! degree = Degree.create "" dto.degree
                return  {
                            SortableSetInteger.id = id
                            SortableSetInteger.degree = degree
                            SortableSetInteger.sortables = dto.sortableIntArrays
                        }
                        |> sortableSet.Integer
             }

    let fromJson (cereal:string) =
        result {
            let! dto = cereal |> Json.deserialize<SortableSetIntsDto>
            return! dto |> fromDto
        }


type SortableSetBpDto = {id:Guid; degree:int; sortableUintArrays:uint64[][]}

module SortableSetBpDto =

    let toDto (sortableSetBp64:SortableSetBp64) =
        {
            SortableSetBpDto.id = 
                           (SortableSetId.value sortableSetBp64.id)
            SortableSetBpDto.degree = 
                           (Degree.value sortableSetBp64.degree)
            SortableSetBpDto.sortableUintArrays = 
                            sortableSetBp64.sortables
                            |> Array.map(fun avs -> avs.values)
        }

    let toJson (sortableSetBp64:SortableSetBp64) =
        sortableSetBp64 |> toDto |> Json.serialize

    let fromDto (dto:SortableSetBpDto) =
        result {
            let! id = SortableSetId.create dto.id
            let! degree = Degree.create "" dto.degree
            let sias = dto.sortableUintArrays 
                       |> Array.map(fun avs -> {bitsP64.values = avs})
            return  {
                        SortableSetBp64.id = id
                        SortableSetBp64.degree = degree
                        SortableSetBp64.sortables = sias
                    }
                    |> sortableSet.Bp64
        }

    let fromJson (cereal:string) =
        result {
            let! dto = cereal |> Json.deserialize<SortableSetBpDto>
            return! dto |> fromDto
        }


type SortableSetDto = {cat:string; value:string}

module SortableSetDto =
    
    let toDto (sortableSet:sortableSet) =
         match sortableSet with
         | Binary ssb -> { cat="Binary"; value = ssb |> SortableSetIntsDto.toJsonBinary }
         | Integer ssi -> { cat="Integer"; value = ssi |> SortableSetIntsDto.toJsonInteger }
         | Bp64 ssbp -> { cat="Bp64"; value = ssbp |> SortableSetBpDto.toJson }

    let fromDto (eDto:SortableSetDto) =
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
            let! dto = cereal |> Json.deserialize<SortableSetDto>
            return! dto |> fromDto
        }



type SortableSetGeneratedDto = {id:Guid; cat:string; prams:Map<string, string>}

module SortableSetGeneratedDto =
    
    let toDto (sortableSetGenerated:SortableSetGenerated) =
        {
            SortableSetGeneratedDto.id = 
                           (SortableSetId.value sortableSetGenerated.id)
            SortableSetGeneratedDto.cat =  sortableSetGenerated.cat
            SortableSetGeneratedDto.prams = 
                            sortableSetGenerated.prams
        }

    let toJson (sortableSetGenerated:SortableSetGenerated) =
        sortableSetGenerated |> toDto |> Json.serialize

    let fromDto (dto:SortableSetGeneratedDto) =
        result {
            let! id = SortableSetId.create dto.id
            return  {
                        SortableSetGenerated.id = id
                        SortableSetGenerated.cat = dto.cat
                        SortableSetGenerated.prams = dto.prams
                    }
        }

    let fromJson (cereal:string) =
        result {
            let! dto = cereal |> Json.deserialize<SortableSetGeneratedDto>
            return! dto |> fromDto
        }

type SortableSetSpecDto = {cat:string; value:string;}

module SortableSetSpecDto =

    let toDto (sortableSetSpec:SortableSetSpec) =
        match sortableSetSpec with
        | SortableSetSpec.Explicit ss -> 
                    {cat="Explicit"; 
                     value = ss |> SortableSetDto.toJson }
        | SortableSetSpec.Generated g -> 
                    {cat="Generated"; 
                     value = g |> SortableSetGeneratedDto.toJson }


    let toJson (sortableSetSpec:SortableSetSpec) =
        sortableSetSpec |> toDto |> Json.serialize


    let fromDto (dto:SortableSetSpecDto) =
            match dto.cat with
            | "Explicit" -> 
                    result {
                            let! exp = dto.value |> SortableSetDto.fromJson
                            return SortableSetSpec.Explicit exp
                           }

            | "Generated" -> 
                    result {
                            let! gen = dto.value |> SortableSetGeneratedDto.fromJson
                            return SortableSetSpec.Generated gen
                           }
            | _ -> Error (sprintf "no match for SortableSetDto.cat: %s" dto.cat)


    let fromJson (cereal:string) =
        result {
            let! dto = cereal |> Json.deserialize<SortableSetSpecDto>
            return! dto |> fromDto
        }