namespace global
open System


type sortableSetIntsDto = {id:Guid; cat:string; degree:int; sortableIntArrays:int[][]}

module SortableSetIntsDto =
    let yab = None
    
    let toDtoBinary (sortableSetBinary:SortableSetBinary) =
        {
            sortableSetIntsDto.id = 
                           (SortableSetId.value sortableSetBinary.id)
            sortableSetIntsDto.cat = "binary"
            sortableSetIntsDto.degree = 
                           (Degree.value sortableSetBinary.degree)
            sortableSetIntsDto.sortableIntArrays = 
                            sortableSetBinary.sortables
                            |> Array.map(fun avs -> avs.values)
        }

    let toDtoInteger (sortableSetBinary:SortableSetInteger) =
        {
            sortableSetIntsDto.id = 
                           (SortableSetId.value sortableSetBinary.id)
            sortableSetIntsDto.cat = "integer"
            sortableSetIntsDto.degree = 
                           (Degree.value sortableSetBinary.degree)
            sortableSetIntsDto.sortableIntArrays = 
                            sortableSetBinary.sortables
        }

    let toJsonBinary (sortableSetBinary:SortableSetBinary) =
        sortableSetBinary |> toDtoBinary |> Json.serialize

    let toJsonInteger (sortableSetInteger:SortableSetInteger) =
        sortableSetInteger |> toDtoInteger |> Json.serialize

    let fromDto (dto:sortableSetIntsDto) =
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
            let! dto = cereal |> Json.deserialize<sortableSetIntsDto>
            return! dto |> fromDto
        }


type sortableSetBpDto = {id:Guid; degree:int; sortableUintArrays:uint64[][]}

module SortableSetBpDto =

    let toDto (sortableSetBp64:SortableSetBp64) =
        {
            sortableSetBpDto.id = 
                           (SortableSetId.value sortableSetBp64.id)
            sortableSetBpDto.degree = 
                           (Degree.value sortableSetBp64.degree)
            sortableSetBpDto.sortableUintArrays = 
                            sortableSetBp64.sortables
                            |> Array.map(fun avs -> avs.values)
        }

    let toJson (sortableSetBp64:SortableSetBp64) =
        sortableSetBp64 |> toDto |> Json.serialize

    let fromDto (dto:sortableSetBpDto) =
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