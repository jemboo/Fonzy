namespace global
open System

type SortableSetExplicitDto = {id:Guid; degree:int; sortableIntArrays:int[][]}

module SortableSetExplicitDto =
    
    let toDto (sortableSetExplicit:SortableSetBinary) =
        {
            SortableSetExplicitDto.id = 
                           (SortableSetId.value sortableSetExplicit.id)
            SortableSetExplicitDto.degree = 
                           (Degree.value sortableSetExplicit.degree)
            SortableSetExplicitDto.sortableIntArrays = 
                            sortableSetExplicit.sortables
                            |> Array.map(fun avs -> avs.values)
        }

    let toJson (sortableSetExplicit:SortableSetBinary) =
        sortableSetExplicit |> toDto |> Json.serialize

    let fromDto (dto:SortableSetExplicitDto) =
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
        }

    let fromJson (cereal:string) =
        result {
            let! dto = cereal |> Json.deserialize<SortableSetExplicitDto>
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
                        SortableSetGenerated.prams =dto.prams
                    }
        }

    let fromJson (cereal:string) =
        result {
            let! dto = cereal |> Json.deserialize<SortableSetGeneratedDto>
            return! dto |> fromDto
        }

type SortableSetDto = {cat:string; value:string;}

module SortableSetDto =
    
    let toDto (sortableSet:SortableSetSpec) =
        match sortableSet with
        | SortableSetSpec.Explicit e -> 
                    {cat="Explicit"; 
                     value = e |> SortableSetExplicitDto.toJson }
        | SortableSetSpec.Generated g -> 
                    {cat="Generated"; 
                     value = g |> SortableSetGeneratedDto.toJson }

    let toJson (sorterLength:SortableSetSpec) =
        sorterLength |> toDto |> Json.serialize

    let fromDto (dto:SortableSetDto) =
            match dto.cat with
            | "Explicit" -> 
                    result {
                            let! exp = dto.value |> SortableSetExplicitDto.fromJson
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
            let! dto = cereal |> Json.deserialize<SortableSetDto>
            return! dto |> fromDto
        }