namespace global
open System

type SortableSetExplicitDto = {id:Guid; degree:int; sortableIntArrays:int[][]}

module SortableSetExplicitDto =
    
    let toDto (sortableSetExplicit:SortableSetExplicit) =
        {
            SortableSetExplicitDto.id = 
                           (SortableSetId.value sortableSetExplicit.id)
            SortableSetExplicitDto.degree = 
                           (Degree.value sortableSetExplicit.degree)
            SortableSetExplicitDto.sortableIntArrays = 
                            sortableSetExplicit.sortableIntArrays
                            |> Array.map(SortableIntArray.value)
        }

    let toJson (sortableSetExplicit:SortableSetExplicit) =
        sortableSetExplicit |> toDto |> Json.serialize

    let fromDto (dto:SortableSetExplicitDto) =
        result {
            let! id = SortableSetId.create dto.id
            let! degree = Degree.create "" dto.degree
            let sias = dto.sortableIntArrays |> Array.map(SortableIntArray.create)
            return  {
                        SortableSetExplicit.id = id
                        SortableSetExplicit.degree = degree
                        SortableSetExplicit.sortableIntArrays =sias
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
    
    let toDto (sortableSet:SortableSet) =
        match sortableSet with
        | SortableSet.Explicit e -> 
                    {cat="Explicit"; 
                     value = e |> SortableSetExplicitDto.toJson }
        | SortableSet.Generated g -> 
                    {cat="Generated"; 
                     value = g |> SortableSetGeneratedDto.toJson }

    let toJson (sorterLength:SortableSet) =
        sorterLength |> toDto |> Json.serialize

    let fromDto (dto:SortableSetDto) =
            match dto.cat with
            | "Explicit" -> 
                    result {
                            let! exp = dto.value |> SortableSetExplicitDto.fromJson
                            return SortableSet.Explicit exp
                           }

            | "Generated" -> 
                    result {
                            let! gen = dto.value |> SortableSetGeneratedDto.fromJson
                            return SortableSet.Generated gen
                           }
            | _ -> Error (sprintf "no match for SortableSetDto.cat: %s" dto.cat)

    let fromJson (cereal:string) =
        result {
            let! dto = cereal |> Json.deserialize<SortableSetDto>
            return! dto |> fromDto
        }