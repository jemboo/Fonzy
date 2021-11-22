namespace global


type WorldStorage =
      | WorldDto of worldDto
      | WorldActionDto of worldActionDto
      | WorldMergeDto of worldMergeDto

module WorldStorage =

    let getId (dsi:WorldStorage) =
        match dsi with
        | WorldStorage.WorldDto dto -> dto.id
        | WorldStorage.WorldActionDto dto -> dto.childId
        | WorldStorage.WorldMergeDto dto -> dto.id

    let getWorldDto (dsi:WorldStorage) =
        match dsi with
        | WorldDto w -> w |> Ok
        | WorldActionDto _ -> "WorldActionDto is not a WorldDto" |> Error
        | WorldMergeDto _ -> "WorldMergeDto is not a WorldDto" |> Error


    let getWorldActionDto (dsi:WorldStorage) =
        match dsi with
        | WorldActionDto wa -> wa |> Ok
        | WorldDto _ -> "WorldDto is not a WorldActionDto" |> Error
        | WorldMergeDto _ -> "WorldMergeDto is not a WorldActionDto" |> Error



type WorldStorageDto = {cat:string; value:string}

module WorldStorageDto =

    let toDto (dsi:WorldStorage) =
        match dsi with
        | WorldStorage.WorldDto wDto -> {
                        cat="WorldDto"; 
                        value = Json.serialize wDto}

        | WorldStorage.WorldActionDto waDto -> {
                        cat = "WorldActionDto"; 
                        value = Json.serialize waDto}

        | WorldStorage.WorldMergeDto waDto -> {
                        cat = "WorldMergeDto"; 
                        value = Json.serialize waDto}



    let toJson (idt:WorldStorage) =
        idt |> toDto |> Json.serialize


    let storeWorld (w:world) = 
            let dto = w |> WorldDto.toDto 
            WorldStorage.WorldDto dto |> toDto

    let storeWorldAction (wa:WorldAction) = 
            wa |> WorldActionDto.toDto 
               |> WorldStorage.WorldActionDto 

    let storeWorldMerge (wa:WorldMerge) = 
            wa |> WorldMergeDto.toDto 
               |> WorldStorage.WorldMergeDto 



    let fromDto (eDto:WorldStorageDto) =
        if eDto.cat = "WorldDto" then
            result {
                let! b = Json.deserialize<worldDto> eDto.value
                return WorldStorage.WorldDto b
            }

        else if eDto.cat = "WorldActionDto" then
            result {
                let! b = Json.deserialize<worldActionDto> eDto.value
                return WorldStorage.WorldActionDto b
            }

        else if eDto.cat = "WorldMergeDto" then
            result {
                let! b = Json.deserialize<worldMergeDto> eDto.value
                return WorldStorage.WorldMergeDto b
                }

        else sprintf "cat: %s for DataStoreItemDto not found"
                      eDto.cat |> Error

    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<WorldStorageDto> js
            return! fromDto dto
        }