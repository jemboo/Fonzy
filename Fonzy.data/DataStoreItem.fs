namespace global


type DataStoreItem =
      | WorldDto of worldDto
      | WorldActionDto of worldActionDto
      | WorldMergeDto of worldMergeDto

module DataStoreItem =

    let getId (dsi:DataStoreItem) =
        match dsi with
        | DataStoreItem.WorldDto dto -> dto.id
        | DataStoreItem.WorldActionDto dto -> dto.childId
        | DataStoreItem.WorldMergeDto dto -> dto.id

    let getWorldDto (dsi:DataStoreItem) =
        match dsi with
        | WorldDto w -> w |> Ok
        | WorldActionDto _ -> "WorldActionDto is not a WorldDto" |> Error
        | WorldMergeDto _ -> "WorldMergeDto is not a WorldDto" |> Error


    let getWorldActionDto (dsi:DataStoreItem) =
        match dsi with
        | WorldActionDto wa -> wa |> Ok
        | WorldDto _ -> "WorldDto is not a WorldActionDto" |> Error
        | WorldMergeDto _ -> "WorldMergeDto is not a WorldActionDto" |> Error



type DataStoreItemDto = {cat:string; value:string}

module DataStoreItemDto =

    let toDto (dsi:DataStoreItem) =
        match dsi with
        | DataStoreItem.WorldDto wDto -> {
                        cat="WorldDto"; 
                        value = Json.serialize wDto}

        | DataStoreItem.WorldActionDto waDto -> {
                        cat = "WorldActionDto"; 
                        value = Json.serialize waDto}

        | DataStoreItem.WorldMergeDto waDto -> {
                        cat = "WorldMergeDto"; 
                        value = Json.serialize waDto}



    let toJson (idt:DataStoreItem) =
        idt |> toDto |> Json.serialize


    let storeWorld (w:world) = 
            let dto = w |> WorldDto.toDto 
            DataStoreItem.WorldDto dto |> toDto

    let storeWorldAction (wa:WorldAction) = 
            wa |> WorldActionDto.toDto 
               |> DataStoreItem.WorldActionDto 

    let storeWorldMerge (wa:WorldMerge) = 
            wa |> WorldMergeDto.toDto 
               |> DataStoreItem.WorldMergeDto 



    let fromDto (eDto:DataStoreItemDto) =
        if eDto.cat = "WorldDto" then
            result {
                let! b = Json.deserialize<worldDto> eDto.value
                return DataStoreItem.WorldDto b
            }

        else if eDto.cat = "WorldActionDto" then
            result {
                let! b = Json.deserialize<worldActionDto> eDto.value
                return DataStoreItem.WorldActionDto b
            }

        else if eDto.cat = "WorldMergeDto" then
            result {
                let! b = Json.deserialize<worldMergeDto> eDto.value
                return DataStoreItem.WorldMergeDto b
                }

        else sprintf "cat: %s for DataStoreItemDto not found"
                      eDto.cat |> Error

    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<DataStoreItemDto> js
            return! fromDto dto
        }