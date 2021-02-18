namespace global


type DataStoreItem =
      | WorldDto of WorldDto
      | WorldActionDto of WorldActionDto

module DataStoreItem =

    let getId (dsi:DataStoreItem) =
        match dsi with
        | DataStoreItem.WorldDto dto -> dto.id
        | DataStoreItem.WorldActionDto dto -> dto.childId

    let getWorldDto (dsi:DataStoreItem) =
        match dsi with
        | WorldDto w -> w |> Ok
        | WorldActionDto _ -> "WorldActionDto is not a WorldDto" |> Error


    let getWorldActionDto (dsi:DataStoreItem) =
        match dsi with
        | WorldActionDto wa -> wa |> Ok
        | WorldDto _ -> "WorldDto is not a WorldActionDto" |> Error



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


    let toJson (idt:DataStoreItem) =
        idt |> toDto |> Json.serialize


    let storeWorld (w:World) = 
            w |> WorldDto.toDto 
              |> DataStoreItem.WorldDto 
              |> toJson

    let storeWorldAction (wa:WorldAction) = 
            wa |> WorldActionDto.toDto 
              |> DataStoreItem.WorldActionDto 
              |> toJson

    let fromDto (eDto:DataStoreItemDto) =
        if eDto.cat = "WorldDto" then
            result {
                let! b = Json.deserialize<WorldDto> eDto.value
                return DataStoreItem.WorldDto b
            }
        else if eDto.cat = "WorldActionDto" then
            result {
                let! b = Json.deserialize<WorldActionDto> eDto.value
                return DataStoreItem.WorldActionDto b
            }
        else sprintf "cat: %s for DataStoreItemDto not found"
                      eDto.cat |> Error

    let fromJson (js:string) =
        result {
            let! dto = Json.deserialize<DataStoreItemDto> js
            return! fromDto dto
        }