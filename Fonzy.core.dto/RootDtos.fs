namespace global
open System


type RootDto = {dataType:string; metaData:Map<string,string>; data:string}

module RootDto = 
    let extractData<'T> (rootDto:RootDto) =
        result {
            let! dto = Json.deserialize<'T>(rootDto.data)
            return dto, rootDto.metaData
        }

    let extractFromJson<'T> (json:string) =
        result {
            let! dto = json |> Json.deserialize<RootDto>
            return! extractData<'T> dto
        }

    let toDto<'T> (dataDto:'T) (metaData:Map<string,string>) =
        let dataStr = Json.serialize dataDto
        {
            RootDto.dataType = typeof<'T>.ToString();
            RootDto.metaData = metaData;
            RootDto.data = dataStr
        }

    let toJson<'T> (dataDto:'T) (metaData:Map<string,string>) =
        let rootDto = toDto dataDto metaData
        rootDto |> Json.serialize
