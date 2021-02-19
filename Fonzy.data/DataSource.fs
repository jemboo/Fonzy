namespace global
open System
open System.IO


type IDataSource =
   abstract member GetDataSource: Guid -> Result<DataStoreItem, string>
   abstract member GetDataSourceIds: Unit -> Result<Guid[], string>
   abstract member AddNewDataStoreItem : DataStoreItem -> Result<string, string>

type DirectoryDataSource(dirPath:string) =
    member this.DirectoryPath = dirPath
    member this.GuidToFilePath (id:Guid) =
        Path.Combine(this.DirectoryPath, (string id) + ".txt")

    interface IDataSource with
        member this.GetDataSource (id:Guid) =
            let filePath = this.GuidToFilePath id
            result {
                let! js = FileUtils.readFile filePath
                let! dsi = js |> DataStoreItemDto.fromJson
                return dsi
            }

        member this.GetDataSourceIds() =
            result {
                let! files = FileUtils.getFilesInDirectory this.DirectoryPath "*.txt"
                return  files |> Array.map(Path.GetFileNameWithoutExtension)
                              |> Array.map(GuidUtils.guidFromStringO)
                              |> Array.filter(Option.isSome)
                              |> Array.map(Option.get)
            }

        member this.AddNewDataStoreItem (dsi:DataStoreItem) =
            let fp = dsi |> DataStoreItem.getId |> this.GuidToFilePath
            FileUtils.writeFile fp (dsi |> Json.serialize) false