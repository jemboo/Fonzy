namespace global
open System
open System.IO


type IDataSource =
   abstract member GetDataSource: Guid -> Result<DataStoreItem, string>
   abstract member GetDataSourceIds: Unit -> Result<Guid[], string>
   abstract member AddNewDataStoreItem : DataStoreItem -> Result<bool, string>

type DirectoryDataSource(dirPath:string) =
    member this.DirectoryPath = dirPath
    member this.GuidToFilePath (id:Guid) =
        Path.Combine(this.DirectoryPath, (string id) + ".txt")
    member this.AssureDirectory = 
            FileUtils.makeDirectory this.DirectoryPath

    interface IDataSource with

        member this.GetDataSource (id:Guid) =
            let filePath = this.GuidToFilePath id
            result {
                let! assure = this.AssureDirectory
                let! js = FileUtils.readFile filePath
                let! dsi = js |> DataStoreItemDto.fromJson
                return dsi
            }

        member this.GetDataSourceIds() =
            result {
                let! assure = this.AssureDirectory
                let! files = FileUtils.getFilesInDirectory this.DirectoryPath "*.txt"
                return  files |> Array.map(Path.GetFileNameWithoutExtension)
                              |> Array.map(GuidUtils.guidFromStringO)
                              |> Array.filter(Option.isSome)
                              |> Array.map(Option.get)
            }

        member this.AddNewDataStoreItem (dsi:DataStoreItem) =
            result {
                let! assure = this.AssureDirectory
                let fp = dsi |> DataStoreItem.getId |> this.GuidToFilePath
                let cereal = dsi |> DataStoreItemDto.toDto
                                 |> Json.serialize
                return! FileUtils.writeFile fp cereal false
            }