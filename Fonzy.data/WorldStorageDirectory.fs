namespace global
open System
open System.IO


type IWorldStorage =
   abstract member GetDataSource: Guid -> Result<WorldStorage, string>
   abstract member GetDataSourceIds: Unit -> Result<Guid[], string>
   abstract member AddNewDataStoreItem : WorldStorage -> Result<bool, string>

type WorldStorageDirectory(dirPath:FileDir) =
    member this.DirectoryPath = dirPath
    member this.GuidToFilePath (id:Guid) =
        let fn =  FileName.fromString ((string id))
        FilePath.appendFileName this.DirectoryPath fn (FileExt.fromString ".txt")
           
    member this.AssureDirectory = 
            FileUtils.makeDirectory this.DirectoryPath

    interface IWorldStorage with

        member this.GetDataSource (id:Guid) =
            result {
                let! fp = this.GuidToFilePath id
                let! assure = this.AssureDirectory
                let! js = FileUtils.readFile fp
                let! dsi = js |> WorldStorageDto.fromJson
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

        member this.AddNewDataStoreItem (dsi:WorldStorage) =
            result {
                let! assure = this.AssureDirectory
                let! fp = dsi |> WorldStorage.getId |> this.GuidToFilePath
                let cereal = dsi |> WorldStorageDto.toDto
                                 |> Json.serialize
                return! FileUtils.makeFile fp cereal
            }