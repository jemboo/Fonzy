namespace global
open System
open System.IO


type IDataSource =
   abstract member GetDataSource: Guid -> Result<string, string>
   abstract member GetDataSourceIds: Unit -> Result<Guid[], string>
   abstract member AddNewWorld : Guid -> string -> Result<Guid, string>

type DirectoryDataSource(dirPath:string) =
    member this.DirectoryPath = dirPath

    interface IDataSource with
        member this.GetDataSource (id:Guid) =
            let filePath = Path.Combine(this.DirectoryPath, (string id), ".txt")
            result {
                let! js = FileUtils.readFile filePath
                return js
            }

        member this.GetDataSourceIds() =
            result {
                let! files = FileUtils.getFilesInDirectory this.DirectoryPath "*.txt"
                let gstra =  files |> Array.map(Path.GetFileNameWithoutExtension)
                                   |> Array.map(GuidUtils.guidFromStringO)
                                   |> Array.filter(Option.isSome)
                                   |> Array.map(Option.get)
                return gstra
            }

        member this.AddNewWorld g s =
             g |> Ok