namespace global
open System

namespace global

type fileDtoStream<'T> = { id:Guid; name:string; root:FileDir; meta:string array; reader:string->Result<'T, string>; writer:'T->string }

module FileDtoStream =
    
    let getFilePath (fdtos:fileDtoStream<'T>) =
        result {
            let! fpath = FilePath.appendFileName 
                                    fdtos.root 
                                    (fdtos.id |> string |> FileName.fromString)
                                    (".txt" |> FileExt.fromString)
            return fpath
        }

    let getCatalogFilePath (fdtos:fileDtoStream<'T>) =
        result {
            let! fpath = FilePath.appendFileName 
                                    fdtos.root 
                                    ("catalog"|> FileName.fromString)
                                    (".txt" |> FileExt.fromString)
            return fpath
        }

    let addToCatalog (fdtos:fileDtoStream<'T>) =
        result {
            let! di = FileUtils.makeDirectory fdtos.root
            let! fpath = getCatalogFilePath fdtos
            let line1 = seq { fdtos.meta |> Json.serialize }
            let! res = FileUtils.appendToFile fpath line1
            return fdtos
        }

    let initFile (fdtos:fileDtoStream<'T>) =
        result {
            let! di = FileUtils.makeDirectory fdtos.root
            let! fpath = getFilePath fdtos
            let line1 = seq { fdtos.meta |> Json.serialize }
            let! res = FileUtils.makeFileFromLines fpath line1
            return fdtos
        }

    let append<'T> (fdtos:fileDtoStream<'T>) (items:seq<'T>) =
        result {
            let! fpath = getFilePath fdtos
            let lines = items |> Seq.map(fdtos.writer)
            let! res = FileUtils.appendToFile fpath lines
            return res
        }


    let makeForSorterDto (id:Guid) (name:string) (root:FileDir) =
        result {
            let fdtos = 
               {
                     fileDtoStream.id = id;
                     name = name;
                     root = root;
                     meta = [|nameof sorterDto; name; id |> string|];
                     reader = SorterDto.fromJson;
                     writer = SorterDto.toJson
                }
            let! res = initFile fdtos
            return fdtos
        }

    let makeForSorterShcArchDto (id:Guid) (name:string) (root:FileDir) =
        result {
            let fdtos = 
               {
                     fileDtoStream.id = id;
                     name = name;
                     root = root;
                     meta = [|nameof sorterShcArchDto; name; id |> string|];
                     reader = SorterShcArchDto.fromJson;
                     writer = SorterShcArchDto.toJson
                }
            let! res = initFile fdtos
            return fdtos
        }

    let read (fdtos:fileDtoStream<'T>) =
        result {
            let! fpath = FilePath.appendFileName fdtos.root 
                                    (fdtos.id |> string |> FileName.fromString)
                                    (".txt" |> FileExt.fromString)
            let! lines = FileUtils.readLines fpath
            let! items = lines |> Seq.skip(1)
                               |> Seq.map(fdtos.reader)
                               |> Seq.toList
                               |> Result.sequence
            return items
        }



//type folderScheme = { dir:FileDir; meta:string array; fileRecords: Map<string, Guid> }

//module FolderScheme =

//    let create (dir:FileDir) 
//               (name:Guid) 
//               (meta:string array) 
//               (fileRecords:Map<string, Guid>) =
//        result {
//         let! folder = name |> string |> FileFolder.create ""
//         let! fdir = dir |> FileDir.appendFolder folder
//         let! metaFile = FileName.create "" "meta"
//         let fext = (".txt" |> FileExt.fromString)
//         let! metaPath = FilePath.appendFileName fdir metaFile fext
//         let! res = FileUtils.makeFile metaPath (meta |> Json.serialize)
//         return {
//                folderScheme.dir = fdir;
//                folderScheme.meta = [||];
//                fileRecords = Map.empty;
//            }
//        }

