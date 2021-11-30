namespace global
open System

namespace global

type fileDtoStream<'T> = { name:Guid; root:FileDir; meta:string[]; reader:string->Result<'T, string>; writer:'T->string }

module FileDtoStream =
    let makeForSorter (name:Guid) (root:FileDir) =
        result {
            let! fpath = FilePath.appendFileName 
                                    root 
                                    (name |> string |> FileName.fromString)
            let meta = [|nameof sorter|]
            let! res = FileUtils.makeFileFromLines fpath meta
            return {
                     fileDtoStream.name = name;
                     root = root;
                     meta = meta;
                     reader = SorterDto.fromJson;
                     writer = SorterDto.toJson
                }
        }



    let append<'T> (fdtos:fileDtoStream<'T>) (items:seq<'T>) =
        result {
            let! fpath = FilePath.appendFileName fdtos.root (fdtos.name |> string |> FileName.fromString)
            let lines = items |> Seq.map(fdtos.writer)
            let! res = FileUtils.appendToFile fpath lines
            return res
        }

    let read (fdtos:fileDtoStream<'T>) =
        result {
            let! fpath = FilePath.appendFileName fdtos.root (fdtos.name |> string |> FileName.fromString)
            let! lines = FileUtils.readLines fpath
            let! items = lines |> Seq.skip(1)
                               |> Seq.map(fdtos.reader)
                               |> Seq.toList
                               |> Result.sequence
            return items
        }



type folderScheme = { dir:FileDir; meta:string array; fileRecords: Map<string, Guid> }

module FolderScheme =

    let create (dir:FileDir) 
               (name:Guid) 
               (meta:string array) 
               (fileRecords:Map<string, Guid>) =
        result {
         let! folder = name |> string |> FileFolder.create ""
         let! fdir = dir |> FileDir.appendFolder folder
         let! metaFile = FileName.create "" "meta.txt"
         let! metaPath = FilePath.appendFileName fdir metaFile
         let! res = FileUtils.makeFile metaPath (meta |> Json.serialize)
         return {
                folderScheme.dir = fdir;
                folderScheme.meta = [||];
                fileRecords = Map.empty;
            }
        }

