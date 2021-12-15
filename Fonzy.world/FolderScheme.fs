﻿namespace global
open System

namespace global

type fileDtoStream<'T> = 
    { 
        id:string; 
        descr:string; 
        root:FileDir; 
        meta:string array; 
        reader:string->Result<'T, string>; 
        writer:'T->string
    }

module FileDtoStream =
    
    let getFilePath (fdtos:fileDtoStream<'T>) =
        result {
            let! fpath = FilePath.fromParts 
                                    fdtos.root 
                                    (fdtos.id |> string |> FileName.fromString)
                                    (".txt" |> FileExt.fromString)
            return fpath
        }

    let getCatalogFilePath (fdtos:fileDtoStream<'T>) =
        result {
            let! fpath = FilePath.fromParts 
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

    let makeFileHeader (fdtos:fileDtoStream<'T>) =
        result {
            let! di = FileUtils.makeDirectory fdtos.root
            let! fpath = getFilePath fdtos
            let line1 = seq { fdtos.meta |> Json.serialize }
            let! res = FileUtils.makeFileFromLines fpath line1
            return fdtos
        }

    let append<'T> (items:seq<'T>) (fdtos:fileDtoStream<'T>) =
        result {
            let! fpath = getFilePath fdtos
            let lines = items |> Seq.map(fdtos.writer)
            let! res = FileUtils.appendToFile fpath lines
            return res
        }


    let forSorterDto (id:string) (descr:string) (root:FileDir) =
        result {
            let fdtos = 
               {
                     fileDtoStream.id = id;
                     descr = descr;
                     root = root;
                     meta = [|nameof sorterDto; descr; id |> string|];
                     reader = SorterDto.fromJson;
                     writer = SorterDto.toJson
                }
            let! res = makeFileHeader fdtos
            return fdtos
        }


    let forSorterShcArchDto (id:string) (descr:string) (root:FileDir) =
        result {
            let fdtos = 
               {
                     fileDtoStream.id = id;
                     descr = descr;
                     root = root;
                     meta = [|nameof sorterShcArchDto; descr; id |> string|];
                     reader = SorterShcArchDto.fromJson;
                     writer = SorterShcArchDto.toJson
                }
            let! res = makeFileHeader fdtos
            return fdtos
        }


    let forSorterShc2Dto (id:string) (descr:string) (root:FileDir) =
        result {
            let fdtos = 
               {
                     fileDtoStream.id = id;
                     descr = descr;
                     root = root;
                     meta = [|nameof sorterShc2Dto; descr; id |> string|];
                     reader = Json.deserialize<sorterShc2Dto>  //fun _ -> "not implemented" |> Error;
                     writer = Json.serialize
                }
            let! res = makeFileHeader fdtos
            return fdtos
        }


    let openSorterShc2Dto (descr:string) (fpath:FilePath) =

        if (FilePath.exists fpath) then
            result {
                let! lines = FileUtils.readLines fpath
                let! meta = lines |> Seq.head |> Json.deserialize<string[]>
                return {
                          fileDtoStream.id =  meta.[2];
                          descr = meta.[1];
                          root = fpath |> FilePath.toFileDir
                          meta = meta;
                          reader = Json.deserialize<sorterShc2Dto>
                          writer = Json.serialize
                        }
                 }
            else
              let fileName = fpath |> FilePath.toFileName
              let fileDir = fpath |> FilePath.toFileDir
              forSorterShc2Dto (FileName.value fileName) descr fileDir



    let read (fdtos:fileDtoStream<'T>) =
        result {
            let! fpath = FilePath.fromParts fdtos.root 
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

