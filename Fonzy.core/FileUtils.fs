namespace global
open System.Collections.Generic
open Microsoft.FSharp.Core
open System
open System.IO
open Newtonsoft.Json


module FileUtils =

    let makeDirectory (fd:FileDir) = 
        try
            Directory.CreateDirectory(FileDir.value fd) |> ignore 
            true |> Ok
        with
            | ex -> ("error in makeDirectory: " + ex.Message ) |> Result.Error


    let clearDirectory (fd:FileDir) = 
        try
            let files  = Directory.GetFiles(FileDir.value fd, "*.*")
            files |> Array.map(fun f -> File.Delete(f)) |> ignore
            Directory.Delete(FileDir.value fd) |> Ok
        with
            | ex -> ("error in clearDirectory: " + ex.Message ) |> Result.Error


    let getFilesInDirectory (fd:FileDir) ext =
        try
            Directory.GetFiles((FileDir.value fd), ext) 
            |> Array.map Path.GetFileName  |> Ok
        with
            | ex -> ("error in getFilesInDirectory: " + ex.Message ) |> Result.Error


    let readFile (fp:FilePath) = 
        try
            use sr = new System.IO.StreamReader(FilePath.value fp)
            let res = sr.ReadToEnd()
            sr.Dispose()
            res |> Ok
        with
            | ex -> ("error in readFile: " + ex.Message ) |> Result.Error


    let writeFile (fp:FilePath) item (append:bool) =
        try
            System.IO.File.WriteAllText((FilePath.value fp), item)
            //use sw = new StreamWriter(path, append)
            //fprintfn sw "%s" item
            //sw.Dispose()
            true |> Ok
        with
            | ex -> ("error in writeFile: " + ex.Message ) |> Result.Error

    //returns a cumer
    //let logFileKeyHeader path item =
    //    writeFile path (sprintf "%skey" item ) true
    //    new Dictionary<int, Dictionary<Guid, string>>()


    //let logFileKey path (cumer:Dictionary<int, Dictionary<Guid, string>>) (key:int) (group:Guid) item  =
    //    let newItems = CollectionUtils.cumulate cumer key group item
    //    newItems |> List.map(fun item->writeFile path (sprintf "%s%d" item key) true)
    //             |> Result.sequence


    //let logFileBackfill path (cumer:Dictionary<int, Dictionary<Guid, string>>) =
    //    let newItems = CollectionUtils.cumerBackFill cumer
    //    newItems |> List.map(fun item->writeFile path (sprintf "%s%d" (snd item) (fst item)) true)
    //             |> Result.sequence
        

module Json = 
    type Marker = interface end
        
    let serialize obj = JsonConvert.SerializeObject obj
        
    let deserialize<'a> str :Result<'a, string> =
        try
            JsonConvert.DeserializeObject<'a> str |> Ok
        with
        | ex -> Result.Error ex.Message
        
    let deserializeOption<'a> str =
        match str with
        | Some s -> (deserialize<'a> s)
        | None -> Result.Error  "option was none"




type csvFile = { header:string; records:string[]; directory:FileDir; fileName:string; }

module CsvFile =

    let writeCsvFile (csv:csvFile) =
        try
            Directory.CreateDirectory(FileDir.value(csv.directory)) |> ignore
            let FileDir = sprintf "%s\\%s" (FileDir.value(csv.directory)) csv.fileName
            use sw = new StreamWriter(FileDir, false)
            fprintfn sw "%s" csv.header
            csv.records |> Array.iter(fprintfn sw "%s")
            sw.Dispose()
            true |> Ok
        with
            | ex -> ("error in writeFile: " + ex.Message ) |> Result.Error
