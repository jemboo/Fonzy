namespace global
open System.Collections.Generic
open Microsoft.FSharp.Core
open System
open System.IO
open Newtonsoft.Json


module FileUtils =

    let makeDirectory (fd:FileDir) = 
        try
            Directory.CreateDirectory(FileDir.value fd) |> Ok
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
            

    let readLines<'T> (fp:FilePath) = 
        try
            System.IO.File.ReadAllLines (FilePath.value fp)
                            |> Ok
        with
            | ex -> ("error in readFile: " + ex.Message ) |> Result.Error


    let makeFile (fp:FilePath) item =
        try
            System.IO.File.WriteAllText((FilePath.value fp), item)
            //use sw = new StreamWriter(path, append)
            //fprintfn sw "%s" item
            //sw.Dispose()
            true |> Ok
        with
            | ex -> ("error in writeFile: " + ex.Message ) |> Result.Error


    let makeFileFromLines (fp:FilePath) (lines:seq<string>) =
        try
            System.IO.File.WriteAllLines((FilePath.value fp), lines)
            true |> Ok
        with
            | ex -> ("error in writeFile: " + ex.Message ) |> Result.Error


    let appendToFile (fp:FilePath) (lines:seq<string>) =
        try
            System.IO.File.AppendAllLines((FilePath.value fp), lines)
            true |> Ok
        with
            | ex -> ("error in writeFile: " + ex.Message ) |> Result.Error

    let makeArchiver (dir:FileDir) =
        fun (folder:FileFolder) (file:FileName) (ext:FileExt) (data:seq<string>) ->
            try
                let fne = sprintf "%s.%s" (FileName.value file) (FileExt.value ext)
                let fp = Path.Combine(FileDir.value dir, fne) |> FilePath.fromString
                let dirInfo = System.IO.Directory.CreateDirectory (FileDir.value dir)
                appendToFile fp data
            with
                | ex -> ("error in archiver: " + ex.Message ) |> Result.Error


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
