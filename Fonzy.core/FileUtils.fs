namespace global
open System.Collections.Generic
open Microsoft.FSharp.Core
open System
open System.IO
open Newtonsoft.Json


module FileUtils =

    let getFilesInDirectory path ext =
        try
            Directory.GetFiles(path, ext) 
            |> Array.map Path.GetFileName  |> Ok
        with
            | ex -> ("error in getFilesInDirectory: " + ex.Message ) |> Result.Error

    let readFile (path:string) =
        try
            //let dto = System.IO.File.ReadAllText(path)
            use sr = new System.IO.StreamReader(path)
            let res = sr.ReadToEnd()
            sr.Dispose()
            res |> Ok
        with
            | ex -> ("error in readFile: " + ex.Message ) |> Result.Error


    let writeFile path item (append:bool) =
        try
            System.IO.File.WriteAllText(path, item)
            //use sw = new StreamWriter(path, append)
            //fprintfn sw "%s" item
            //sw.Dispose()
            path |> Ok
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




type LogFile = {cat:string; descr:string; header:string; records:string[]}
