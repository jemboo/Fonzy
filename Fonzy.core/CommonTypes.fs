namespace global
open System
open System.IO

// Math
type Degree = private Degree of int
module Degree =
    let value (Degree v) = v
    let create fieldName v = 
        ConstrainedType.createInt fieldName Degree 1 1000 v
    let within (b:Degree) v =
        (v >= 0) && (v < (value b))
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>int)
        }
    let binExp (v: Degree) =
        let fexp = (value v) |> float
        ( ** ) 2.0 fexp |> int
    
    let maxSwitchesPerStage (degree:Degree) =
        (value degree) / 2


type MutationRate = private MutationRate of float
module MutationRate =
    let value (MutationRate v) = v
    let create fieldName v =
        ConstrainedType.createFloat fieldName MutationRate 0.0 1.0 v
    let fromFloat v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>float)
        }


// Common
type ArrayLength = private ArrayLength of int
type EntityId = private EntityId of Guid
type JsonString = private JsonString of string
type FileDir = private FileDir of string
type FileFolder = private FileFolder of string
type FilePath = private FilePath of string
type FileName = private FileName of string
type FileExt = private FileExt of string

type ReportingFrequency = private ReportingFrequency of int
type String50 = private String50 of string
type UseEagerProc = private UseEagerProc of bool
type UseParallel = private UseParallel of bool

module ArrayLength =
    let value (ArrayLength len) = len
    let create fieldName (len:int) =
        ConstrainedType.createInt fieldName ArrayLength 0 100000000 len
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>int)
        }

module EntityId =
    let value (EntityId v) = v
    let create id = Ok (EntityId id)
    let fromGuid (id:Guid) = create id |> Result.ExtractOrThrow

module JsonString =
    let value (JsonString str) = str
    let create fieldName str = 
        ConstrainedType.createString fieldName JsonString 100000000 str
    let createOption fieldName str = 
        ConstrainedType.createStringOption fieldName JsonString 50 str

// folder name (single name)
module FileFolder =
    let value (FileFolder str) = str
    let create fieldName str = 
        ConstrainedType.createString fieldName FileFolder 1000 str
    let createOption fieldName str = 
        ConstrainedType.createStringOption fieldName FileFolder 1000 str
    let fromString v = create "" v |> Result.ExtractOrThrow

// directory name (full path from root to folder)
module FileDir =
    let value (FileDir str) = str
    let create fieldName str = 
        ConstrainedType.createString fieldName FileDir 1000 str
    let createOption fieldName str = 
        ConstrainedType.createStringOption fieldName FileDir 1000 str
    let fromString v = create "" v |> Result.ExtractOrThrow
    let appendFolder (fn:FileFolder) (fd:FileDir) =
        try
            fromString (Path.Combine(value fd, fn |> FileFolder.value)) |> Ok
        with
            | ex -> ("error in addFolderName: " + ex.Message ) |> Result.Error

// file extension (single name)
module FileExt =
    let value (FileExt str) = str
    let create fieldName str = 
        ConstrainedType.createString fieldName FileExt 5 str
    let createOption fieldName str = 
        ConstrainedType.createStringOption fieldName FileExt 5 str
    let fromString v = create "" v |> Result.ExtractOrThrow

// file name only, (no path, no extension)
module FileName =
    let value (FileName str) = str
    let create fieldName str = 
        ConstrainedType.createString fieldName FileName 100 str
    let createOption fieldName str = 
        ConstrainedType.createStringOption fieldName FileName 100 str
    let fromString v = create "" v |> Result.ExtractOrThrow


// FileDir + (FileFolder (optional)) + FileName + file extension
module FilePath =
    let value (FilePath str) = str
    let create fieldName str = 
        ConstrainedType.createString fieldName FilePath 1000 str
    let createOption fieldName str = 
        ConstrainedType.createStringOption fieldName FilePath 1000 str
    let fromString v = create "" v |> Result.ExtractOrThrow
    let toFileDir (fp:FilePath) =
        Path.GetDirectoryName (value fp)
    let toFileName (fp:FilePath) =
        Path.GetFileName (value fp)
    let appendFileName (fd:FileDir) (fn:FileName) =
        try
            fromString (Path.Combine((FileDir.value fd), (FileName.value fn))) |> Ok
        with
            | ex -> ("error in addFolderName: " + ex.Message ) |> Result.Error


module ReportingFrequency =
    let value (ReportingFrequency freq) = freq
    let create fieldName (freq:int) =
        ConstrainedType.createInt fieldName ReportingFrequency 1 10000 freq
    let fromInt v = create "" v |> Result.ExtractOrThrow
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return! create "" (gv:?>int)
        }

module String50 =
    let value (String50 str) = str
    let create fieldName str = 
        ConstrainedType.createString fieldName String50 50 str
    let createOption fieldName str = 
        ConstrainedType.createStringOption fieldName String50 50 str

module UseEagerProc = 
    let create (useEagerProc:bool) =
        UseEagerProc useEagerProc
    let value (UseEagerProc v) = v
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return create (gv:?>bool)
        }

module UseParallel = 
    let create (useParallel:bool) =
        UseParallel useParallel
    let value (UseParallel v) = v
    let fromKey (m:Map<'a, obj>) (key:'a) =
        result {
            let! gv = ResultMap.read key m
            return create (gv:?>bool)
        }
