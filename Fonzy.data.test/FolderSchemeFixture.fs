namespace Fonzy.data.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Newtonsoft.Json
open System.IO
open System.Text

type Contact = {firstName:string; lastName:string}



[<TestClass>]
type FolderSchemeFixture () =
    member this.rootDir = "c:\\testDirForFolderSchemeFixture"

    member this.testSorters () =
        let randy = Rando.LcgFromSeed (RandomSeed.fromInt 123)
        let pfx = [|1;2;3|] |>  Switch.fromIndexes |> Seq.toList
        let deg = Degree.fromInt 16
        let sc = SwitchCount.fromInt 30
        let srg = sorterRndGen.RandSwitches (pfx, sc, deg)
        seq { while true do yield SorterRndGen.createRandom srg randy }

    

    [<TestMethod>]
    member this.fileDtoStream_RW () =
        let folderId = Guid.NewGuid()
        let sorters = this.testSorters() |> Seq.take(3) |> Seq.toList

        let fsr = result {
            let! folderRoot = FileDir.create "" this.rootDir
            let! fds = FileDtoStream.forSorterDto (folderId |> string) "descr" folderRoot
            let! res = FileDtoStream.append sorters fds 
            return fds
          }
        let fDtoS = fsr |> Result.ExtractOrThrow
        let sortersBack = FileDtoStream.read fDtoS |> Result.ExtractOrThrow

        Assert.AreEqual(sorters, sortersBack);




    [<TestMethod>]
    member this.TestMethodPassing () =
        Assert.AreEqual(1, 1);

  