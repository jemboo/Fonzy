namespace Fonzy.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Newtonsoft.Json


[<TestClass>]
type FileUtilsFixture() =

    [<TestMethod>]
    member this.Degree_maxSwitchesPerStage() =
        let degree = Degree.fromInt 7
        let msw = 3
        let cc = Degree.maxSwitchesPerStage degree
        Assert.AreEqual(msw, cc)


    [<TestMethod>]
    member this.PrintArray() =
        // ??????????????????????
        let st = SwitchUses.createEmpty (SwitchCount.fromInt 1000)
        let wts = SwitchUses.getWeights st
        let ss = sprintf "%A" wts |> (fun s->s.Replace("\n ", ""))
        Assert.AreEqual(1, 1)


    [<TestMethod>]
    member this.writeCsvFile() =
        let csv =
            {
                csvFile.header = "cat"; 
                directory = FilePath.fromString "c:\\testFileUtils"; 
                fileName = "fileName.txt"; 
                records = [|"a"; "b"|]
            }
        let res = CsvFile.writeCsvFile csv |> Result.ExtractOrThrow
        Assert.IsTrue(res)

    [<TestMethod>]
    member this.FileIo() =
        let fp = "c:\log\JsonTest.txt"

        Assert.AreEqual(1, 1)



