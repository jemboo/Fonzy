namespace Fonzy.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Newtonsoft.Json


[<TestClass>]
type FileUtilsFixture() =

    [<TestMethod>]
    member this.writeCsvFile() =
        let csv =
            {
                csvFile.header = "cat"; 
                directory = FileDir.fromString "c:\\testFileUtils"; 
                fileName = "fileName.txt"; 
                records = [|"a"; "b"|]
            }
        let res = CsvFile.writeCsvFile csv |> Result.ExtractOrThrow
        Assert.IsTrue(res)

    [<TestMethod>]
    member this.makeArchiver() =
        let fDir = FileDir.fromString "c:\\folderTest\\archiver"
        let folder = FileFolder.fromString "FileFolder"
        let file = FileName.fromString "FileName"
        let ext = FileExt.fromString "txt"
        let testData = seq {"line1"; "line2"}

        let archiver = FileUtils.makeArchiver fDir
        let res = archiver folder file ext testData
        Assert.AreEqual(1, 1)



