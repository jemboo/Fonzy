namespace Fonzy.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open Newtonsoft.Json


[<TestClass>]
type FileUtilsFixture() =

    [<TestMethod>]
    member this.PrintArray() =
        // ??????????????????????
        let st = SwitchUses.createEmpty (SwitchCount.fromInt 1000)
        let wts = SwitchUses.getWeights st
        let ss = sprintf "%A" wts |> (fun s->s.Replace("\n ", ""))
        Assert.AreEqual(1, 1)


    [<TestMethod>]
    member this.SerializeLogFile() =
        let lfDto = {LogFile.cat="cat"; descr="descr"; header="header"; records=[|"a"; "b"|]}
        // serialized as:
        //{"cat":"cat","descr":"descr","header":"header","records":["a","b"]}
        let dto = JsonConvert.SerializeObject lfDto
        let lfDtoRet = JsonConvert.DeserializeObject<LogFile> dto
        //System.Console.WriteLine(dto)
        Assert.AreEqual(lfDto, lfDtoRet)

    [<TestMethod>]
    member this.FileIo() =
        let fp = "c:\log\JsonTest.txt"
        let lfDto = {LogFile.cat="cat"; descr="descr"; header="header"; records=[|"a"; "b"|]}
        let dtoOut = JsonConvert.SerializeObject lfDto
        System.IO.File.WriteAllText(fp, dtoOut)
        let dto = System.IO.File.ReadAllText(fp)
        let lfDtoRet = JsonConvert.DeserializeObject<LogFile> dto
        Assert.AreEqual(lfDto, lfDtoRet)



