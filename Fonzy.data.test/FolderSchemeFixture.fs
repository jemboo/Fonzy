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

    [<TestMethod>]
    member this.readFolde () =
        let folderName = Guid.NewGuid()
        let fsr = result {
            let! folderRoot = FileDir.create "" this.rootDir
            let! res = FolderScheme.create folderRoot folderName [||] Map.empty
            return res
          }
        let fs = fsr  |> Result.ExtractOrThrow

        Assert.AreEqual((FileDir.value fs.dir), this.rootDir);



    [<TestMethod>]
    member this.TestMethodPassing () =
        let folderName = Guid.NewGuid()
        let fsr = result {
            let! folderRoot = FileDir.create "" this.rootDir
            let! res = FolderScheme.create folderRoot folderName [||] Map.empty
            return res
          }
        let fs = fsr  |> Result.ExtractOrThrow

        Assert.AreEqual((FileDir.value fs.dir), this.rootDir);

    [<TestMethod>]
    member this.streaminText() =

       let json = @"[
                {
                ""firstname"": ""Joe"",
                ""lastname"": ""Schmoe""
                },
                {
                ""firstname"": ""John"",
                ""lastname"": ""Doe""
                },
                {
                ""firstname"": ""Bob"",
                ""lastname"": ""Smith""
                },
                {
                ""firstname"": ""Jane"",
                ""lastname"": ""Adams""
                },
                {
                ""firstname"": ""Mike"",
                ""lastname"": ""Edwards""
                },
                {
                ""firstname"": ""Tom"",
                ""lastname"": ""Jones""
                },
                {
                ""firstname"": ""Sandra"",
                ""lastname"": ""Davis""
                },
                {
                ""firstname"": ""David"",
                ""lastname"": ""Johnson""
                }
            ]"

       let bytes = Encoding.UTF8.GetBytes(json)
       let ms = new MemoryStream(bytes)
       let sr = new StreamReader(ms)
       let jsr = new JsonTextReader(sr)
       let ser = new JsonSerializer()
       let mutable c = { Contact.firstName = "f"; lastName = "l"}
       while jsr.Read() do
        if jsr.TokenType = JsonToken.StartObject then
            c <- ser.Deserialize<Contact>(jsr)

       Assert.AreEqual(1, 1);


    [<TestMethod>]
    member this.streaminFile() =

       let filePath = Path.Combine(this.rootDir, "TestText.txt")
       let fileStream = File.Open(filePath, FileMode.Open)
       let sr = new StreamReader(fileStream)
       let jsr = new JsonTextReader(sr)
       let ser = new JsonSerializer()
       let mutable c = { Contact.firstName = "f"; lastName = "l"}
       while jsr.Read() do
        if jsr.TokenType = JsonToken.StartObject then
            c <- ser.Deserialize<Contact>(jsr)

       Assert.AreEqual(1, 1);



    [<TestMethod>]
    member this.streaminFile2() =

       let filePath = Path.Combine(this.rootDir, "TestText2.txt")
       let fileStream = File.Open(filePath, FileMode.Open)
       let sr = new StreamReader(fileStream)
       let mutable keepGoing = true
       let mutable nl = ""
       let mutable c = { Contact.firstName = "f"; lastName = "l"}
       while keepGoing do
         nl <- sr.ReadLine()
         if nl |> String.IsNullOrEmpty then
            keepGoing <- false
         else
           c <- (Json.deserialize<Contact> nl |> Result.ExtractOrThrow)

       Assert.AreEqual(1, 1);



    [<TestMethod>]
    member this.appendinFile() =
       let mutable c = { Contact.firstName = "f"; lastName = "l"}
       let filePath = Path.Combine(this.rootDir, "TestText2.txt")
       let fileStream = File.Open(filePath, FileMode.Append)
       let sr = new StreamWriter(fileStream)
       let jsn = Json.serialize c
       sr.WriteLine jsn
       sr.Close()

       Assert.AreEqual(1, 1);






      ////  let tr = System.IO
      // // let serializer = System.Text.Json.JsonSerializer.Deserialize<int>
      // let fp = Path.Combine(this.rootDir, "TestText.txt")
      // let fs = new FileStream(fp, FileMode.Open, FileAccess.Read)
      // //let jsonReader = new System.Text.Json.Json
       