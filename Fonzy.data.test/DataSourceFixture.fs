namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open System.IO

[<TestClass>]
type DataSourceFixture () =

    member this.testDir = "c:\\testDirForDataSourceFixture"

    member this.fullPath(fn:string) =
        Path.Combine(this.testDir, fn + ".txt")

    member this.makeTestDirectory() =
        Directory.CreateDirectory(this.testDir)
        |> ignore

    member this.setupDataSourceSubsetA() =
        let world1 = World.create None 
                         ((Causes.fromCauseSpec CauseSpec.testCauseSpec1)
                            |> Result.ExtractOrThrow)
                         Enviro.Empty

        let world2 = World.create None 
                         ((Causes.fromCauseSpec CauseSpec.testCauseSpec2)
                         |> Result.ExtractOrThrow)
                         Enviro.Empty

        this.makeTestDirectory() |> ignore
        use sw = new StreamWriter(this.fullPath(string world1.id))
        fprintfn sw "%s" (world1 |> DataStoreItemDto.storeWorld|> Json.serialize)
        sw.Dispose()
        use sw = new StreamWriter(this.fullPath(string world2.id))
        fprintfn sw "%s" (world2 |> DataStoreItemDto.storeWorld |> Json.serialize)
        sw.Dispose()

    member this.setupDataSourceSubsetB() =
        let world3 = World.create None
                         ((Causes.fromCauseSpec CauseSpec.testCauseSpec3)
                         |> Result.ExtractOrThrow)
                         Enviro.Empty

        use sw = new StreamWriter(this.fullPath((string world3.id)))
        fprintfn sw "%s" (world3 |> DataStoreItemDto.storeWorld |> Json.serialize)
        sw.Dispose()


    member this.setupDataSource() =
        this.setupDataSourceSubsetA()
        this.setupDataSourceSubsetB()


    member this.tearDownDataSource() =
        let files  = Directory.GetFiles(this.testDir, "*.*")
        files |> Array.map(fun f -> File.Delete(f)) |> ignore
        //Threading.Thread.Sleep(100)
        Directory.Delete(this.testDir)


    [<TestMethod>]
    member this.DirectoryDataSource_GetDs() =
        this.setupDataSource() |> ignore
        let dirDs = new DirectoryDataSource(this.testDir) :> IDataSource
        let ds = dirDs.GetDataSource(CauseSpec.testCauseSpec1Id) |> Result.ExtractOrThrow
        this.tearDownDataSource() 
        Assert.IsTrue(true);

    [<TestMethod>]
    member this.DirectoryDataSource_GetDsIds() =
        this.setupDataSource() |> ignore
        let dirDs = new DirectoryDataSource(this.testDir) :> IDataSource
        let ids = dirDs.GetDataSourceIds() |> Result.ExtractOrThrow
        this.tearDownDataSource() 
        Assert.AreEqual(ids.Length, 3);


    [<TestMethod>]
    member this.DirectoryDataSource_AddNewDataStoreItem() =
        this.setupDataSourceSubsetA() |> ignore
        let dirDs = new DirectoryDataSource(this.testDir) :> IDataSource
        let ids = dirDs.GetDataSourceIds() |> Result.ExtractOrThrow
        Assert.AreEqual(ids.Length, 2)

        let world3 = World.create None
                         ((Causes.fromCauseSpec CauseSpec.testCauseSpec3)
                         |> Result.ExtractOrThrow)
                         Enviro.Empty
        dirDs.AddNewDataStoreItem (world3 |> WorldDto.toDto |> DataStoreItem.WorldDto) 
                                          |> Result.ExtractOrThrow 
                                          |> ignore
       
        let ids = dirDs.GetDataSourceIds() |> Result.ExtractOrThrow
        this.tearDownDataSource()
        Assert.AreEqual(ids.Length, 3)