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


    member this.setupWorld1() =
        let world1 = World.create (Guid.NewGuid())
                         ((Causes.fromCauseSpec CauseSpecSorters.testCauseSpec1)
                            |> Result.ExtractOrThrow)
                         Enviro.Empty

        this.makeTestDirectory() |> ignore
        use sw = new StreamWriter(this.fullPath(string world1.id))
        fprintfn sw "%s" (world1 |> DataStoreItemDto.storeWorld |> Json.serialize)
        sw.Dispose()


    member this.setupWorld2() =
        let world2 = World.create (Guid.NewGuid())
                            ((Causes.fromCauseSpec CauseSpecSorters.testCauseSpec2)
                            |> Result.ExtractOrThrow)
                            Enviro.Empty

        this.makeTestDirectory() |> ignore
        use sw = new StreamWriter(this.fullPath(string world2.id))
        fprintfn sw "%s" (world2 |> DataStoreItemDto.storeWorld |> Json.serialize)
        sw.Dispose()


    member this.setupWorld3() =
        let world3 = World.create (Guid.NewGuid())
                         ((Causes.fromCauseSpec CauseSpecSorters.testCauseSpec3)
                         |> Result.ExtractOrThrow)
                         Enviro.Empty

        this.makeTestDirectory() |> ignore
        use sw = new StreamWriter(this.fullPath((string world3.id)))
        fprintfn sw "%s" (world3 |> DataStoreItemDto.storeWorld |> Json.serialize)
        sw.Dispose()


    member this.setupAllWorlds() =
        this.setupWorld1()
        this.setupWorld2()
        this.setupWorld3()


    member this.tearDownDataSource() =
        let files  = Directory.GetFiles(this.testDir, "*.*")
        files |> Array.map(fun f -> File.Delete(f)) |> ignore
        //Threading.Thread.Sleep(100)
        Directory.Delete(this.testDir)


    [<TestMethod>]
    member this.DirectoryDataSource_GetDs() =
        this.setupAllWorlds() |> ignore
        let dirDs = new DirectoryDataSource(this.testDir) :> IDataSource
        let ds = dirDs.GetDataSource(CauseSpecSorters.testCauseSpec1Id) |> Result.ExtractOrThrow
        this.tearDownDataSource() 
        Assert.IsTrue(true);


    [<TestMethod>]
    member this.DirectoryDataSource_GetDsIds() =
        this.setupAllWorlds() |> ignore
        let dirDs = new DirectoryDataSource(this.testDir) :> IDataSource
        let ids = dirDs.GetDataSourceIds() |> Result.ExtractOrThrow
        this.tearDownDataSource() 
        Assert.AreEqual(ids.Length, 3);


    [<TestMethod>]
    member this.DirectoryDataSource_AddNewDataStoreItem() =
        this.setupWorld1() |> ignore
        this.setupWorld2() |> ignore
        let dirDs = new DirectoryDataSource(this.testDir) :> IDataSource
        let ids = dirDs.GetDataSourceIds() |> Result.ExtractOrThrow
        Assert.AreEqual(ids.Length, 2)

        let world3 = World.create (Guid.NewGuid())
                         ((Causes.fromCauseSpec CauseSpecSorters.testCauseSpec3)
                         |> Result.ExtractOrThrow)
                         Enviro.Empty
        dirDs.AddNewDataStoreItem (world3 |> WorldDto.toDto |> DataStoreItem.WorldDto) 
                                          |> Result.ExtractOrThrow 
                                          |> ignore
       
        let ids = dirDs.GetDataSourceIds() |> Result.ExtractOrThrow
        this.tearDownDataSource()
        Assert.AreEqual(ids.Length, 3)


        
    [<TestMethod>]
    member this.DirectoryDataSource_TurnWorldActionIntoWorld() =

        Assert.AreEqual(3, 3)