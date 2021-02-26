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


    member this.world1 =
        World.createFromParent 
                World.empty
                (TestData.WorldActions.causeSpecRndSorters1
                    |> Causes.fromCauseSpec
                    |> Result.ExtractOrThrow)
        |> Result.ExtractOrThrow

    member this.world2 =
        World.createFromParent 
                World.empty
                (TestData.WorldActions.causeSpecRndSorters2
                    |> Causes.fromCauseSpec
                    |> Result.ExtractOrThrow)
        |> Result.ExtractOrThrow

    member this.world3 =
        World.createFromParent 
                World.empty
                (TestData.WorldActions.causeSpecRndSorters3
                    |> Causes.fromCauseSpec
                    |> Result.ExtractOrThrow)
        |> Result.ExtractOrThrow

    member this.setupDataStore(d:DataStoreItem) =
        this.makeTestDirectory() |> ignore
        use sw = new StreamWriter(this.fullPath(d |> DataStoreItem.getId |> string))
        fprintfn sw "%s" (d |> DataStoreItemDto.toJson)
        sw.Dispose()


    member this.setupAllWorlds() =
        this.setupDataStore (DataStoreItem.WorldDto (this.world1 |> WorldDto.toDto))
        this.setupDataStore (DataStoreItem.WorldDto (this.world2 |> WorldDto.toDto))
        this.setupDataStore (DataStoreItem.WorldDto (this.world3 |> WorldDto.toDto))


    member this.tearDownDataSource() =
        let files  = Directory.GetFiles(this.testDir, "*.*")
        files |> Array.map(fun f -> File.Delete(f)) |> ignore
        //Threading.Thread.Sleep(100)
        Directory.Delete(this.testDir)


    [<TestMethod>]
    member this.DirectoryDataSource_GetDs() =
        this.setupAllWorlds()
        let dirDs = new DirectoryDataSource(this.testDir) :> IDataSource
        let ds = dirDs.GetDataSource(this.world1.id) |> Result.ExtractOrThrow
        this.tearDownDataSource() 
        Assert.AreEqual(ds |> DataStoreItem.getId, this.world1.id);


    [<TestMethod>]
    member this.DirectoryDataSource_GetDsIds() =
        this.setupAllWorlds() |> ignore
        let dirDs = new DirectoryDataSource(this.testDir) :> IDataSource
        let ids = dirDs.GetDataSourceIds() |> Result.ExtractOrThrow
        this.tearDownDataSource() 
        Assert.AreEqual(ids.Length, 3);


    [<TestMethod>]
    member this.DirectoryDataSource_AddNewDataStoreItem() =
        this.setupDataStore (DataStoreItem.WorldDto (this.world1 |> WorldDto.toDto))
        this.setupDataStore (DataStoreItem.WorldDto (this.world2 |> WorldDto.toDto))
        let dirDs = new DirectoryDataSource(this.testDir) :> IDataSource
        let ids = dirDs.GetDataSourceIds() |> Result.ExtractOrThrow
        Assert.AreEqual(ids.Length, 2)

        dirDs.AddNewDataStoreItem (this.world3 |> WorldDto.toDto |> DataStoreItem.WorldDto) 
                                          |> Result.ExtractOrThrow 
                                          |> ignore
       
        let ids = dirDs.GetDataSourceIds() |> Result.ExtractOrThrow
        this.tearDownDataSource()
        Assert.AreEqual(ids.Length, 3)


        
    [<TestMethod>]
    member this.DirectoryDataSource_TurnWorldActionIntoWorld() =

        Assert.AreEqual(3, 3)