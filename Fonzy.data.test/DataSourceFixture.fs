namespace Fonzy.world.test

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
                (TestData.CauseSpec.SorterSet.rand1
                    |> Causes.fromCauseSpec
                    |> Result.ExtractOrThrow)
        |> Result.ExtractOrThrow

    member this.world2 =
        World.createFromParent 
                World.empty
                (TestData.CauseSpec.SorterSet.rand2
                    |> Causes.fromCauseSpec
                    |> Result.ExtractOrThrow)
        |> Result.ExtractOrThrow

    member this.world3 =
        World.createFromParent 
                World.empty
                (TestData.CauseSpec.SorterSet.rand3
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
        let ds = dirDs.GetDataSource(WorldId.value this.world1.id) |> Result.ExtractOrThrow
        this.tearDownDataSource() 
        Assert.AreEqual(ds |> DataStoreItem.getId, WorldId.value this.world1.id);

    [<TestMethod>]
    member this.assureDirectory() =
        let dirDs = new DirectoryDataSource(this.testDir) :> IDataSource
        let res = dirDs.AssureDirectory |> Result.ExtractOrThrow
        let res2 = dirDs.AssureDirectory |> Result.ExtractOrThrow
        this.tearDownDataSource() 
        Assert.IsTrue(res);
        Assert.IsTrue(res2);

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

        let res = dirDs.AddNewDataStoreItem (
                            this.world3 |> WorldDto.toDto |> DataStoreItem.WorldDto) 
                        |> Result.ExtractOrThrow 
        Assert.IsTrue(res)
        let ids2 = dirDs.GetDataSourceIds() |> Result.ExtractOrThrow 
        this.tearDownDataSource()
        Assert.AreEqual(ids2.Length, 3)

        
    [<TestMethod>]
    member this.DirectoryDataSource_TurnWorldActionIntoWorld() =
        this.makeTestDirectory() |> ignore
        let dirDs = new DirectoryDataSource(this.testDir) :> IDataSource
        let randWorldAction = TestData.WorldAction.SorterGen.randWorldAction 
        let worldActionDto = randWorldAction |> WorldActionDto.toDto
        let dataStoreItem  = worldActionDto |> DataStoreItem.WorldActionDto
        dirDs.AddNewDataStoreItem dataStoreItem
                                          |> Result.ExtractOrThrow 
                                          |> ignore
        let ids = dirDs.GetDataSourceIds() |> Result.ExtractOrThrow  
        Assert.AreEqual(ids.Length, 1)
        let dataStoreItemBack = dirDs.GetDataSource ids.[0]
                                        |> Result.ExtractOrThrow
        let worldActionDto = dataStoreItemBack 
                                        |> DataStoreItem.getWorldActionDto
                                        |> Result.ExtractOrThrow

        let worldActionBack = worldActionDto
                                        |> WorldActionDto.fromDto
                                        |> Result.ExtractOrThrow
        
        Assert.AreEqual(randWorldAction.cause.causeSpec.id, 
                        worldActionBack.cause.causeSpec.id)
        let dataStoreWorldDto = WorldAction.createWorld worldActionBack 
                                    |> Result.ExtractOrThrow
                                    |> WorldDto.toDto
                                    |> DataStoreItem.WorldDto
        dirDs.AddNewDataStoreItem dataStoreWorldDto
                                    |> Result.ExtractOrThrow
                                    |> ignore
        let ids = dirDs.GetDataSourceIds() |> Result.ExtractOrThrow
        this.tearDownDataSource()
        Assert.AreEqual(ids.Length, 1)