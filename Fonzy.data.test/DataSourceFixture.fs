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

    member this.setupDataSource() =
        let world1 = World.create None 
                         ((Causes.fromCauseSpec CauseSpec.testCauseSpec1)
                            |> Result.ExtractOrThrow)
                         Enviro.Empty

        let world2 = World.create None 
                         ((Causes.fromCauseSpec CauseSpec.testCauseSpec2)
                         |> Result.ExtractOrThrow)
                         Enviro.Empty

        let world3 = World.create None
                         ((Causes.fromCauseSpec CauseSpec.testCauseSpec3)
                         |> Result.ExtractOrThrow)
                         Enviro.Empty

        this.makeTestDirectory() |> ignore
        use sw = new StreamWriter(this.fullPath((string world1.id)))
        fprintfn sw "%s" (world1 |> DataStoreItemDto.storeWorld)
        sw.Dispose()
        use sw = new StreamWriter(this.fullPath((string world2.id)))
        fprintfn sw "%s" (world2 |> DataStoreItemDto.storeWorld)
        sw.Dispose()
        use sw = new StreamWriter(this.fullPath((string world3.id)))
        fprintfn sw "%s" (world3 |> DataStoreItemDto.storeWorld)
        sw.Dispose()

    member this.tearDownDataSource() =
        let files  = Directory.GetFiles(this.testDir, "*.*")
        files |> Array.map(fun f -> File.Delete(f)) |> ignore
        //Threading.Thread.Sleep(100)
        Directory.Delete(this.testDir)

    [<TestMethod>]
    member this.DirectoryDataSource_GetDs() =
        this.setupDataSource() |> ignore
        let g = new DirectoryDataSource(this.testDir) :> IDataSource
       // let ds = g.GetDataSource() |> Result.ExtractOrThrow
        
        this.tearDownDataSource() 
        Assert.IsTrue(true);

    [<TestMethod>]
    member this.DirectoryDataSource_GetDsIds() =
        this.setupDataSource() |> ignore
        let g = new DirectoryDataSource(this.testDir) :> IDataSource
        let ids = g.GetDataSourceIds() |> Result.ExtractOrThrow
        let g = Guid.NewGuid()
        let s = (string g)
        this.tearDownDataSource() 
        Assert.AreEqual(ids.Length, 3);
