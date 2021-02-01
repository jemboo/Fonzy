namespace Fonzy.core.test
open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type RandoFixture () =

    [<TestMethod>]
    member this.MakeUint () =
        let seed = RandomSeed.create "" 424 |> Result.ExtractOrThrow
        let randoLcg = new RandomLcg(seed)
        let rv = int (randoLcg.NextUInt % 500u)
        Assert.IsTrue(rv > 0);
        let rv = int (randoLcg.NextUInt % 500u)
        Assert.IsTrue(rv > 0);
        let rv = int (randoLcg.NextUInt % 500u)
        Assert.IsTrue(rv > 0);
        let rv = int (randoLcg.NextUInt % 500u)
        Assert.IsTrue(rv > 0);


    [<TestMethod>]
    member this.TestPermutations () =        
        let degree = 6 |> Degree.create "" |> Result.ExtractOrThrow
        let seed = RandomSeed.create "" 424 |> Result.ExtractOrThrow
        let randoLcg = new RandomLcg(seed)
        let sortableCount = 4

        let perms = Permutation.createRandoms degree randoLcg
                    |> Seq.map(fun i -> Permutation.arrayValues i )
                    |> Seq.take sortableCount

        Assert.IsTrue(true);


    //[<TestMethod>]
    //member this.RngGenDto() =
    //    let rngGen = {RngGen.rngType=RngType.Lcg; seed = RandomSeed.create "" 123|>Result.ExtractOrThrow}
    //    let dto = RngGenDto.toDto rngGen
    //    let rngGenBack = RngGenDto.fromDto dto |> Result.ExtractOrThrow
    //    Assert.IsTrue((rngGen=rngGenBack))
        

    [<TestMethod>]
    member this.IndexedRandomData() =
        let rg = RngGen.createLcg 123
        let seeds = RandoCollections.IndexedSeedGen rg
                    |> Seq.take(10) |> Seq.toArray

        Assert.IsTrue(seeds.Length = 10)


    [<TestMethod>]
    member this.normalDistRandom() =
        let randy = Rando.fromRngGen (RngGen.createLcg 123)
        let ave = Rando.normalDistRandomSeq 0.0 1.0 randy
                    |> Seq.take(1000)
                    |> Seq.average
        Assert.IsTrue(Math.Abs(ave) < 0.2)


    [<TestMethod>]
    member this.normalDistRandomInts() =
        let randy = Rando.fromRngGen (RngGen.createLcg 123)
        let normInt() = 
            float (fst (Rando.normalDistInt2d 1.0 100.0 randy))
        let lst = seq {0 .. 1000} |> Seq.map(fun _ -> normInt())
                        |> Seq.toList
        let ave = lst |> List.average
        Assert.IsTrue(Math.Abs(ave) < 0.2)
