namespace Fonzy.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open System.Diagnostics

[<TestClass>]
type CombinatoricsFixture () =

    [<TestMethod>]
    member this.fisherYatesShuffle() =
      let starting = [|2; 4; 6; 8; 10; 12; 14 |]
      let rnd = Rando.LcgFromSeed (RandomSeed.fromInt 424)
      let seededFY = Combinatorics.fisherYatesShuffle rnd
      let actual =  seededFY [|2; 4; 6; 8; 10; 12; 14 |] |> Seq.toArray
      Assert.AreEqual(starting.Length, actual.Length)

    [<TestMethod>]
    member this.rndMonoTwoCycle() =
      let degree = Degree.create "" 5 |> Result.ExtractOrThrow
      let rnd = Rando.LcgFromSeed (RandomSeed.fromInt 424) 
      let ts = seq {0 .. 10} 
                    |> Seq.map(fun _ -> Combinatorics.rndMonoTwoCycle degree rnd)
                    |> Seq.toArray
      Assert.IsTrue (ts.Length = 11)


    [<TestMethod>]
    member this.drawTwoWithoutRep() =
      let degree = Degree.create "" 7 |> Result.ExtractOrThrow
      let rnd = Rando.LcgFromSeed (RandomSeed.fromInt 4324) 
      let ts = seq {0 .. 1000} 
                    |> Seq.map(fun _ -> Combinatorics.drawTwoWithoutRep degree rnd)
                    |> Seq.groupBy(id)
                    |> Seq.map(fun t-> (fst t), (snd t) |> Seq.length)
                    |> Seq.toArray
      Assert.IsTrue (ts.Length = 21)


    [<TestMethod>]
    member this.makeAllMonoTwoCycles() =
      let degree = Degree.create "" 5 |> Result.ExtractOrThrow
      let dd = Combinatorics.makeAllMonoTwoCycles degree |> Seq.toArray
      Assert.IsTrue (dd.Length = 10)



    [<TestMethod>]
    member this.TestCompareArrays() =
      let firstArray = [|2; 4; 6; 8; 10; 12; 14 |]
      let shortArray = [|2; 4; 6; 8; 10; 12; |]
      let otherArray = [|2; 4; 6; 8; 11; 12; 14 |]
      Assert.IsTrue ((firstArray = firstArray))
      Assert.IsFalse ((firstArray = shortArray))
      Assert.IsFalse ((firstArray = otherArray))
 

    [<TestMethod>]
    member this.composeMapIntArrays() =
        let degree = Degree.create "" 6 |> Result.ExtractOrThrow
        let flipA = [|5; 4; 3; 2; 1; 0 |]
        let idA =[|0 .. (Degree.value degree)-1|]
        let prodByIdR = Combinatorics.composeIntArrayMaps flipA idA
        Assert.IsTrue ((prodByIdR = flipA))
        let prodByIdL = Combinatorics.composeIntArrayMaps idA flipA
        Assert.IsTrue ((prodByIdL = flipA))
        let prodSquare =  Combinatorics.composeIntArrayMaps flipA flipA
        Assert.IsTrue ((prodSquare = idA))


    [<TestMethod>]
    member this.isSorted() =
        Assert.IsTrue (Combinatorics.isSortedI Array.empty<int>)
        Assert.IsFalse (Combinatorics.isSortedI [|0; 1; 1; 0; 1; 0|])
        Assert.IsTrue (Combinatorics.isSortedI [|0; 0; 0; 0; 1; 1|])


    [<TestMethod>]
    member this.isSortedOffset() =
        Assert.IsFalse (Combinatorics.isSortedOffsetI [|0; 1; 1; 0; 1; 0; 1; 1 |] 1 5)
        Assert.IsTrue (Combinatorics.isSortedOffsetI [|0; 0; 0; 0; 1; 1; 0; 0|] 1 5)


    [<TestMethod>]
    member this.entropyBits() =
        let id = Combinatorics.identity 7
        let res1 = Combinatorics.entropyBits [|1; 1 |]
        Assert.AreEqual (res1, 1.0)
        let res2 = Combinatorics.entropyBits [|1; 1; 1; 1; 0|]
        Assert.AreEqual (res2, 2.0)

    [<TestMethod>]
    member this.rndFullTwoCycleArray() =
        let length = 29
        let rnd = Rando.LcgFromSeed (RandomSeed.fromInt 424)
        let unsortedArray = Combinatorics.rndFullTwoCycleArray rnd length
        let unsortedScore = Combinatorics.unsortednessSquared unsortedArray
        Assert.IsTrue (unsortedScore >  0)


    [<TestMethod>]
    member this.inverseMapArray() =
        let degree = Degree.create "" 8 |> Result.ExtractOrThrow
        let randy = Rando.LcgFromSeed (RandomSeed.fromInt 424)
        let mutable i = 0
        while i<100 do
            let bloke = Combinatorics.randomPermutation degree randy
            let inv = Combinatorics.inverseMapArray bloke
            let prod = Combinatorics.composeIntArrayMaps bloke inv
            Assert.IsTrue((prod = (Combinatorics.identity (Degree.value degree))))
            i <- i+1

    [<TestMethod>]
    member this.conjugateIntArrays() =
        let degree = Degree.create "" 8 |> Result.ExtractOrThrow
        let randy = Rando.LcgFromSeed (RandomSeed.fromInt 424)
        let mutable i = 0
        while i<10 do
            let conjer = Combinatorics.randomPermutation degree randy
            let tc1 = Combinatorics.randomPermutation degree randy
            let conj1 = Combinatorics.conjIntArrays conjer tc1 
            let tc2 = Combinatorics.randomPermutation degree randy
            let conj2 = Combinatorics.conjIntArrays conjer tc2 
            let prod = Combinatorics.composeIntArrayMaps tc1 tc2
            let conjprod = Combinatorics.conjIntArrays conjer prod 
            let prodconj = Combinatorics.composeIntArrayMaps conj1 conj2
            Assert.IsTrue((conjprod=prodconj))
            i <- i+1


    [<TestMethod>]
    member this.conjugateIntArrays_preserves_twoCycle() =
        let degree = Degree.create "" 8 |> Result.ExtractOrThrow
        let randy = Rando.LcgFromSeed (RandomSeed.fromInt 424)
        let mutable i = 0
        while i<10 do
            let tc = Combinatorics.rndFullTwoCycleArray randy (Degree.value degree)
            let conjer = Combinatorics.randomPermutation degree randy
            let conj = Combinatorics.conjIntArrays conjer tc 
            Assert.IsTrue(Combinatorics.isTwoCycle conj)
            i <- i+1

        

    [<TestMethod>]
    member this.rndTwoCycleArray() =
        let randy = Rando.LcgFromSeed (RandomSeed.fromInt 424)
        let arraySize = 16
        let cycleCount = 2
        let block = Combinatorics.rndTwoCycleArray randy arraySize cycleCount
        Assert.IsTrue (Combinatorics.isTwoCycle block)
        let cycleCount = 8
        let block2 = Combinatorics.rndTwoCycleArray randy arraySize cycleCount
        Assert.IsTrue (Combinatorics.isTwoCycle block2)


    [<TestMethod>]
    member this.toCumulative() =
        let testArray = [|2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0; 9.0; 10.0|]
        let startVal = 3.5
        let expectedResult = [|5.5; 8.5; 12.5; 17.5; 23.5; 30.5; 38.5; 47.5; 57.5|]
        let actualResult = Combinatorics.toCumulative startVal testArray
        Assert.AreEqual(expectedResult.[8], actualResult.[8])


    [<TestMethod>]
    member this.fromWeightedDistribution() =
        let testArray = [|2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0; 9.0; 10.0; |]
        let rndy = Rando.LcgFromSeed (RandomSeed.fromInt 424)
        let mutable log = []
        let LogRes s =
            log <- s::log
        let weightFunction (w:float) =
            1.0 / w
        testArray |>  (Combinatorics.fromWeightedDistribution weightFunction rndy)
            |> Seq.take 100 |> Seq.toArray
            |> Array.groupBy(id)
            |> Array.iter(fun b -> LogRes (sprintf "%f %d" (fst b) (snd b).Length))

        Assert.IsTrue (log.Length > 1)


    [<TestMethod>]
    member this.enumNchooseM() =
        let n = 8
        let m = 5
        let res = Combinatorics.enumNchooseM n m 
                    |> Seq.map(List.toArray)
                    |> Seq.toList
        Assert.IsTrue(res |> Seq.forall(Combinatorics.isSortedI))
        Assert.AreEqual(res.Length, 56)



    [<TestMethod>]
    member this.reflectivePairs() =
      let degree = Degree.fromInt 30
      let rnd = Rando.LcgFromSeed (RandomSeed.fromInt 424) 

      let runTest () =
          let pairs1 = ReflectiveIndexes.reflectivePairs 
                                         degree
                                         rnd          
                           |> Seq.toArray
          let res = pairs1 |> Array.forall (ReflectiveIndexes.isGood)
          (pairs1, res)
      let qua = Array.init 10 (fun _ -> runTest())
      qua |> Array.iter(fun tup -> Assert.IsTrue(snd tup))
      Assert.IsTrue(true)

