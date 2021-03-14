namespace Fonzy.core.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type ComboStructuresFixture () =

    [<TestMethod>]
    member this.TestIdentityPermutation() =
      let expectedLen = (Degree.value TestData.degree)
      let expectedSum = ( expectedLen * (expectedLen - 1)) / 2
      let permutes = Permutation.identity  TestData.degree
      Assert.AreEqual(expectedLen, permutes |> Permutation.arrayValues |> Array.length)
      Assert.AreEqual(expectedSum, permutes |> Permutation.arrayValues |> Array.sum)


    [<TestMethod>]
    member this.powers() =
        let maxPower = 20
        let arA = TestData.ComboStructures.permutation |> Permutation.powers maxPower
                    |> Seq.toArray
        Assert.IsTrue (arA.Length > 0)
        Assert.IsTrue (arA.Length < maxPower)


    [<TestMethod>]
    member this.TestInversePermutation() =

       let inv = Permutation.inverse TestData.ComboStructures.permutation
       let prod = Permutation.productR TestData.ComboStructures.permutation inv 
                        |> Result.ExtractOrThrow
       let id = Permutation.identity TestData.degree
       Assert.AreEqual(id, prod)


    [<TestMethod>]
    member this.TestMakeMonoCycle() =
       let degree = Degree.create "" 6 |> Result.ExtractOrThrow
       let rnd = Rando.LcgFromSeed 424
       let tcp = TwoCyclePerm.makeMonoCycle degree 2 3 |> Result.ExtractOrThrow
       Assert.IsTrue(tcp |> TwoCyclePerm.toPermutation |> Permutation.isTwoCycle)

    [<TestMethod>]
    member this.TestMakeAllMonoCycles() =
       let degree = Degree.create "" 6 |> Result.ExtractOrThrow
       let rnd = Rando.LcgFromSeed 424
       let tcA = TwoCyclePerm.makeAllMonoCycles degree |> Seq.toArray
       let tcASq = tcA |> Array.map (fun tcp -> 
                tcp |> TwoCyclePerm.toPermutation |> Permutation.isTwoCycle)
       Assert.AreEqual(tcA.Length, 15)
       for i in {0 .. tcASq.Length - 1} do
          Assert.IsTrue(tcASq.[i])


    [<TestMethod>]
    member this.TestMakeMakeRandomFullPolyCycle() =
       let degree = Degree.create "" 9 |> Result.ExtractOrThrow
       let rnd = Rando.LcgFromSeed 424
       let id = TwoCyclePerm.identity degree
       for i in {0 .. 20} do
                let tcp = TwoCyclePerm.makeRandomFullTwoCycle degree rnd
                Assert.IsTrue(tcp |> TwoCyclePerm.toPermutation |> Permutation.isTwoCycle)


    [<TestMethod>]
    member this.TestMakeMakeRandomPolyCycle() =
       let degree = Degree.create "" 9 |> Result.ExtractOrThrow
       let rnd = Rando.LcgFromSeed 424
       let switchFreq = 0.5
       let id = TwoCyclePerm.identity degree
       for i in {0 .. 20} do
                let tcp = TwoCyclePerm.makeRandomTwoCycle degree rnd switchFreq
                Assert.IsTrue(tcp |> TwoCyclePerm.toPermutation |> Permutation.isTwoCycle)


    [<TestMethod>]
    member this.TwoCyclePerm_makeMode1() =
       let degree = Degree.fromInt 10
       let aa = TwoCyclePerm.makeOddMode degree
       //let b0 = Array2D.length1 aa

       Assert.IsTrue(true)

       
    [<TestMethod>]
    member this.makeFromTupleSeq() =        
        let rndy = Rando.LcgFromSeed 44
        let switchFreq = SwitchFrequency.fromFloat 0.5
        let degree = Degree.fromInt 16
        let switchCount = SwitchCount.fromInt 8
        let stageTupes = Stage.makeRandomStagedSwitchSeq degree switchFreq rndy
                         |> Seq.map(fun s -> (s.low, s.hi))
                         |> Seq.take (SwitchCount.value switchCount)
        let twoCycle = TwoCyclePerm.makeFromTupleSeq degree stageTupes
        Assert.IsTrue (true)


    [<TestMethod>]
    member this.ZeroOneSequence_FromInteger() =
     let len = 6
     let expectedArray = [|1; 0; 1; 0; 1; 0|]
     let converted = ZeroOneSequence.FromInteger len 21
     Assert.IsTrue ((expectedArray = converted))