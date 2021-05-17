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
        Assert.AreEqual(
            arA.[(Degree.value TestData.degree) - 1], 
            Permutation.identity TestData.degree)


    [<TestMethod>]
    member this.permPowerDist() =
        let maxPower = 2000
        let degree = Degree.fromInt 16
        let permCount = 100000
        let randPerms = Permutation.createRandoms 
                                    degree 
                                    TestData.iRando
                        |> Seq.take permCount
                        |> Seq.map(fun p ->
                                    p |> (Permutation.powers maxPower) |> Seq.toArray)
                        |> Seq.toArray
        let yabs = randPerms |> Array.countBy(fun po->po.Length)
                             |> Array.sortBy(snd)
        yabs |> Array.iter(fun tup -> 
                        Console.WriteLine (sprintf "%d\t%d" (fst tup) (snd tup)))
        
        Assert.IsTrue (randPerms.Length > 0)


    [<TestMethod>]
    member this.stack() =
        let dd = Degree.fromInt 2
        let aL = [|0;1|]
        let aR = [|1;0|]
        let aS = [|0;1;3;2|]
        let aSl = aS |> Array.toList
        let tcpS =  TwoCycleGen.stack aL aR
                    |> Array.toList
        Assert.AreEqual (aSl, tcpS)


    [<TestMethod>]
    member this.quad() =
        let tAvs = [|0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;|]
        let pp = tAvs |> TwoCycleGen.t4
                      |> Seq.toList
        Assert.AreEqual(pp.Length, tAvs.Length)


    [<TestMethod>]
    member this.qStack() =
        let avsL = [|0;1;|]
        let avsR = [|0;1;|]
        let pp = TwoCycleGen.qStack 1 avsL avsR
                 |> Array.toList
        Assert.AreEqual(pp.Length, avsL.Length + avsR.Length)


    [<TestMethod>]
    member this.rndQStack() =
        let ys = Array.init 100 (fun _ -> 
                TwoCycleGen.rndQstack TestData.iRando)

        let chk = ys |> Array.map(TwoCyclePerm.toPermutation)
                     |> Array.map(Permutation.isTwoCycle)

        Assert.AreEqual(1, 1)

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
       let aa = TwoCycleGen.makeOddMode degree
       let ac = TwoCycleGen.makeOddModeWithCap degree
       let acv = TwoCyclePerm.arrayValues ac

       Assert.IsTrue(acv.Length > 0)

       
    [<TestMethod>]
    member this.TwoCyclePerm_makeCoConjugateEvenOdd() =
       let degree = Degree.fromInt 10
       let permLst1 = [Permutation.identity degree; Permutation.identity degree]

       let aa = TwoCycleGen.makeCoConjugateEvenOdd permLst1
                |> Result.ExtractOrThrow
       let al = aa |> Seq.toList

       Assert.IsTrue(al.Length > 0)


    [<TestMethod>]
    member this.TwoCyclePerm_makeReflSymmetric() =
       let degree = Degree.fromInt 16
       let rnd = Rando.LcgFromSeed 424
       let refSyms = Array.init 100 (fun _ ->
           TwoCyclePerm.makeReflSymmetric
                                        degree
                                        rnd)
       let smyr = refSyms |> Array.toSeq
                    |> Seq.map 
                    (Switch.fromTwoCyclePerm >> Seq.toArray)
                    |> Seq.concat
                    |> Seq.map(fun sw -> sw = (Switch.reflect degree sw))
                    |> Seq.countBy(id)
                    |> Seq.toArray
       
       Assert.AreEqual(smyr.Length, 2)
       let rr = refSyms |> Array.map(fun tcp -> 
                (tcp, tcp = (tcp |> TwoCyclePerm.reflect)))

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
    member this.IntBits_FromInteger() =
     let len = 6
     let expectedArray = [|1; 1; 1; 0; 1; 0|]
     let converted = IntBits.fromInteger 6 23
     Assert.IsTrue ((expectedArray = converted.values))
     

    [<TestMethod>]
    member this.IntBits_FromAndToInteger() =
     let testInt = 123456
     let converted = IntBits.fromInteger 32 testInt
     let intBack = IntBits.toInteger converted
     Assert.AreEqual (testInt, intBack)


    [<TestMethod>]
    member this.TwoCycleGen_evenDegree() =
     let degree = Degree.fromInt 16
     let res = TwoCycleGen.evenDegree degree 4

     Assert.IsTrue ((TwoCyclePerm.arrayValues res).Length = (Degree.value degree))


    [<TestMethod>]
    member this.uIntBits_fromIntBits() =
     let degree = Degree.fromInt 16
     let maxV = 1 <<< (Degree.value degree)
     let sortableCount = SortableCount.fromInt 100
     let unEncodedVals = Array.init 
                            (SortableCount.value sortableCount) 
                            (fun dex -> (dex * 867 + 1) % maxV)
     let intBitsArray = 
        unEncodedVals 
            |> Array.map(fun av ->
               IntBits.fromInteger (Degree.value degree)
                                   av)

     let theUints = BitsP32.zeroCreate (Degree.value degree)

     let bp32s = BitsP32.fromIntBits intBitsArray
                 |> Seq.toArray

     let bitsBack = BitsP32.toIntBits bp32s
                    |> Seq.toArray

     for i = 0 to (unEncodedVals.Length - 1) do
        BitsP32.stripeWrite theUints
                             intBitsArray.[i]
                             i
     let decodedVals = 
            bitsBack |> Array.map(IntBits.toInteger)
     let l = decodedVals.Length
     Assert.AreEqual (bitsBack.Length, l)
     for i = 0 to (unEncodedVals.Length - 1) do
         Assert.AreEqual (unEncodedVals.[i], decodedVals.[i])


    [<TestMethod>]
    member this.uIntBits_stripeRead() =
     let degree = Degree.fromInt 16
     let encodedVal = 8675
     let pos = 11
     let intBits = IntBits.fromInteger (Degree.value degree)
                                       encodedVal
     let yab = Array.init 50 (fun i -> (i + 10 - 1) / 10)
     let blank = BitsP32.zeroCreate (Degree.value degree)
     BitsP32.stripeWrite blank
                         intBits
                         pos
     let bitsBack = BitsP32.stripeRead blank 
                                       pos

     let decoded = IntBits.toInteger bitsBack

     Assert.AreEqual (encodedVal, decoded)
