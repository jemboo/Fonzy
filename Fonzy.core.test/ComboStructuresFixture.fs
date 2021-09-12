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
    member this.TestInversePermutation() =

       let inv = Permutation.inverse TestData.ComboStructures.permutation
       let prod = Permutation.productR TestData.ComboStructures.permutation inv 
                        |> Result.ExtractOrThrow
       let id = Permutation.identity TestData.degree
       Assert.AreEqual(id, prod)


    [<TestMethod>]
    member this.TestMakeMonoCycle() =
       let degree = Degree.create "" 6 |> Result.ExtractOrThrow
       let rnd = Rando.LcgFromSeed (RandomSeed.fromInt 424)
       let tcp = TwoCyclePerm.makeMonoCycle degree 2 3 |> Result.ExtractOrThrow
       Assert.IsTrue(tcp |> TwoCyclePerm.toPermutation |> Permutation.isTwoCycle)


    [<TestMethod>]
    member this.TestMakeAllMonoCycles() =
       let degree = Degree.create "" 6 |> Result.ExtractOrThrow
       let rnd = Rando.LcgFromSeed (RandomSeed.fromInt 424)
       let tcA = TwoCyclePerm.makeAllMonoCycles degree |> Seq.toArray
       let tcASq = tcA |> Array.map (fun tcp -> 
                tcp |> TwoCyclePerm.toPermutation |> Permutation.isTwoCycle)
       Assert.AreEqual(tcA.Length, 15)
       for i in {0 .. tcASq.Length - 1} do
          Assert.IsTrue(tcASq.[i])


    [<TestMethod>]
    member this.TestMakeMakeRandomFullPolyCycle() =
       let degree = Degree.create "" 9 |> Result.ExtractOrThrow
       let rnd = Rando.LcgFromSeed (RandomSeed.fromInt 424)
       let id = TwoCyclePerm.identity degree
       for i in {0 .. 20} do
                let tcp = TwoCyclePerm.rndFullTwoCycle degree rnd
                Assert.IsTrue(tcp |> TwoCyclePerm.toPermutation |> Permutation.isTwoCycle)


    [<TestMethod>]
    member this.TestMakeMakeRandomPolyCycle() =
       let degree = Degree.create "" 9 |> Result.ExtractOrThrow
       let rnd = Rando.LcgFromSeed (RandomSeed.fromInt 424)
       let switchFreq = 0.5
       let id = TwoCyclePerm.identity degree
       for i in {0 .. 20} do
                let tcp = TwoCyclePerm.rndTwoCycle degree switchFreq rnd 
                Assert.IsTrue(tcp |> TwoCyclePerm.toPermutation |> Permutation.isTwoCycle)


    [<TestMethod>]
    member this.TwoCyclePerm_makeMode1() =
       let degree = Degree.fromInt 10
       let aa = TwoCycleGen.oddMode degree
       let ac = TwoCycleGen.oddModeWithCap degree
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
       let rnd = Rando.LcgFromSeed (RandomSeed.fromInt 424)
       let refSyms = Array.init 100 (fun _ ->
           TwoCyclePerm.rndSymmetric
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
        let rndy = Rando.LcgFromSeed (RandomSeed.fromInt 424)
        let switchFreq = SwitchFrequency.fromFloat 0.5
        let degree = Degree.fromInt 16
        let switchCount = SwitchCount.fromInt 8
        let stageTupes = seq {(0,1)}
        let twoCycle = TwoCyclePerm.makeFromTupleSeq degree stageTupes
        Assert.IsTrue (true)


    [<TestMethod>]
    member this.IntBits_FromInteger() =
     let len = 6
     let expectedArray = [|1; 1; 1; 0; 1; 0|]
     let converted = BitSet.fromInteger 6 23
     Assert.IsTrue ((expectedArray = converted.values))
     

    [<TestMethod>]
    member this.IntBits_FromAndToInteger() =
     let testInt = 123456
     let converted = BitSet.fromInteger 32 testInt
     let intBack = BitSet.toInteger converted
     Assert.AreEqual (testInt, intBack)

    [<TestMethod>]
    member this.trueBitCount64() =
        let gA = BitSet.create [|1;0;1;1;0;1;0;1;1;0;1;0;1;1;0;1;0;1;1;0;1;0;1;1;0;1;0;1;1;0;1;0;1;1;0;1;0;1;1;0;1|]
        let gB = BitSet.toUint64 gA
        let tc = ByteUtils.trueBitCount64 gB
        Assert.AreEqual(tc, 25)


    [<TestMethod>]
    member this.recordOn64() =
        let degree = Degree.fromInt 7
        let records = Record64Array.make degree
        let ress = Record64Array.recordPosition records
        Assert.AreEqual(1, 1)


    [<TestMethod>]
    member this.TwoCycleGen_evenMode() =
     let evenDegree = Degree.fromInt 16
     let resE = TwoCycleGen.evenMode evenDegree
     Assert.IsTrue ((TwoCyclePerm.arrayValues resE).Length = (Degree.value evenDegree))
     let oddDegree = Degree.fromInt 15
     let resO = TwoCycleGen.evenMode oddDegree
     Assert.IsTrue ((TwoCyclePerm.arrayValues resO).Length = (Degree.value oddDegree))


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
               BitSet.fromInteger (Degree.value degree)
                                   av)

     let theUints = BitsP32.zeroCreate (Degree.value degree)

     let bp32s = BitsP32.fromBitSets intBitsArray
                 |> Seq.toArray

     let bitsBack = BitsP32.toBitSets bp32s
                    |> Seq.toArray

     for i = 0 to (unEncodedVals.Length - 1) do
        BitsP32.stripeWrite theUints
                             intBitsArray.[i]
                             i
     let decodedVals = 
            bitsBack |> Array.map(BitSet.toInteger)
     let l = decodedVals.Length
     Assert.AreEqual (bitsBack.Length, l)
     for i = 0 to (unEncodedVals.Length - 1) do
         Assert.AreEqual (unEncodedVals.[i], decodedVals.[i])


    [<TestMethod>]
    member this.uIntBits_stripeRead() =
     let degree = Degree.fromInt 16
     let encodedVal = 8675
     let pos = 11
     let intBits = BitSet.fromInteger (Degree.value degree)
                                       encodedVal
     let blank = BitsP32.zeroCreate (Degree.value degree)
     BitsP32.stripeWrite blank
                         intBits
                         pos
     let bitsBack = BitsP32.stripeRead blank 
                                       pos
     let decoded = BitSet.toInteger bitsBack
     Assert.AreEqual (encodedVal, decoded)


    [<TestMethod>]
     member this.Record64Array_recordIntBiy() =
      let degree = Degree.fromInt 16
      let encodedVal = 8675
      let rec64Array = Record64Array.make degree

      let intBits = BitSet.fromInteger (Degree.value degree)
                                         encodedVal
      Record64Array.recordIntBits rec64Array intBits
      let intBitsBack = Record64Array.toBitSets degree rec64Array
                        |> Seq.head
      Assert.AreEqual (intBits, intBitsBack)


    [<TestMethod>]
     member this.VecP64() =
      let lhs = [|0UL .. 9UL|]
      let rhs = [|2048UL .. 2057UL|]
      let vot = Array.zeroCreate 100

      VecP64.aOr2 lhs rhs vot |> ignore

      Assert.AreEqual (1, 1)