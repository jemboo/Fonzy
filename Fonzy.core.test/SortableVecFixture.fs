namespace Fonzy.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open System
open System.Collections.Generic

[<TestClass>]
type SortableVecFixture () =
    // FastUtils
    [<TestMethod>]
    member this.contains() =
        let res = FastUtils.contains 900UL [|1UL..1000UL|]
        Assert.IsTrue(res)


    [<TestMethod>]
    member this.fromIntBits() =
        let sc = SortableCount.fromInt 100
        let intBits = IntBits.createRandoms
                        TestData.degree
                        TestData.iRando
                      |> Seq.take (SortableCount.value sc)
                      |> Seq.toList
        let svec = SortableVec.fromIntBits 
                            TestData.degree
                            intBits
                
        Assert.AreEqual(svec.vecLines.Length, (Degree.value TestData.degree))
        Assert.AreEqual(svec.vecLines.[0].values.Length, 2)


    [<TestMethod>]
    member this.toIntBits() =
        let sc = SortableCount.fromInt 100
        let intBits = IntBits.createRandoms
                        TestData.degree
                        TestData.iRando
                      |> Seq.take (SortableCount.value sc)
                      |> Seq.toList
        let svec = SortableVec.fromIntBits 
                            TestData.degree
                            intBits

        let intBitsBack = svec |> SortableVec.toIntBits
                           |> Seq.toList

        Assert.AreEqual(intBits, intBitsBack)