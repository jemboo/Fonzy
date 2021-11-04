namespace Fonzy.world.test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open SortingEval

[<TestClass>]
type SortableSetMakerFixture () =

    [<TestMethod>]
    member this.SortableSetSpecReduced_none() =
        let degree = Degree.fromInt 16

        let sstAllIntSets = sortableSetType.AllForDegree 
                                   (sortableSetRep.Integer degree)
                               
        let ssAllIntSets = SortableSetMaker.makeNoRepo sstAllIntSets
                            |> Result.ExtractOrThrow
    
        let trimAllInt = ssAllIntSets.sortableSetImpl 
                            |> SortableSetImpl.getIntSets 
                            |> Seq.length


        let sstAllBp64Sets = sortableSetType.AllForDegree 
                                   (sortableSetRep.Bp64 degree)

        let ssAllBp64Sets = SortableSetMaker.makeNoRepo sstAllBp64Sets
                            |> Result.ExtractOrThrow

        let trimAllBp64 = ssAllBp64Sets.sortableSetImpl 
                            |> SortableSetImpl.getIntSets 
                            |> Seq.length

        Assert.AreEqual (trimAllInt - 1, trimAllBp64)


    [<TestMethod>]
    member this.SortableSetSpecReduced_comp() =
        let degree = Degree.fromInt 8
        let prefixNot = []
        let prefix1a = [{Switch.hi=3; low=0;}]
        let prefix1b = [{Switch.hi=2; low=1;}]
        let prefix2 = [{Switch.hi=3; low=0;}; {Switch.hi=2; low=1;}]


        let sstAllIntSets = sortableSetType.AllForDegree 
                                   (sortableSetRep.Integer degree)

        let sstAllBp64Sets = sortableSetType.AllForDegree 
                                   (sortableSetRep.Bp64 degree)


        let sstTrimIntSets = sortableSetType.SwitchReduced  
                                    (sstAllIntSets, prefix1a)

        let ssTrimIntSets = SortableSetMaker.makeNoRepo sstTrimIntSets
                            |> Result.ExtractOrThrow

        let trimInt = ssTrimIntSets.sortableSetImpl 
                            |> SortableSetImpl.getIntSets 
                            |> Seq.length


        let sstTrimBp64Sets = sortableSetType.SwitchReduced  
                                (sstAllBp64Sets, prefix1a)

        let ssTrimBp64Sets = SortableSetMaker.makeNoRepo sstTrimBp64Sets
                            |> Result.ExtractOrThrow

        let trimBp64 = ssTrimBp64Sets.sortableSetImpl 
                            |> SortableSetImpl.getIntSets 
                            |> Seq.length

        Assert.AreEqual (trimInt - 1, trimBp64)
                      



    [<TestMethod>]
    member this.SortableSetSpecReduced_make() =
        let degree = Degree.fromInt 4
        let prefix1a = [{Switch.hi=3; low=0;}]
        let prefix1b = [{Switch.hi=2; low=1;}]
        let prefix2 = [{Switch.hi=3; low=0;}; {Switch.hi=2; low=1;}]


        let sstAllBitSets = sortableSetType.AllForDegree 
                                   (sortableSetRep.Integer degree)

        let ssAllBitSets = SortableSetMaker.makeNoRepo sstAllBitSets
                            |> Result.ExtractOrThrow
                            
        let sstRedx1a = sortableSetType.SwitchReduced  
                                    (sstAllBitSets, prefix1a)
        let ssRedx1a = SortableSetMaker.makeNoRepo sstRedx1a
                             |> Result.ExtractOrThrow


        let sstRedx1ab = sortableSetType.SwitchReduced  
                                    (sstRedx1a, prefix1b)
        let ssRedx1ab = SortableSetMaker.makeNoRepo sstRedx1ab
                            |> Result.ExtractOrThrow

        let sstRedx2 = sortableSetType.SwitchReduced  
                                    (sstAllBitSets, prefix2)
        let ssRedx2 = SortableSetMaker.makeNoRepo sstRedx2
                             |> Result.ExtractOrThrow
                             
        let trim1a = ssRedx1a.sortableSetImpl |> SortableSetImpl.getIntSets |> Seq.toArray

        let trim1ab = ssRedx1ab.sortableSetImpl |> SortableSetImpl.getIntSets |> Seq.toArray 
        let trim2 = ssRedx2.sortableSetImpl |> SortableSetImpl.getIntSets |> Seq.toArray 

        Assert.AreEqual (trim1ab.Length, trim2.Length)


    [<TestMethod>]
    member this.SortableSetSpecReduced_makeT() =
        let degree = Degree.fromInt 4
        let prefix1a = [{Switch.hi=2; low=0;}]
        let prefix1b = [{Switch.hi=1; low=2;}]
        let prefix2 = [{Switch.hi=2; low=0;}; {Switch.hi=1; low=2;}]


        let sstAllBitSets = sortableSetType.AllForDegree 
                                   (sortableSetRep.Integer degree)
    
        let sstRedx1a = sortableSetType.SwitchReduced  
                                    (sstAllBitSets, prefix1a)
        let ssRedx1a, uses1a = SortableSetMaker.makeTNoRepo sstRedx1a
                                |> Result.ExtractOrThrow

        let sstRedx1ab = sortableSetType.SwitchReduced  
                                    (sstRedx1a, prefix1b)
        let ssRedx1ab, uses1ab = SortableSetMaker.makeTNoRepo sstRedx1ab
                                 |> Result.ExtractOrThrow

        let sstRedx2 = sortableSetType.SwitchReduced  
                                    (sstAllBitSets, prefix2)
        let ssRedx2, uses2 = SortableSetMaker.makeTNoRepo sstRedx2
                             |> Result.ExtractOrThrow
                         
        let trim1a = ssRedx1a.sortableSetImpl |> SortableSetImpl.getIntSets |> Seq.toArray

        let trim1ab = ssRedx1ab.sortableSetImpl |> SortableSetImpl.getIntSets |> Seq.toArray
        let trim2 = ssRedx2.sortableSetImpl |> SortableSetImpl.getIntSets |> Seq.toArray

        Assert.AreEqual (trim1ab.Length, trim2.Length)
        Assert.AreEqual (uses1ab.weights.Length, uses2.weights.Length)



    [<TestMethod>]
    member this.SortableSetSpecReduced_makeMemoize() =
        let degree = Degree.fromInt 16
        let prefix1a = [{Switch.hi=3; low=0;}]
        let prefix2 = [{Switch.hi=3; low=1;}]
        
        let sstAllBitSets = sortableSetType.AllForDegree 
                                   (sortableSetRep.Integer degree)


        let ssAbs =  SortableSetMaker.makeMemoizeNoRepo sstAllBitSets
                        |> Result.ExtractOrThrow

        let sstRedx1a = sortableSetType.SwitchReduced  
                                    (sstAllBitSets, prefix1a)


        let ssRedx1a =  SortableSetMaker.makeMemoizeNoRepo sstRedx1a
                        |> Result.ExtractOrThrow

        let ssAbs2 =  SortableSetMaker.makeMemoizeNoRepo sstAllBitSets
                        |> Result.ExtractOrThrow

        //let ssGen = SortableSetGen.allBp64 degree
        //            |> sortableSetSpec.Generated

        //let ssetT1a, wUsesT1a = 
        //    (ssGen, prefix1) |> sortableSetSpecReduced
        //                     |> SortableSetSpecReduced.makeMemoize
        //                     |> Result.ExtractOrThrow

        //let ssetT1b, wUsesT1b = 
        //    (ssGen, prefix1) |> sortableSetSpecReduced
        //                     |> SortableSetSpecReduced.makeMemoize
        //                     |> Result.ExtractOrThrow

        //let ssetT2, wUsesT2 = 
        //    (ssGen, prefix2) |> sortableSetSpecReduced
        //                     |> SortableSetSpecReduced.makeMemoize
        //                     |> Result.ExtractOrThrow

        Assert.IsTrue(true)


