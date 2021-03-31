namespace Fonzy.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open System
open System.Collections.Generic
open TreeZipper

[<TestClass>]
type TreeFixture () =

    [<TestMethod>]
    member this.t1() =
        // Create a sample tree
        let branches = 
          Node( Node(Leaf 1, Leaf 3), 
                Node(Leaf 7, Node(Leaf 12, Leaf 20)) )
    
        // Wrap it as a zipper & print
        let sample = TZ(branches, Top)
        printfn "%O" sample 
        Assert.AreEqual(1, 1)


    [<TestMethod>]
    member this.t2() =
        let rec yy n =
            match n with
            | Node (l,r) -> Node(yy l, yy r)
            | Leaf v -> Leaf(v+1)


        let branches = 
          Node( Node(Leaf 1, Leaf 3), 
                Node(Leaf 7, Node(Leaf 12, Leaf 20)) )
    
        // Wrap it as a zipper & print
        let sample = yy branches
        printfn "%O" sample 
        Assert.AreEqual(1, 1)





    [<TestMethod>]
    member this.sTree_init() =
        let tg() = NodeType.RandInit0 TestData.iRando 0.29
        let res = sTree.init 4 0 tg
        let ats = Array.init 100000 (fun _ -> sTree.init 4 0 tg)
        let rese = ats |> Array.map(fun t -> sTree.applyGuide t t)
                       |> Array.map (fun t-> t |> sTree.enumer |> Seq.toArray)

        let hist = CollectionUtils.histogram (id) rese
                   |> Map.toSeq |> Seq.sortBy(fun tup -> -(snd tup)) |> Seq.toArray

        let rep = StringUtils.printIntArray (hist |> Array.map(snd))

        let tcps = rese |> Array.map(Combinatorics.fixedCount)
                        |> CollectionUtils.histogram (id)
        let fxTup = tcps |> Map.toArray
        let fxRep = fxTup |> StringUtils.printLinesOfArrayf 
                             (fun tup -> (sprintf "%d\t%d" (fst tup) (snd tup)))

        Console.WriteLine fxRep
        Console.WriteLine rep
        Assert.AreEqual(1, 1)


    [<TestMethod>]
    member this.sTree_applyGuide() =
        let testNoB = sTree.Node(
                        NodeType.Switch, 
                        sTree.Node(
                            NodeType.Free, 
                            sTree.Node(
                                NodeType.Switch, 
                                sTree.Leaf(0), 
                                sTree.Leaf(1)), 
                            sTree.Node(
                                NodeType.Switch, 
                                sTree.Leaf(2), 
                                sTree.Leaf(3))), 
                        sTree.Node(
                            NodeType.Free, 
                            sTree.Node(
                                NodeType.Switch, 
                                sTree.Leaf(4), 
                                sTree.Leaf(5)), 
                            sTree.Node(
                                NodeType.Switch, 
                                sTree.Leaf(6), 
                                sTree.Leaf(7))))

        let gres = testNoB |> sTree.applyGuide testNoB
        let tVals = gres |> sTree.enumer |> Seq.toList
        Assert.AreEqual(1, 1)


    [<TestMethod>]
    member this.sTree_enumer() =
        //let testNoB = NoB.Node(
        //                NodeType.UnInit, 
        //                NoB.Node(
        //                    NodeType.UnInit, 
        //                    Branch(BranchType.UnInit,(0,0),(1,1)), 
        //                    Branch(BranchType.UnInit,(2,2),(3,3))), 
        //                Branch(BranchType.UnInit,(4,4),(5,5)))

        //let res = testNoB |> NoB.enumerLeaves
        //                  |> Seq.toArray
        Assert.AreEqual(1, 1)




