namespace Fonzy.core.test
open Microsoft.VisualStudio.TestTools.UnitTesting
open System
open System.Collections.Generic
open TreeZipper

module testData =
    let tree1 = 
      Tree.Node (1, 
        Tree.Node (2, 
          Tree.Node (4, Empty, Empty), 
          Tree.Node (5, Empty, Empty)), 
        Tree.Node (3, 
          Tree.Node (6, 
            Tree.Node (7, Empty, Empty),
            Empty), 
          Tree.Node (8, Empty, Empty)))

    let tree7 =
      Tree.Node (7, 
        Tree.Node (3, 
          Tree.Node (2, Empty, Empty),
          Tree.Node (5, 
            Tree.Node (4, Empty, Empty), 
            Tree.Node (6, Empty, Empty))), 
        Tree.Node (9, 
          Tree.Node (8, Empty, Empty), 
          Tree.Node (10, Empty, Empty)))


[<TestClass>]
type TreeFixture () =

    [<TestMethod>]
    member this.t0() =
        Tree.traverseB testData.tree1 
            |> Seq.iter (printfn "%d") // 1 2 4 5 3 6 7 8

        Assert.AreEqual(1, 1)

    [<TestMethod>]
    member this.t0a() =
        let treeD = 
            Tree.delete 7 testData.tree7

        Assert.AreEqual(1, 1)

    [<TestMethod>]
    member this.t0b() =
        let treeD = 
            Tree.Empty
            |> Tree.insert 2
            |> Tree.insert 1
            |> Tree.insert 5
            |> Tree.insert 4

        Assert.AreEqual(1, 1)

    [<TestMethod>]
    member this.t0c() =
        Tree.traverseD testData.tree1 
            |> Seq.iter (printfn "%d") // 1 2 4 5 3 6 7 8

        Tree.traverseB testData.tree1 
            |> Seq.iter (printfn "%d") 

        Assert.AreEqual(1, 1)



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
        let ats = Array.init 100000 (fun _ -> sTree.init 4 0 tg)
        let rese = ats |> Array.map(fun t -> sTree.applyGuide t t)
                       |> Array.map (fun t-> t |> sTree.enumer |> Seq.toArray)

        let hist = CollectionUtils.histogram (id) rese
                   |> Map.toSeq |> Seq.sortBy(fun tup -> -(snd tup)) |> Seq.toArray

       //let rep = StringUtils.printSeqToRow (hist |> Seq.map(snd))

        let tcps = rese |> Array.map(Combinatorics.fixedCount)
                        |> CollectionUtils.histogram (id)
        let fxTup = tcps |> Map.toArray
        //let fxRep = fxTup |> StringUtils.printSeqfToColumn 
        //                     (fun tup -> (sprintf "%d\t%d" (fst tup) (snd tup)))

        //Console.WriteLine fxRep
       // Console.WriteLine rep
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




