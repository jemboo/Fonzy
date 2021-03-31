namespace global
open System.Collections.Generic
open Microsoft.FSharp.Core
open System
open System.Security.Cryptography
open System.Runtime.Serialization.Formatters.Binary
open System.IO

type NodeType =
    | Free
    | Switch


module NodeType = 
    let RandInit0 (randy:IRando) (switchThresh:float) =
        let rv = randy.NextFloat
        if (rv < switchThresh) then NodeType.Free
        else NodeType.Switch

type sTree<'T> =
    | Node of NodeType * sTree<'T> * sTree<'T>
    | Leaf of 'T


module sTree = 
    let rec init d o tg =
        match d with
        | 0 -> sTree.Leaf(o)
        | _ -> sTree.Node(tg(), 
                          init (d-1) (o) tg, 
                          init (d-1) (o + (pown 2 (d-1))) tg)

    let rec enumer sT =
        match sT with
        | sTree.Node (nt, l, r) -> 
                seq { yield! enumer l;  yield! enumer r }
        | sTree.Leaf (v) -> seq { yield v }


    let rec applyGuide (n:sTree<'T>) (gn:sTree<'T>) =
        match n, gn with
        | sTree.Node (nt, ln, rn), sTree.Node (gt, lg, rg) -> 
                match nt, gt with
                | NodeType.Free, NodeType.Free -> 
                    Node(nt, applyGuide ln lg, applyGuide rn rg)
                | NodeType.Switch, NodeType.Free ->
                    Node(nt, applyGuide ln lg, applyGuide rn rg)
                | NodeType.Free, NodeType.Switch ->
                    Node(nt, applyGuide ln lg, applyGuide rn rg)
                | NodeType.Switch, NodeType.Switch ->
                    Node(nt, applyGuide ln rg, applyGuide rn lg)
        | sTree.Leaf v, sTree.Leaf gv ->
                sTree.Leaf(gv)
        | _ , _ -> failwith "Node/Leaf mismatch"


    let rec applyGuide0 (n:sTree<'T>) (gn:sTree<'T>) =
        match n, gn with
        | sTree.Node (nt, ln, rn), sTree.Node (gt, lg, rg) -> 
                match nt with
                | NodeType.Free -> Node(nt, applyGuide0 ln lg, applyGuide0 rn rg)
                | NodeType.Switch ->
                    Node(nt, applyGuide0 ln rg, applyGuide0 rn lg)
                | _ -> failwith "wrong NodeType"
        | sTree.Leaf v, sTree.Leaf gv ->
                sTree.Leaf(gv)
        | _ , _ -> failwith "Node/Leaf mismatch"


//type BranchType =
//    | NoSwitch
//    | Switch


//module BranchType = 
//    let RandInit0 (randy:IRando) =
//        let rv = randy.NextFloat
//        if (rv < 0.8) then BranchType.NoSwitch
//        else BranchType.Switch


//type NoB<'T> =
//    | Node of NodeType * NoB<'T> * NoB<'T>
//    | Branch of BranchType * 'T * 'T

   

//module NoB = 
//    let rec enumerLeaves n =
//        match n with
//        | NoB.Node (nt, l, r) -> 
//                seq { yield! enumerLeaves l;  yield! enumerLeaves r }
//        | NoB.Branch (bt, l, r) ->
//                seq { yield l; yield r }

//    let rec enumerBranches n =
//        match n with
//        | NoB.Node (nt, l, r) -> 
//                seq { yield! enumerBranches l;  yield! enumerBranches r }
//        | NoB.Branch (bt, l, r) -> seq { yield NoB.Branch(bt, l, r) }


//    let rec randInit0 (randy:IRando) (n)=
//        match n with
//        | NoB.Node (nt, l, r) -> 
//                Node(NodeType.RandInit0 randy, 
//                      l |> randInit0 randy, r |> randInit0 randy)
//        | NoB.Branch (bt, l, r) ->
//                Branch(BranchType.RandInit0 randy, l, r)
                

//    let SwichNode (n:NoB<'T>) =
//        match n with
//        | NoB.Node (nt, l, r) -> Node(nt, r, l)
//        | NoB.Branch (bt, l, r) -> Branch(bt, r, l)
            

//    let MixNode (n:NoB<'T>) =
//        match n with
//        | NoB.Node (nt, l, r) -> Node(nt, r, l |> SwichNode)
//        | _ -> failwith "cant mix a branch"


//    let rec applyGuide (n:NoB<'T>) (gn:NoB<'T>) =
//        match n, gn with
//        | NoB.Node (nt, ln, rn), NoB.Node (gt, lg, rg) -> 
//                match nt with
//                | NodeType.Free -> Node(nt, applyGuide ln lg, applyGuide rn rg)
//                | NodeType.Slider ->
//                    Node(nt, applyGuide ln rg, applyGuide rn lg)
//                | _ -> failwith "wrong NodeType"
//        | NoB.Branch (bt, lb, rb), NoB.Branch (gt, lg, rg) ->
//                match bt with
//                | BranchType.NoSwitch -> Branch(bt, lg, rg)
//                | BranchType.Switch -> Branch(bt, rb, lb)
//                | _ -> failwith "wrong BranchType"
//        | _ , _ -> failwith "Node/Branch mismatch"



type Tree<'T> = 
    | Node of Tree<'T> * Tree<'T>
    | Leaf of 'T
    // override x.ToString() = (...)


type Path<'T> = 
  | Top 
  | Left of Path<'T> * Tree<'T>
  | Right of Path<'T> * Tree<'T>
 // override x.ToString() = (...)

type TreeZipper<'T> = 
  | TZ of Tree<'T> * Path<'T>
 // override x.ToString() = (...)
 /// Navigates to the left sub-tree

 module TreeZipper =
    let left = function
    | TZ(Leaf _, _) -> failwith "cannot go left"
    | TZ(Node(l, r), p) -> TZ(l, Left(p, r))
 
    /// Navigates to the right sub-tree
    let right = function
    | TZ(Leaf _, _) -> failwith "cannot go right"
    | TZ(Node(l, r), p) -> TZ(r, Right(p, l))
 
    /// Gets the value at the current position
    let current = function
    | TZ(Leaf x, _) -> x
    | _ -> failwith "cannot get current"

    let edit nv = function
    | TZ(Leaf x, y) -> TZ(Leaf nv, y)
    | _ -> failwith "cannot edit Node"

    let up = function
    | TZ(l, Left(p, r))
    | TZ(r, Right(p, l)) -> TZ(Node(l, r), p)
    | TZ(_, Top) -> failwith "cannot go up"

    // Navigate to the root of the tree
    let rec top = function
    | TZ(_, Top) as t -> t
    | tz -> top (up tz)

    let unit v = TZ(Leaf v, Top)
    
    /// Transform leaves in the current sub-tree of 'treeZip'
    /// into other trees using the provided function 'f'
    let bindSub f treeZip = 
      let rec bindT = function
        | Leaf x -> let (TZ(t, _)) = top (f x) in t
        | Node(l, r) -> Node(bindT l, bindT r)
      let (TZ(current, path)) = treeZip
      TZ(bindT current, path)


    type TreeZipperBuilder() = 
        /// Enables the 'for x in xs do ..' syntax
        member x.For(tz:TreeZipper<'T>, f) : TreeZipper<'T> = bindSub f tz
        /// Enables the 'yield x' syntax
        member x.Yield(v) = unit v
      
    /// Global instance of the computation builder
    let tree = TreeZipperBuilder()

    type TreeZipperBuilder with
    // Operations for navigation through the tree
    [<CustomOperation("left", MaintainsVariableSpace=true)>]
    member x.Left(tz) = left tz
    [<CustomOperation("right", MaintainsVariableSpace=true)>]
    member x.Right(tz) = right tz
    [<CustomOperation("up", MaintainsVariableSpace=true)>]
    member x.Up(tz) = up tz
    [<CustomOperation("top", MaintainsVariableSpace=true)>]
    member x.Top(tz) = top tz

    /// Extracts the current value and returns it
    [<CustomOperation("current", MaintainsVariableSpace=false)>]
    member x.Current(tz) = current tz

    /// Transform the current sub-tree using 'f'
    [<CustomOperation("map", MaintainsVariableSpace=true)>]
    member x.Select(tz, [<ProjectionParameter>] f) = bindSub (f >> unit) tz
