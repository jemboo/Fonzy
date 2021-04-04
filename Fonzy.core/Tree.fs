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
    let rec init d o ntGen =
        match d with
        | 0 -> sTree.Leaf(o)
        | _ -> sTree.Node(ntGen(), 
                          init (d-1) (o) ntGen, 
                          init (d-1) (o + (pown 2 (d-1))) ntGen)

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

    let makePerm (nswFreq:float) (randy:IRando) (logDegree:int) =
        let degree = Degree.fromInt (pown 2 logDegree)
        let ntGen() =
            NodeType.RandInit0 randy nswFreq
        let sT = init logDegree 0 ntGen
        applyGuide sT sT |> enumer |> Seq.toArray
                         |> TwoCyclePerm.create degree


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
