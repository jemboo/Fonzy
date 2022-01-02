namespace global

module SortableSetMaker = 
    let rndIntBits (ssr:sortableSetRep) 
                   (degree:Degree) 
                   (rngGen:RngGen) 
                   (sortableCount:SortableCount) = 
        let rando = rngGen |> Rando.fromRngGen
        let _intSet() = 
            IntSet.createRandoms degree 
                                 rando
            |> Seq.take (SortableCount.value sortableCount)
            |> IntSetsRollout.fromIntSets degree

        let _bitSet() = 
            BitSet.createRandoms degree 
                                 rando
            |> Seq.take (SortableCount.value sortableCount)
            |> IntSetsRollout.fromBitSet degree

        let _bitsP64Set() = 
            BitsP64.createRandoms degree 
                                  rando 
                                  (SortableCount.value sortableCount)
            |> BP64SetsRollout.fromBitsP64 degree

        match ssr with
        | sortableSetRep.Binary d ->
            result {
                 let! bst = _bitSet()
                 return bst |> sortableSetImpl.Binary
                 }
        | sortableSetRep.Integer d ->
            result {
                  let! ist =  _intSet()
                  return ist |> sortableSetImpl.Integer
                 }
        | sortableSetRep.Bp64 d -> 
            result { 
                     let! bs = _bitsP64Set() 
                     return bs |> sortableSetImpl.Bp64
                   }


    let allZeroOnes (ssr:sortableSetRep) = 
        let degree = (ssr |> SortableSetRep.getDegree)
        
        let ssid = ([("allZeroOnes" :> obj); (degree :> obj)]) 
                   |> GuidUtils.guidFromObjs
                   |> SortableSetId.fromGuid
        let _intSet() = 
            IntSet.arrayOfAllFor degree
            |> IntSetsRollout.fromIntSets degree

        let _bitSet() = 
            (BitSet.arrayOfAllFor degree)
            |> IntSetsRollout.fromBitSet degree

        let _bitsP64Set() = 
            (BitsP64.arrayOfAllFor degree)
            |> BP64SetsRollout.fromBitsP64 degree

        match ssr with
        | sortableSetRep.Binary d -> 
            result {
                 let! bst = _bitSet()
                 return bst |> sortableSetImpl.Binary
                 }
        | sortableSetRep.Integer d -> 
            result {
                  let! ist =  _intSet()
                  return ist |> sortableSetImpl.Integer
                 }
        | sortableSetRep.Bp64 d -> 
            result { 
                     let! bs = _bitsP64Set() 
                     return bs |> sortableSetImpl.Bp64
                   }


    let binaryMerges (degs:Degree list) 
                     (ssr:sortableSetRep) = 
        
        let _bitSet() = 
            (BitSet.stackSortedBlocks degs)
            |> Seq.toArray

        match ssr with
        | sortableSetRep.Binary d -> 
            result {
                 let! bst = _bitSet()
                            |> IntSetsRollout.fromBitSet d
                 return bst |> sortableSetImpl.Binary
                 }
        | sortableSetRep.Integer d -> 
            result {
                 let! bst = _bitSet()
                            |> IntSetsRollout.fromBitSet d
                 return bst |> sortableSetImpl.Integer
                 }
        | sortableSetRep.Bp64 d -> 
            result { 
                     let! bs = _bitSet()
                                |> BitsP64.fromBitSet 
                                |> BP64SetsRollout.fromBitsP64 d
                     return bs |> sortableSetImpl.Bp64
                   }



    let rec makeT (repo: (SortableSetId->sortableSetImpl) option) 
                  (ssType:sortableSetType) = 

        let _lookup ssid =
            match repo with
            | Some f -> f ssid |> Ok
            | None -> "repo missing" |> Error

        match ssType with
        | sortableSetType.BinaryMerge (degs, ssr) ->
            result {
                let! bImpl = binaryMerges degs ssr
                return (bImpl, {switchUses.weights = [||] })
            }
        | sortableSetType.AllForDegree ssr -> 
            result {
                let! bImpl = allZeroOnes ssr
                return (bImpl, {switchUses.weights = [||] })
            }
        | sortableSetType.Explicit ssid ->
            result {
                let! sset = _lookup ssid
                return (sset, {switchUses.weights = [||] }) 
            }
        | sortableSetType.Random (rng, sc, ssr) ->
            let degree = ssr |> SortableSetRep.getDegree
            result {
                let! rndBitsImpl = rndIntBits ssr degree rng sc
                return (rndBitsImpl, {switchUses.weights = [||] }) 
            }
        | sortableSetType.SwitchReduced (sst, swPfx) ->
            result {
                let! implB, wUsesB = makeT repo sst
                let! implR, wUsesR  = SortingOps.SortableSet.switchReduce
                                             implB
                                            (swPfx |> List.toArray)
                return (implR, wUsesB |> SwitchUses.append wUsesR)
            }


    let rec make (repo: (SortableSetId->sortableSetImpl) option) 
                 (ssType:sortableSetType) = 
        let _lookup ssid =
            match repo with
            | Some f -> f ssid |> Ok
            | None -> "repo missing" |> Error
        match ssType with
        | sortableSetType.BinaryMerge (degs, ssr) ->
            binaryMerges degs ssr
        | sortableSetType.AllForDegree ssr -> 
                allZeroOnes ssr
        | sortableSetType.Explicit ssid ->
                _lookup ssid
        | sortableSetType.Random (rng, sc, ssr) ->
            let degree = ssr |> SortableSetRep.getDegree
            rndIntBits ssr degree rng sc
        | sortableSetType.SwitchReduced (sst, swPfx) ->
            result {
                let! toChop = make repo sst
                let! res = SortingOps.SortableSet.switchReduce
                                            toChop
                                            (swPfx |> List.toArray)
                return fst res
            }

    let makeNoRepo (sortableSetType:sortableSetType) =
        SortableSet.make (make None) sortableSetType

    let makeTNoRepo (sortableSetType:sortableSetType) =
        SortableSet.makeT (makeT None) sortableSetType

    let makeMemoize (repo: (SortableSetId->sortableSetImpl) option)  =
        FuncUtils.memoization (make repo)
        
    let makeMemoizeNoRepo =
        makeMemoize None