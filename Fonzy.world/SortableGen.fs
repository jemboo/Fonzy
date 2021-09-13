namespace global

module SortableSetMaker = 
    let rndIntBits (ssr:sortableSetRep) 
                   (degree:Degree) 
                   (rngGen:RngGen) 
                   (sortableCount:SortableCount) = 
        let rando = rngGen |> Rando.fromRngGen
        let intSet() = 
            IntSet.createRandoms degree 
                                 rando
            |> Seq.take (SortableCount.value sortableCount)
            |> Seq.toArray

        let bitSet() = 
            BitSet.createRandoms degree 
                                 rando
            |> Seq.take (SortableCount.value sortableCount)
            |> Seq.toArray

        let bitsP64Set() = 
            BitsP64.createRandoms degree 
                                  rando 
                                  (SortableCount.value sortableCount)
            |> Seq.toArray

        match ssr with
        | sortableSetRep.Binary d -> (bitSet(), d) |> sortableSetImpl.Binary
        | sortableSetRep.Integer d -> (intSet(), d) |> sortableSetImpl.Integer
        | sortableSetRep.Bp64 d -> (bitsP64Set(), d) |> sortableSetImpl.Bp64


    let allZeroOnes (ssr:sortableSetRep) = 
        let degree = (ssr |> SortableSetRep.getDegree)
        
        let ssid = ([("allZeroOnes" :> obj); (degree :> obj)]) 
                   |> GuidUtils.guidFromObjList
                   |> SortableSetId.fromGuid
        let intSet() = 
            IntSet.arrayOfAllFor degree
            |> Seq.toArray

        let bitSet() = 
            (BitSet.arrayOfAllFor degree)
            |> Seq.toArray

        let bitsP64Set() = 
            (BitsP64.arrayOfAllFor degree)
            |> Seq.toArray

        match ssr with
        | sortableSetRep.Binary d -> 
            (bitSet(), d) |> sortableSetImpl.Binary
        | sortableSetRep.Integer d -> 
            (intSet(), d) |> sortableSetImpl.Integer
        | sortableSetRep.Bp64 d -> 
            (bitsP64Set(), d) |> sortableSetImpl.Bp64



    let rec makeT (ssType:sortableSetType) = 
        match ssType with
        | sortableSetType.AllForDegree ssr -> 
                (allZeroOnes ssr, {switchUses.weights =[||] }) |> Ok
        | sortableSetType.Explicit ssid ->
                "sortableSetType.Explicit is Not supported" |> Error
        | sortableSetType.Random (rng, sc, ssr) ->
            let degree = ssr |> SortableSetRep.getDegree
            (rndIntBits ssr degree rng sc, {switchUses.weights =[||] })  |> Ok
        | sortableSetType.SwitchReduced (sst, swPfx) ->
            result {
                let! toChop = makeT sst
                let! res = SortingOps.SortableSet.switchReduce
                                            (fst toChop)
                                            (swPfx |> List.toArray)
                return res
            }

    let rec make (repo: (SortableSetId->sortableSetImpl) option) 
                 (ssType:sortableSetType) = 
        let _lookup ssid =
            match repo with
            | Some f -> f ssid |> Ok
            | None -> "repo missing" |> Error
        match ssType with
        | sortableSetType.AllForDegree ssr -> 
                allZeroOnes ssr |> Ok
        | sortableSetType.Explicit ssid ->
                _lookup ssid
        | sortableSetType.Random (rng, sc, ssr) ->
            let degree = ssr |> SortableSetRep.getDegree
            rndIntBits ssr degree rng sc  |> Ok
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

    let makeMemoize (repo: (SortableSetId->sortableSetImpl) option)  =
        FuncUtils.memoization (make repo)