namespace global
open System

type sorterSet = { id:SorterSetId; 
                   degree:Degree; 
                   sorterCount:SorterCount; 
                   sorters:Map<SorterId, sorter> }
module SorterSet =

    let fromSorters (sorterSetId:SorterSetId)
                    (degree:Degree) 
                    (sorters:seq<sorter>) =
        let sorterArray = 
                sorters 
                |> Seq.map(fun s-> (s |> Sorter.makeId, s))
                |> Map.ofSeq
        {
            sorterSet.id =sorterSetId;
            degree=degree; 
            sorterCount= SorterCount.fromInt sorterArray.Count; 
            sorters = sorterArray
        }


type sorterSetGen =
    | Mutate of sorterMutType * sorter * RngGen * SorterCount
    | Rnd of sorterRndGen * RngGen * SorterCount
    | Repo of SorterSetId


module SorterSetGen =

    let createSorterSet (repo: (SorterSetId->sorterSet) option) 
                        (sgen:sorterSetGen) =

        let _lookup ssid =
            match repo with
            | Some f -> f ssid |> Ok
            | None -> "repo missing" |> Error

        let _rgs srg rg sc =
            let randy = rg |> Rando.fromRngGen
            let sa = SorterRndGen.createRandomArray srg sc randy
            let ssId = seq { srg:> obj; rg:> obj; } 
                         |> GuidUtils.guidFromObjs
                         |> SorterSetId.fromGuid
            let degree = srg |> SorterRndGen.getDegree
            SorterSet.fromSorters ssId degree sa

        let _mut smt str rg sc =
            let randy = rg |> Rando.fromRngGen
            let sa = seq {0 .. ((SorterCount.value sc) - 1)}
                      |> Seq.map(fun _ -> SorterMutate.mutate smt randy str)
            let ssId = seq { smt:> obj; str:> obj; rg:> obj; sc:> obj;} 
                         |> GuidUtils.guidFromObjs
                         |> SorterSetId.fromGuid
            SorterSet.fromSorters ssId str.degree sa

        match sgen with
        | Mutate (smt, str, rg, sc) -> (_mut smt str rg sc) |> Ok
        | Rnd (srg, rg, sc) -> ( _rgs srg rg sc ) |> Ok
        | Repo ssid -> _lookup ssid