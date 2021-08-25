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

    // IRando dependent
    //let createRandom (sorterSetId:SorterSetId)
    //                 (sorterGen:SorterGen) 
    //                 (sorterCount:SorterCount) 
    //                 (rnd:IRando) =
    //    fromSorters
    //        sorterSetId
    //        (sorterGen|> SorterGen.getDegree)
    //        (Array.init (SorterCount.value sorterCount)
    //                   (fun _ -> (SorterGen.createRandom 
    //                                            sorterGen rnd)))