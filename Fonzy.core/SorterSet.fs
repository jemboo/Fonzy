namespace global
open System

type SorterSet = { id:SorterSetId; 
                   degree:Degree; 
                   sorterCount:SorterCount; 
                   sorters:Map<SorterId,Sorter> }
module SorterSet =
    let fromSorters (sorterSetId:SorterSetId)
                    (degree:Degree) 
                    (sorters:seq<Sorter>) =
        let sorterArray = 
                sorters 
                |> Seq.map(fun s-> (s |> Sorter.makeId, s))
                |> Map.ofSeq
        {
            SorterSet.id =sorterSetId;
            degree=degree; 
            sorterCount= SorterCount.fromInt sorterArray.Count; 
            sorters = sorterArray
        }

    // IRando dependent
    let createRandom (sorterSetId:SorterSetId)
                     (degree:Degree) 
                     (switchOrStageCount:SwitchOrStageCount) 
                     (switchFreq:SwitchFrequency)
                     (sorterCount:SorterCount) 
                     (rnd:IRando) =
        fromSorters
            sorterSetId
            degree 
            (Array.init (SorterCount.value sorterCount)
                       (fun _ -> (SorterGen.createRandom 
                                    degree switchOrStageCount switchFreq rnd)))