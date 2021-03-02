namespace global
open System

type MergeMapItem = {sourceMapName:string; sourceMapKey:string; outputKey:string;}
module MergeMapItem = 
    module Sorting =
        let mergeSortersWithSortables =
            [{MergeMapItem.sourceMapName = "sorterWorld"; 
              sourceMapKey = "sorters"; 
              outputKey = "sorters"};
             {MergeMapItem.sourceMapName = "sortableWorld"; 
              sourceMapKey = "sortables"; 
              outputKey = "sortables"};]

type WorldMerges = {id:Guid; sourceNameMap: Map<string,Guid>; 
                   mergeMapItems:MergeMapItem list; enviro:Enviro}
module WorldMerges = 

    let mergeWorlds (mergedWorldId:Guid) (sourceWorlds:Map<string,World>) 
                    (mergeMapItems:MergeMapItem list) (mergedEnviro:Enviro) =

        let procMergeMapItem (mergeMapItem:MergeMapItem) 
                             (sourceWorlds:Map<string,World>) 
                             (destMapR:Result<Map<string,string>, string>) =
            result {
                let! sourceWorld = ResultMap.read mergeMapItem.sourceMapName 
                                                  sourceWorlds
                let! sourceMap = sourceWorld.enviro |> Enviro.toMap
                let! sourceItem = ResultMap.read mergeMapItem.sourceMapKey
                                                 sourceMap
                let! destMap = destMapR
                let! destMapPlus = ResultMap.add mergeMapItem.outputKey 
                                                 sourceItem
                                                 destMap
                return destMapPlus
            }

        result {
            let destMapR = mergedEnviro |> Enviro.toMap
            let! mergedMap = mergeMapItems |> List.fold(fun mmR mi -> 
                               procMergeMapItem mi sourceWorlds mmR) destMapR
            let sourceIds = sourceWorlds |> Map.toSeq
                                         |> Seq.map(fun tup -> (fst tup, (snd tup).id))
                                         |> Map.ofSeq
            return {WorldMerges.id = mergedWorldId;
                    sourceNameMap=sourceIds; 
                    mergeMapItems= mergeMapItems;
                    enviro = Enviro.ObjectMap mergedMap}
        }