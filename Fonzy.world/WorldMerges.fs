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

type WorldMerge = {id:Guid; parentIds: Map<string,Guid>; 
                   mergeMapItems:MergeMapItem list; enviro:Enviro}
module WorldMerge = 
    let procMergeMapItem (mergeMapItem:MergeMapItem) 
                         (parentWorlds:Map<string,World>) 
                         (mergedEnviro:Enviro) =
        result {
            let! sourceWorld = ResultMap.read mergeMapItem.sourceMapName 
                                              parentWorlds
            let! sourceMap = sourceWorld.enviro |> Enviro.toMap
            let! sourceItem = ResultMap.read mergeMapItem.sourceMapKey
                                             sourceMap
            let! destMap = mergedEnviro |> Enviro.toMap
            let! destMapPlus = ResultMap.add mergeMapItem.outputKey 
                                             sourceItem
                                             destMap
            return destMapPlus
        }

        

    let mergeWorlds (mergedWorldId:Guid) (parentWorlds:Map<string,World>) 
                    (mergeMapItems:MergeMapItem list) (mergedEnviro:Enviro) = 
        None