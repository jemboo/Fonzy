namespace global
open System

type World = {id:WorldId; parentId:WorldId; cause:Cause; enviro:Enviro}

module World = 
    let emptyWorldId = WorldId.fromGuid (Guid.Parse "00000000-0000-0000-0000-000000000000")
    let empty = 
        {id=emptyWorldId; 
        parentId = WorldId.fromGuid Guid.Empty; 
        cause= Causes.noOp; 
        enviro=Enviro.Empty}


    let create (parentId:WorldId) (cause:Cause) (enviroment:Enviro) =
          let worldId = GuidUtils.addGuids 
                            (WorldId.value parentId) 
                            (CauseSpecId.value cause.causeSpec.id)
                        |> WorldId.fromGuid;
          {id=worldId; parentId=parentId; cause=cause; enviro=enviroment}


    let createFromParent (parentWorld:World) (cause:Cause) =
        result {
            let! newEnv = cause.op parentWorld.enviro
            return create (parentWorld.id) cause newEnv
        }
     

type WorldAction = {childId:WorldId; parentWorld:World; cause:Cause;}

module WorldAction =

    let create (parentWorld:World) (cause:Cause) =
            {
                parentWorld = parentWorld; 
                childId = GuidUtils.addGuids 
                            (WorldId.value parentWorld.id) 
                            (CauseSpecId.value cause.causeSpec.id)
                          |> WorldId.fromGuid;
                cause=cause;
            }

    let createWorld (worldAction:WorldAction) =
        World.createFromParent 
            worldAction.parentWorld 
            worldAction.cause

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

type WorldMerge = {id:WorldMergeId; sourceNameMap: Map<string,Guid>; 
                   mergeMapItems:MergeMapItem list; enviro:Enviro}
module WorldMerge = 

    let mergeWorlds (mergedWorldId:WorldMergeId) (sourceWorlds:Map<string,World>) 
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
                                         |> Seq.map(fun tup -> (fst tup, WorldId.value (snd tup).id))
                                         |> Map.ofSeq
            return {WorldMerge.id = mergedWorldId;
                    sourceNameMap=sourceIds; 
                    mergeMapItems= mergeMapItems;
                    enviro = Enviro.ObjectMap mergedMap}
        }







