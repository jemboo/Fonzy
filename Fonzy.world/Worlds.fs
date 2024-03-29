﻿namespace global
open System

type world = {id:WorldId; parentId:WorldId; cause:cause; enviro:enviro}

module World = 
    let emptyWorldId = WorldId.fromGuid (Guid.Parse "00000000-0000-0000-0000-000000000000")
    let makeWorldId (parentId:WorldId) 
                    (spec:causeSpec) =
        [parentId:>obj; spec:>obj;]
                |> GuidUtils.guidFromObjs
                |> WorldId.fromGuid;

    let empty = 
        {id=emptyWorldId; 
        parentId = WorldId.fromGuid Guid.Empty; 
        cause= Causes.noOp; 
        enviro=enviro.Empty}


    let create (parentId:WorldId) 
               (cause:cause) 
               (enviroment:enviro) =
          let worldId = makeWorldId parentId cause.causeSpec
          {id=worldId; parentId=parentId; cause=cause; enviro=enviroment}


    let createFromParent (context:obj->Result<obj->unit,string>)
                         (parentWorld:world) 
                         (cause:cause) =
        result {
            let! newEnv = cause.op context parentWorld.enviro
            return create (parentWorld.id) cause newEnv
        }
     

type WorldAction = {childId:WorldId; parentWorld:world; cause:cause;}

module WorldAction =

    let create (parentWorld:world) 
               (cause:cause) =
            {
                parentWorld = parentWorld; 
                childId = GuidUtils.addGuids 
                            (WorldId.value parentWorld.id) 
                            (CauseSpecId.value cause.causeSpec.id)
                          |> WorldId.fromGuid;
                cause=cause;
            }

    let createWorld (context:obj->Result<obj->unit,string>) 
                    (worldAction:WorldAction) =
        World.createFromParent 
            context
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
                   mergeMapItems:MergeMapItem list; enviro:enviro}
module WorldMerge = 

    let mergeWorlds (mergedWorldId:WorldMergeId) (sourceWorlds:Map<string, world>) 
                    (mergeMapItems:MergeMapItem list) (mergedEnviro:enviro) =

        let procMergeMapItem (mergeMapItem:MergeMapItem) 
                             (sourceWorlds:Map<string,world>) 
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
                    enviro = enviro.ObjectMap mergedMap}
        }







