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





