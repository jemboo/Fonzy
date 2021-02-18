namespace global
open System


type World = {id:Guid; parentId:Guid option; cause:Cause; enviroment:Enviro}

module World = 
    let emptyWorldId = Guid.Parse "00000000-0000-0000-0000-000000000000"
    let empty = 
        {id=emptyWorldId; 
        parentId=None; 
        cause= (CauseSpec.noOpCauseSpec |> Causes.fromCauseSpec |> Result.ExtractOrThrow); 
        enviroment=Enviro.Empty}


    let create (parentId:Guid option) (cause:Cause) (enviroment:Enviro) =
          let wid = match parentId with
                     | Some pid -> GuidUtils.addGuids pid cause.causeSpec.id
                     | None -> cause.causeSpec.id

          {id=wid; parentId=parentId; cause=cause; enviroment=enviroment}


    let createFromParent (parentWorld:World) (cause:Cause) =
        result {
            let! newEnv = cause.op parentWorld.enviroment
            return create (Some parentWorld.id) cause newEnv
        }
     

type WorldAction = {childId:Guid; parentWorld:World; cause:Cause;}

module WorldAction =

    let create (parentWorld:World) (cause:Cause) =
            {
                parentWorld=parentWorld; 
                childId=GuidUtils.addGuids parentWorld.id cause.causeSpec.id; 
                cause=cause;
            }

    let createWorld (worldAction:WorldAction) =
        World.createFromParent 
            worldAction.parentWorld 
            worldAction.cause


