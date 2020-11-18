namespace global
open System

type World = {id:Guid; parentId:Guid option; cause:Cause; enviroment:Enviro}
module World = 
    let create (id:Guid) (parentId:Guid option) (cause:Cause) (enviroment:Enviro) =
            {id=id; parentId=parentId; cause=cause; enviroment=enviroment}

    let createFromParent (id:Guid) (parentWorld:World) (cause:Cause) =
        result {
            let! newEnv = cause.op parentWorld.enviroment
            return {id=id; parentId= Some parentWorld.id; cause=cause; enviroment=newEnv}
        }
     

type WorldAction = {childId:Guid; parentWorld:World; cause:Cause;}
module WorldAction = 
    let create (childId:Guid) (parentWorld:World) (cause:Cause) =
            {parentWorld=parentWorld; childId=childId; cause=cause;}

    let createWorld (worldAction:WorldAction) =
        World.createFromParent worldAction.childId worldAction.parentWorld worldAction.cause


type Job = 
     | GetWorld of World
     | MakeWorld of WorldAction

module Job = 
    let getId (job:Job) = 
        match job with
        | GetWorld w -> w.id
        | MakeWorld wa -> wa.childId

    let getParentId (job:Job) = 
        match job with
        | GetWorld w -> w.parentId
        | MakeWorld wa -> Some wa.parentWorld.id

    let getCause (job:Job) = 
        match job with
        | GetWorld w -> w.cause
        | MakeWorld wa -> wa.cause

