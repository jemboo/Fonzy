namespace global
open System

module Nope = 
    let q = None

//type World = {id:Guid; parentId:Guid option; cause:Cause; enviroment:Enviro}
//module World = 
//    let create (id:Guid) (parentId:Guid option) (cause:Cause) (enviroment:Enviro) =
//            {id=id; parentId=parentId; cause=cause; enviroment=enviroment}

//    let createFromParent (id:Guid) (parentWorld:World) (cause:Cause) =
//        result {
//            let! newEnv = cause.op parentWorld.enviroment
//            return {id=id; parentId= Some parentWorld.id; cause=cause; enviroment=newEnv}
//        }
     

//type WorldAction = {childId:Guid; parentWorld:World; cause:Cause;}
//module WorldAction = 
//    let create (childId:Guid) (parentWorld:World) (cause:Cause) =
//            {parentWorld=parentWorld; childId=childId; cause=cause;}

//    let createWorld (worldAction:WorldAction) =
//        World.createFromParent worldAction.childId worldAction.parentWorld worldAction.cause


