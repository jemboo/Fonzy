namespace global
open System


type job = 
     | GetWorld of world
     | MakeWorld of WorldAction

module Job =

    let getId (job:job) = 
        match job with
        | GetWorld w -> w.id
        | MakeWorld wa -> wa.childId

    let getParentId (job:job) = 
        match job with
        | GetWorld w -> w.parentId
        | MakeWorld wa -> wa.parentWorld.id

    let getCause (job:job) = 
        match job with
        | GetWorld w -> w.cause
        | MakeWorld wa -> wa.cause
